#set working directory
setwd("~/Desktop/TMHG535/TMHG535_modelling")


library(deSolve)
data <- read.csv('Tomato_flu.csv')

# Set duration period of simulation
week_start <- 1
week_stop <- 200
times <- seq(week_start, week_stop, by = (1/7))

# MODEL INITIAL CONDITIONS
initP<-3400000 # population size

initE<-0 # Exposed
initI<-1 # Infectious
initR<-0 # Immune
initS<-initP-initE-initI-initR # Susceptible (non-immune)

state <- c(S = initS, E=initE, I = initI,R = initR)

# set up a function to solve the equations
TomatoFlu<-function(t, state, parameters) 
{
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         P <- (S+E+I+R)
         beta <- R0*gamma
         
         #isolation factor
         iso_perc <- (t>=(week_start+week_interv))*iso_perc_i
         isolate <- (1-(iso_com*iso_perc))
         
         lam <- isolate*beta*I/P
         
         # rate of change
         dS <- -lam*S
         dE <- lam*S-sigma*E
         dI <- sigma*E-gamma*I
         dR <- gamma*I
         
         # return the rate of change
         list(c(dS, dE, dI, dR))
       }
  ) 
  
}



RunOde <- function(parms){
  parameters <- c(
                sigma=1/4.5,
                report=1, 
                amp=0,
                phi=0
, parms 

)

out <- ode(y = state, times = times, func = TomatoFlu, parms = parameters)

return(out)
}

# root mean square deviation
RMSD <- function(inc){
  inc.dat <- inc[inc$time%in%data$week,]
  rmsd <- sqrt(mean((inc.dat$inc-data$Cases)^2))
  return(rmsd)
}

#log likelihood (negative) 
LL <- function(inc, cumulative=F){
  obs <- data$Cases
  if(cumulative){
    cobs <- cumsum(data$cases)
    ll <- dpois(cobs, inc, log = TRUE)  
  }else{
    ll <- dpois(obs,inc, log = TRUE)  
  }
  
  return(-sum(ll))
}

# incidence function
Incidence <- function(odeoutput){
  out <- odeoutput
  ## define the equired parameters again 
  parameters <- c( 
                  sigma=1/4.5,
                  report=1, 
                  amp=0,
                  phi=0)
  
  # some more model outputs
  # total population
  #pop<-out[,"S"]+out[,"E"]+out[,"I"]+out[,"R"]
  
  # weekly incidence
  return(data.frame(time=out[,"time"],
                    inc=parameters["report"]*parameters["sigma"]*out[,"E"]
                    )
        )
}


# for doing the optimization ; fn=1 rmsd, fn=2 loglikelihood
fn4optim <- function(parms, fn = 1){
  #parms <- c(R0,gamma,week_interv,iso_perc_i,iso_com)
  
  odeout <- RunOde(c(R0=parms[1],gamma=parms[2],sigma=parms[3],week_interv=2,iso_perc_i=0,iso_com=0))
  inc <- Incidence(odeout)
  if(fn ==1){
    rmsd <- RMSD(inc)
    return(rmsd)
  }
  if(fn==2){
    ll <- LL(inc = inc$inc)
    return(ll)
  }
}



shinyServer(

function(input, output, session) {

#input parameters
  parms <- reactive(c(
    R0= input$R0,
    gamma= input$gamma,
    week_interv= input$week_interv,
    iso_perc_i= input$iso_perc_i,
    iso_com=input$iso_com
  ))

  #update the ode output
  outode <- reactive(RunOde(parms()))

    
  # update the incidence
  outinc <- reactive(Incidence(outode()))
  
  #plotting function
  plotX <- function(){

    inc <- outinc()
    # plot incidence
    plot(inc$time,inc$inc,type='l',lwd=3,main = "Predicted Tomato Flu Outbreak",xlab = "Time in weeks",ylab="New reported cases per week",ylim=c(0,max(data[,"Cases"],inc$inc)))
    # overlay the observed data points
    points(data[,"week"],data[,"Cases"],pch=19,col='red')

  }

  #output for UI
  output$graphs <- renderPlot({
    plotX()
  })

  # do the optimization
  observeEvent(input$optimize, {
    input$optimize
    
    res <- optim(c(6.6,0.25),fn4optim,
                 lower = c(2,0.1),
                 upper = c(7,0.5),
                 method='L-BFGS-B')
    
    R0 <- res$par[1]
    gamma <- res$par[2]
   week_interv <- 13#res$par[3]
   iso_perc_i <- 0#res$par[4]
   iso_com <- 0#res$par[5]
    print(res$par)
    beepr::beep(1)
    ### update the sliders using the values from optim
    updateSliderInput(session = session,inputId = "R0", label = "R0", min = 1, max = 10,step = 0.01, value = R0)
    updateSliderInput(session = session,inputId = "gamma", label = "gamma: rate of recovery = 1/(duration of infection)",
                      min = 1/10,max = 1/7,step = 0.001,value = gamma)
    updateSliderInput(session = session,inputId = "week_interv", label = "week_interv: weeks after first case when the intervention starts",
                      min = 13,max = 53,step = 0.01,value = week_interv)
    updateSliderInput(session = session,inputId = "iso_perc_i", label = "iso_perc_i: proportion of infected population which isolate themself during infected period",
                      min = 0,max = 1,step = 0.01,value = iso_perc_i)
    updateSliderInput(session = session,inputId = "iso_com", label = "iso_com: average completeness of home isolation",
                      min = 0,max = 1,step = 0.01,value = iso_com)
    
    
  })
  
})






