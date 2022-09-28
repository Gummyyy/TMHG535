
setwd("~/Desktop/TMHG535/TMHG535_modelling")

# install.packages("deSolve")

library(deSolve)





#SEIR Model

SEIR.dyn <- function(t,var,par) {
  S <- var[1]
  E <- var[2]
  I <- var[3]
  R <- var[4]
  
  beta <- par[1]
  gamma <- par[2]
  sigma <- par[3]
  N<-S+E+I+R
  
  lam <- beta*I/N
  dS <- -lam*S
  dE <- lam*S-sigma*E
  dI <- sigma*E-gamma*I
  dR <- gamma*I
  return(list(c(dS,dE,dI,dR)))
}

beta <- 5.99* 1/7
gamma <- 1/7
sigma <- 1/4.5
SEIR.par <- c(beta,gamma,sigma)
SEIR.init <- c(3400000,0,1,0)
SEIR.t <- seq(0,130,by=1/7)

det.sol<-lsoda(SEIR.init,SEIR.t,SEIR.dyn,SEIR.par)
                                                                                                                                                                                                                                                                                                                                                                        

det.t<-det.sol[,1]
det.S<-det.sol[,2]
det.E<-det.sol[,3]
det.I<-det.sol[,4]
det.R<-det.sol[,5]

plot(0, 0, type='n', xlim=c(0,106),ylim=c(0,3500000), xlab='time(in Weeks)', ylab='Population')
points(det.t,det.S,type='l',col='blue',lwd=3)
points(det.t,det.E,type='l',col='black',lwd=3)
points(det.t,det.I,type='l',col='red',lwd=3)
points(det.t,det.R,type='l',col='green',lwd=3)
legend(locator(1),legend=c("S","E","I","R"),col=c('blue','black','red','green'),lty=rep(1,3))

