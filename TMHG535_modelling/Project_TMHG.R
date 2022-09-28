logistic <- function(r,K,N0,t)
{
  K/(1+(K/N0-1)*exp(-r*t))
}
t1 <- seq(0,100,0.1)
V.r <- seq(0.01,0.1,0.01)

plot(0,type='n',xlim=c(0,100),ylim=c(0,100),xlab='t',ylab='N')
points(82,86,col="red")
for (i in 1:10){
  lines(t1,logistic(V.r[i],K=3400000,N0=1,t1),col=rainbow(20)[i])
}
legend(0,100,paste('r =',V.r),col=rainbow(20),lty=1,cex=0.7)




library(deSolve)

#define logistic growth model
logistic.dyn <- function(t,N,par){
  # Rename the parameters
  r <- par[1]
  K <- par[2]
  # Calculate the derivative
  dN <- r*N*(1-N/K)
  # Last instruction: return a list
  return(list(dN))
}

#define parameter r and K
logistic.par <- c(0.055,3400000)
#define time step
logistic.t <- seq(0,365)
#define N0
logistic.init <- 1

#solve de
logistic.sol <- lsoda(logistic.init,logistic.t,logistic.dyn,logistic.par)

plot(logistic.sol[,1],logistic.sol[,2],type='l',main='Logistic growth', xlab='t',ylab='N')

SIR.dyn <- function(t,var,par) { 
  # Rename the variables and parameters
  
  S <- var[1]
  I <- var[2]
  R <- var[3]
  N <- S+I+R
  beta <- par[1]
  gamma <- par[2]
  # Derivatives
  dS <- -beta*S*I/N
  dI <- beta*S*I/N - gamma*I
  dR <- gamma*I
  # Return the 3 values
  list(c(dS,dI,dR))
}

#define variable and parameter
#R0 = beta/gamma
beta <- 0.17 #infection rate
gamma <- 0.1176471 # recovery rate
SIR.par <- c(beta,gamma)
SIR.init <- c(3400000,1,0)
SIR.t <- seq(0,365,by=1)
#solve
SIR.sol <- lsoda(SIR.init,SIR.t,SIR.dyn,SIR.par)
#extract reesult
TIME <- SIR.sol[,1]
S <- SIR.sol[,2]
I <- SIR.sol[,3]
R <- SIR.sol[,4]
N <- S + I + R

plot(0, 0, type='n', xlim=c(0,365),ylim=c(0,30000), xlab='time', ylab='N')
points(TIME,S,type='l',col='blue',lwd=3)
points(TIME,I,type='l',col='red',lwd=3)
points(TIME,R,type='l',col='green',lwd=3)
points(82,86,col="red")
legend(locator(1),legend=c("S","I","R"),col=c('blue','red','green'),lty=rep(1,3))

