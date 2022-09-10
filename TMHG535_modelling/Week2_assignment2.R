
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
logistic.par <- c(1,100)
#define time step
logistic.t <- seq(0,20,0.1)
#define N0
logistic.init <- 1

#solve de
logistic.sol <- lsoda(logistic.init,logistic.t,logistic.dyn,logistic.par)

plot(logistic.sol[,1],logistic.sol[,2],type='l',main='Logistic growth', xlab='t',ylab='N')

#manually plot without using library
points(logistic.t,1/(0.01+0.99*exp(-logistic.t)),col='red')


# SIR Epidemic model

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
beta <- 1
gamma <- 0.25
SIR.par <- c(beta,gamma)
SIR.init <- c(99,1,0)
SIR.t <- seq(0,30,by=0.1)
#solve
SIR.sol <- lsoda(SIR.init,SIR.t,SIR.dyn,SIR.par)
#extract reesult
TIME <- SIR.sol[,1]
S <- SIR.sol[,2]
I <- SIR.sol[,3]
R <- SIR.sol[,4]
N <- S + I + R

plot(0, 0, type='n', xlim=c(0,50),ylim=c(0,100), xlab='time', ylab='N')
points(TIME,S,type='l',col='blue',lwd=3)
points(TIME,I,type='l',col='red',lwd=3)
points(TIME,R,type='l',col='green',lwd=3)
legend(locator(1),legend=c("S","I","R"),col=c('blue','red','green'),lty=rep(1,3))


#Excerise

#define parameter
#define variable and parameter
# R0 = 2, gamma = 1/28 ~ 0.03571429, beta = 1/14 ~ 0.07142857
beta <- 0.07142857
gamma <- 0.03571429
SIR.par <- c(beta,gamma)
SIR.init <- c(99,1,0)
SIR.t <- seq(0,250,by=0.1)

#solve
SIR.sol <- lsoda(SIR.init,SIR.t,SIR.dyn,SIR.par)
#extract reesult
TIME <- SIR.sol[,1]
S <- SIR.sol[,2]
I <- SIR.sol[,3]
R <- SIR.sol[,4]
N <- S + I + R

plot(0, 0, type='n', xlim=c(0,250),ylim=c(0,100), xlab='time', ylab='N')
points(TIME,S,type='l',col='blue',lwd=3)
points(TIME,I,type='l',col='red',lwd=3)
points(TIME,R,type='l',col='green',lwd=3)
legend(locator(1),legend=c("S","I","R"),col=c('blue','red','green'),lty=rep(1,3))
