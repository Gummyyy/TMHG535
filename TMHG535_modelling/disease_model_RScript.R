
setwd('C:/R_practices')#could be D drive in some computer, please check where you created this folder last time

# Note: before you can use odesolve for the first time on a computer, you have to download it:
# install.packages("deSolve")

library(deSolve)

#SI Deterministic Model

  SI.dyn <- function(t,var,par) {
    S <- var[1]
    I <- var[2]
    
    beta <- par[1]
    N<-S+I
    
    dS<--beta*S*I/N
    dI<-beta*S*I/N
    return(list(c(dS,dI)))
  }
  
beta <- 1
SI.par <- c(beta)
SI.init <- c(99,1)
SI.t <- seq(0,30,by=0.1)

det.sol<-lsoda(SI.init,SI.t,SI.dyn,SI.par)
  


det.t<-det.sol[,1]
det.S<-det.sol[,2]
det.I<-det.sol[,3]

plot(0, 0, type='n', xlim=c(0,30),ylim=c(0,100), xlab='time', ylab='infecteds')
points(det.t,det.S,type='l',col='blue',lwd=3)
points(det.t,det.I,type='l',col='red',lwd=3)
legend(locator(1),legend=c("S","I"),col=c('blue','red'),lty=rep(1,2))

#-----------------------------

#SIS Deterministic Model

SIS.dyn <- function(t,var,par) {
  S <- var[1]
  I <- var[2]
  
  beta <- par[1]
  gamma <- par[2]
  N<-S+I
  
  dS<--beta*S*I/N+gamma*I
  dI<-beta*S*I/N-gamma*I
  return(list(c(dS,dI)))
}

beta <- 1
gamma <- 0.25
SIS.par <- c(beta,gamma)
SIS.init <- c(99,1)
SIS.t <- seq(0,30,by=0.1)

det.sol<-lsoda(SIS.init,SIS.t,SIS.dyn,SIS.par)


det.t<-det.sol[,1]
det.S<-det.sol[,2]
det.I<-det.sol[,3]

plot(0, 0, type='n', xlim=c(0,30),ylim=c(0,100), xlab='time', ylab='infecteds')
points(det.t,det.S,type='l',col='blue',lwd=3)
points(det.t,det.I,type='l',col='red',lwd=3)
legend(locator(1),legend=c("S","I"),col=c('blue','red'),lty=rep(1,2))

#-------------


#SIR Deterministic Model

SIR.dyn <- function(t,var,par) {
  S <- var[1]
  I <- var[2]
  R <- var[3]
  
  beta <- par[1]
  gamma <- par[2]
  N<-S+I+R
  
  dS<--beta*S*I/N
  dI<-beta*S*I/N-gamma*I
  dR<-gamma*I
  return(list(c(dS,dI,dR)))
}

beta <- 1
gamma <- 0.5
SIR.par <- c(beta,gamma)
SIR.init <- c(99,1,0)
SIR.t <- seq(0,50,by=0.1)

det.sol<-lsoda(SIR.init,SIR.t,SIR.dyn,SIR.par)


det.t<-det.sol[,1]
det.S<-det.sol[,2]
det.I<-det.sol[,3]
det.R<-det.sol[,4]

plot(0, 0, type='n', xlim=c(0,50),ylim=c(0,100), xlab='time', ylab='infecteds')
points(det.t,det.S,type='l',col='blue',lwd=3)
points(det.t,det.I,type='l',col='red',lwd=3)
points(det.t,det.R,type='l',col='green',lwd=3)
legend(locator(1),legend=c("S","I","R"),col=c('blue','red','green'),lty=rep(1,3))
