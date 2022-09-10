setwd('~/Desktop/TMHG535/TMHG535_modelling')#could be D drive in some computer, please check where you created this folder last time
getwd()
t1 <- seq(0,5,0.1)
plot(t1,20*exp(t1),type='l', xlab="time",ylab="population")
lines(t1,exp(2*t1),col='red')

t2 <-seq(0,20,0.1) 

p1 <- 100*exp(0.1*t2)
p2 <- 1*exp(0.4*t2)
p2

plot(t2,p1,type='l',ylim=c(0,500), xlab="time",ylab="population")
lines(t2,p2,col='red')

logistic <- function(r,K,N0,t)
{
  K/(1+(K/N0-1)*exp(-r*t))
}

logistic(r=1,K=100,N0=10,t=0)
logistic(r=1,K=100,N0=10,t=1)
logistic(r=0,K=100,N0=10,t=16)
logistic(r=1,K=10,N0=100,t=10)

t2 <- seq(0,10,0.1)

plot(t2,logistic(1,100,1,t2),type='l',ylim=c(0,120), xlab="time",ylab="population")

for(n in c(2,5,10,50,100,120))
{
  lines(t2,logistic(1,100,n,t2))
}

col.v <- c('brown','red','orange','green','blue','purple')
#vary N0 (initial number)
N0.v <-c(2,5,10,50,100,120)
plot(0,type='n',xlim=c(0,10),ylim=c(0,100),xlab='t',ylab='N')
for (i in 1:6)
{
  lines(t2,logistic(1,100,N0.v[i],t2),col=col.v[i])
}
legend(6,80,paste('N0=',N0.v),col=col.v,lty=2)

#vary r (growth rate)

r.v = seq(0.05,0.4,0.05)
t3 <- seq(0:140)

plot(0,type='n',xlim=c(0,200),ylim=c(0,500),xlab='t',ylab='N')
for (i in 1:8)
{
  lines(t3,logistic(r.v[i],K=500,N0=1,t3),col=rainbow(8)[i])
}
legend(150,370,paste('r=',r.v),col=rainbow(8),lty=1,cex=0.7)

#vary K (carrying capacity)

K.v = seq(10,100,10) 
t3 <- seq(0:140)

plot(0,type='n',xlim=c(0,200),ylim=c(0,100),xlab='t',ylab='N')
for (i in 1:10)
{
  lines(t3,logistic(r=0.1,K.v[i],N0=1,t3),col=rainbow(10)[i])
}
legend(150,100,paste('K =',K.v),col=rainbow(10),lty=1,cex=0.7)



