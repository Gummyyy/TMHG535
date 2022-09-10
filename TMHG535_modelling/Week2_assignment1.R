## example
t1 <- seq(0,5,0.1)

plot(t1,20*exp(t1),type='l',xlab='t',ylab='N',main='Exponential growth')

lines(t1,exp(2*t1),col='red')

#Q: What is the difference between these two lines?
#Ans There are two different things between two line which are population at time point 0[N(0)] and growth rate(r). Black line has N0 at 20 and r equal to 1 while Red line has N0 at 1 and r equal to 2  


# Excerise 1
#Growth rates rA = 0.1 and initial numbers NA0 = 100
#Growth rate rB = 0.4 and initial numbers NB0 = 1
# How many days before B over take A?
#define time range
t <- seq(0,30,0.1)
# plot A
plot(t,100*exp(0.1*t),type='l',xlab='t',ylab='N',main='Exponential growth', col='red')

#plot B
lines(t,1*exp(0.4*t),col='blue')

#Answer
# It will take around 16 days when B will over take A

#Q: What are biological interpretation of the two parameters r and K?
#ans r represent growth rate of the population while K represent the maximum capacity of the environment which could carrying population.

# create logistic function that return N
logistic <- function(r,K,N0,t) {K/(1+(K/N0-1)*exp(-r*t))}


#Q: What are biological interpretation of the two parameters r and K?
#ans r represent growth rate of the population while K represent the maximum capacity of the environment which could carrying population.

#try logistic function
logistic(1,100,10,0)

logistic(1,100,10,1)

logistic(0,100,10,5)

logistic(1,10,100,10)


#Q: Can you explain the output of the model? What is this number?
#ans the out put is the amount of population when growth rate is r, maximum capacity is K, and starting population N0 at given time t


#def new time step
t2 <- seq(0,10,0.1)
plot(t2,logistic(1,100,1,t2),type='l',ylim=c(0,120),xlab='t',ylab='N')

# plot for variety of N0
for(n in c(2,5,10,50,100,120)) {lines(t2,logistic(1,100,n,t2))}


# create plot with variety of color
N0.v <- c(1,2,5,10,20,50)

col.v <- c('brown','red','orange','green','blue','purple')

windows()

plot(0,type='n',xlim=c(0,10),ylim=c(0,120),xlab='t',ylab='N',main='Varying N0')

for (i in 1:6) {lines(t2,logistic(1,100,N0.v[i],t2),col=col.v[i])}


# add legend
legend(6,60,paste('N0 = ',N0.v),col=col.v,lty=1)


#Q: What happened when initial N is greater than K and what happened when initial N is smaller than K? Does it make sense in reality with the population and their survival?

#ans When N0 is greater than K, after that time stage, the population tend to reduce and remain stable at K as the maximum capacity, but for the case that N0 smaller than K, the population tend to rise and remain stable at K.
# Both of the situation are making sense when considering the population migrating to the new environment where the supply for sustainable population is insufficient(N0 > K) and sufficient(N0 < K). 


#Exercise: Take similar approach, do a plot of multiple lines where r varies from 0.05 to 0.4  when K = 500 and N0 = 1 (Hint: Use r.v=seq(0.05,0.4,0.05) and t3=0:140)
r.v <- seq(0.05,0.4,0.05)
col.v <- c('brown','red','orange','green','blue','purple','yellow','brown')
t3<- seq(0,140,0.1)
plot(0,type='n',xlim=c(0,140),ylim=c(0,600),xlab='t',ylab='n',main='Varying r')
for (i in 1:8) {lines(t3,logistic(r.v[i],500,1,t3),col=col.v[i])}

#Do a plot of multiple lines where K varies from 10 to 100 when r = 0.1 and N0 = 1 (Hint: Use K.v=seq(10,100,10))
K.v <- seq(10,100,10)

col.v <- c('brown','red','orange','green','blue','purple','yellow','brown','pink','cyan')
plot(0,type='n',xlim=c(0,140),ylim=c(0,120),xlab='t',ylab='r',main='Varying K')
for (i in 1:10) {lines(t3,logistic(0.1,K.v[i],1,t3),col=col.v[i])}

#Q: What happens when the carrying capacity is changing?
#ans When carrying capacity(K) is higher, it will take longer time for the amount of population to reach the maximum capacity. Likewise, when K is lower, it will take shorter time until the population reach the maximum capacity


