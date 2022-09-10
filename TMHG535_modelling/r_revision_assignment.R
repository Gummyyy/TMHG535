
#read data set
data <- read.csv("data_week0.csv")

#plot X and Y coordinates
plot(x= data$X, y= data$Y)

#perform linear regression model
linear1 <- lm(Y~X,data)

#plot linear model

fitted_model <-fitted(linear1)

lines(data$X,fitted_model)

# add point and refit linear model for 5 iteration

new.point <- NULL  # define new point array in "new" object
for (i in 1:5){  # iterate for 5 time
  new.point[i] <- runif(1,1,3000) #randomly pick number (between 1-3,000) and add to new.point array

  points(101,new.point[i],col=i,pch=3) #add point to existing plot where x = 101, y = previously picked number and assign different color to point

  new.data <- rbind(data,c(101,new.point[i])) # joining the new point(x=101, y=previously picked number ) to original data

  new.linear <- lm(Y~X,new.data) #create linear model from jointed data

  lines(new.data$X,new.linear$fitted.values,col=i) # plot new line from new linear model and assign different color to line

}


