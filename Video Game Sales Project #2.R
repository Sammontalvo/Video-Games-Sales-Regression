##########################
####VIDEO GAME SALES######
###PREDICTIVE ANALYSIS####
##########################

#uplaod the ggplot2 package
library(ggplot2)
library(plyr) 

##TO IMPORT THE TIDY DATA SETS CSV FILES##
TidyCS_Genre<-read.csv('https://raw.githubusercontent.com/Sammontalvo/Video-Game-Sales/master/TidyCS_Genre.csv')
TidyCS_Platform<-read.csv('https://raw.githubusercontent.com/Sammontalvo/Video-Game-Sales/master/TidyCS_Platform.csv')
TidyCS_Publisher<-read.csv('https://raw.githubusercontent.com/Sammontalvo/Video-Game-Sales/master/TidyCS_Publisher.csv')
TidyTS_Year<-read.csv('https://raw.githubusercontent.com/Sammontalvo/Video-Game-Sales/master/TidyTS_Year.csv')

#view the dataframes
View(TidyCS_Genre)
View(TidyCS_Platform)
View(TidyCS_Publisher)
View(TidyTS_Year)

#Deletes the column X 
TidyCS_Genre$X<-NULL 
TidyCS_Platform$X<-NULL
TidyCS_Publisher$X<-NULL
TidyTS_Year$X<-NULL

#exploratory analysis from last project findings
plot_A <- ggplot(TidyCS_Platform, aes(NA_Sales, EU_Sales, colour = factor(Platform))) +
  geom_point()
print(plot_A)

plot_B<-plot_A +theme(legend.position="bottom")
print(plot_B)

plot_C<-plot_B + 
  xlab('Average North America Sales (Mil.)') +  #x-axis label
  ylab('Average Europe Sales (Mil.)') #y-axis label
plot_C

plot_D<-plot_C +
  ggtitle('Comparison of Sales') #adds title
plot_D

plot_E<-plot_C +
  ggtitle('Comparison of Sales') + #adds title
  theme(plot.title = element_text(hjust = .5))
plot_E

plot_Line2<-ggplot(TidyCS_Platform, aes(x = NA_Sales, y = EU_Sales)) + 
  geom_point() +
  geom_smooth(method ='lm') #creates a new plot with line of best fit

plot_Line3<-plot_Line2+ 
  xlab('Average North America Sales (Mil.)') +  #x-axis label
  ylab('Average Europe Sales (Mil.)') #y-axis label
plot_Line3

#TRANSFORMATIONS TO TIDYCS_Publisher
TidyCS_Publisher$NA_Sales2<-TidyCS_Publisher$NA_Sales^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
TidyCS_Publisher$NA_Sales3<-TidyCS_Publisher$NA_Sales^3#CUBIC TRANSFORMATION (3rd ORDER)

#SEPERATING TRAINING AND TESTING#
p<-.7 #fraction of sample to be used for separating training from testing

#number of observations (rows) in the dataframe
obs_count<-dim(TidyCS_Publisher)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)
View(train_ind) #view the training index


#generate training and testing partitions
Training <- TidyCS_Publisher[train_ind, ] #pulls random rows for training
Testing <- TidyCS_Publisher[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#MODELING & PREDICTING TRAINING PARTITIONS#
#PLOTTING THE TRAINING PARITION
plot(Global_Sales ~ NA_Sales, TidyCS_Publisher, xlim=c(0,2), ylim=c(0,3)) #PLOT ENTIRE DATASET
plot(Global_Sales ~ NA_Sales, Training, xlim=c(0,2), ylim=c(0,3), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
points(Training$NA_Sales, Training$Global_Sales, col='blue') #PLOTS THE OUT-OF-SAMPLE TRAINING PARTITION

#BUILDING MODEL 1 FROM THE TRAINING DATA
M1 <- lm(Global_Sales ~ NA_Sales, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values


#COMPUTING IN-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Global_Sales)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_IN #IN-SAMPLE ERROR

#PLOTTING THE MODEL 1 IN 2D 
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(NA_Sales=x_grid))
plot(Training$Global_Sales ~ Training$NA_Sales, col='blue')
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING A QUADRATIC MODEL 2 FROM THE TRAINING DATA
M2 <- lm(Global_Sales ~ NA_Sales + NA_Sales2, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#COMPUTING IN-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Global_Sales)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_IN #IN-SAMPLE ERROR

#PLOTTING MODEL 2 IN 2D
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(NA_Sales=x_grid, NA_Sales2=x_grid^2))
plot(Training$Global_Sales ~ Training$NA_Sales, col='blue')
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING A CUBIC MODEL 3 FROM THE TRAINING DATA
M3 <- lm(Global_Sales ~ NA_Sales + NA_Sales2 + NA_Sales3, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Global_Sales)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_IN #IN-SAMPLE ERROR

#PLOTTING MODEL 3 IN 2D
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(NA_Sales=x_grid, NA_Sales2=x_grid^2, NA_Sales3=x_grid^3))
plot(Training$Global_Sales ~ Training$NA_Sales, col='blue')
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING A MODEL WITH MULTIPLE INDEPENDENT VARIABLES FROM THE TRAINING DATA
M4 <- lm(Global_Sales ~ NA_Sales + EU_Sales, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#COMPUTING IN-SAMPLE AND IN-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$Global_Sales)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_IN #IN-SAMPLE ERROR

#MODELING & PREDICTING TESTING PARTITIONS#
#PLOTING THE TESTING PARTITION
plot(Global_Sales ~ NA_Sales, TidyCS_Publisher, xlim=c(0,2), ylim=c(0,3)) #PLOT ENTIRE DATASET
plot(Global_Sales ~ NA_Sales, Testing, xlim=c(0,2), ylim=c(0,3),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$NA_Sales, Testing$Global_Sales, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

#BUILDING MODEL 5 FROM THE TESTING DATA
M5 <- lm(Global_Sales ~ NA_Sales, Testing)
summary(M5) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TESTING DATA
PRED_1_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data
View(PRED_1_OUT)
View(M5$fitted.values) #these are the same as the fitted values

#COMPUTING OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Global_Sales)^2)/length(PRED_1_OUT)) #computes out-of-sample 
RMSE_1_OUT #OUT-OF-SAMPLE ERROR


#PLOTTING MODEL 5 IN 2D
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M5, list(NA_Sales=x_grid))
plot(Testing$Global_Sales ~ Testing$NA_Sales, col='red')
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING QUADRATIC MODEL 6 FROM THE TESTING DATA
M6 <- lm(Global_Sales ~ NA_Sales + NA_Sales2, Testing)
summary(M6) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_OUT <- predict(M6, Testing) #generate predictions on the (in-sample) training data
View(PRED_2_OUT)
View(M6$fitted.values) #these are the same as the fitted values

#COMPUTING OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Global_Sales)^2)/length(PRED_2_OUT)) #computes out-of-sample 
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING MODEL 6 IN 2D
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M6, list(NA_Sales=x_grid, NA_Sales2=x_grid^2))
plot(Testing$Global_Sales ~ Testing$NA_Sales, col='red')
lines(x_grid, predictions, col='green', lwd=3)


#BUILDING THE CUBIC MODEL FROM THE TESTING DATA
M7 <- lm(Global_Sales ~ NA_Sales + NA_Sales2 + NA_Sales3, Testing)
summary(M7) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TESTING DATA
PRED_3_OUT <- predict(M7, Testing) #generate predictions on the (in-sample) training data
View(PRED_3_OUT)
View(M7$fitted.values) #these are the same as the fitted values

#COMPUTING OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$Global_Sales)^2)/length(PRED_3_OUT)) 
RMSE_3_OUT #OUT-OF-SAMPLE ERROR


#PLOTTING MODEL 7 IN 2D
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M7, list(NA_Sales=x_grid, NA_Sales2=x_grid^2, NA_Sales3=x_grid^3))
plot(Testing$Global_Sales ~ Testing$NA_Sales, col='red')
lines(x_grid, predictions, col='green', lwd=3)

#BUILDING A MODEL WITH MULTIPLE INDEPENDENT VARIABLES FROM THE TESTING DATA
M8 <- lm(Global_Sales ~ NA_Sales + EU_Sales, Testing)
summary(M8) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_OUT <- predict(M8, Testing) #generate predictions on the (in-sample) training data
View(PRED_4_OUT)
View(M8$fitted.values) #these are the same as the fitted values

#COMPUTING IN-SAMPLE AND IN-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Global_Sales)^2)/length(PRED_4_OUT))  #computes in-sample error
RMSE_4_OUT #OUT-OF-SAMPLE ERROR


#PLOTTING REGRESSION AGAINST EACH OTHER#
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$Global_Sales ~ Training$NA_Sales, col='blue')
predictions_1 <- predict(M1, list(NA_Sales=x_grid))
predictions_2 <- predict(M2, list(NA_Sales=x_grid, NA_Sales2=x_grid^2))
predictions_3 <- predict(M3, list(NA_Sales=x_grid, NA_Sales2=x_grid^2, NA_Sales3=x_grid^3))
lines(x_grid, predictions_1, col='darkgreen', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='green', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='orange', lwd=3) #PLOTS M3
points(Testing$Global_Sales ~ Testing$NA_Sales, col='red', pch=3)

#COMPARISON OF THE ROOT MEAN SQUARED ERRROR
c(RMSE_1_IN, RMSE_2_IN, RMSE_3_IN, RMSE_4_IN)
c(RMSE_1_OUT, RMSE_2_OUT, RMSE_3_OUT, RMSE_4_OUT)





