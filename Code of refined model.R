# Forecast of California House Values Based on the Random Forest Algorithm 
# by Hongyi Wang
# Revised model

#1. Access the data
housing <- read.table("housing.csv",sep=",", header=TRUE)

#2. Data Visualization 

#3. Data Cleaning 

clean_housing <- data.frame(housing)
inx <- is.na(clean_housing$total_bedrooms)
clean_housing$total_bedrooms[inx] <- mean(clean_housing$total_bedrooms,na.rm = T)

# we transform the levels of ocean_proximity into numerical values instead of a set of binary variables
clean_housing$ocean_proximity <- as.numeric(as.factor(clean_housing$ocean_proximity))
summary(clean_housing)
# households     median_income     median_house_value ocean_proximity
# Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     Min.   :1.000  
# 1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     1st Qu.:1.000  
# Median : 409.0   Median : 3.5348   Median :179700     Median :2.000  
# Mean   : 499.5   Mean   : 3.8707   Mean   :206856     Mean   :2.166  
# 3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     3rd Qu.:2.000  
# Max.   :6082.0   Max.   :15.0001   Max.   :500001     Max.   :5.000  

clean_housing$total_rooms = clean_housing$total_rooms/ clean_housing$population
clean_housing$total_bedrooms = clean_housing$total_bedrooms/ clean_housing$population
names(clean_housing)[4:5] <- c("mean_rooms", "mean_bedrooms")

cleaned_housing <- clean_housing
cleaned_housing[,1:8] <- scale(clean_housing[, 1:8])
summary(cleaned_housing)

# we take the logarithm of the medain_house_value 
cleaned_housing$median_house_value <- log(housing$median_house_value)
summary(cleaned_housing)
#   households      median_income     median_house_value ocean_proximity
# Min.   :-1.3040   Min.   :-1.7743   Min.   : 9.616     Min.   :1.000  
# 1st Qu.:-0.5742   1st Qu.:-0.6881   1st Qu.:11.692     1st Qu.:1.000  
# Median :-0.2368   Median :-0.1768   Median :12.099     Median :2.000  
# Mean   : 0.0000   Mean   : 0.0000   Mean   :12.085     Mean   :2.166  
# 3rd Qu.: 0.2758   3rd Qu.: 0.4593   3rd Qu.:12.486     3rd Qu.:2.000  
# Max.   :14.6012   Max.   : 5.8581   Max.   :13.122     Max.   :5.000  

#4. Create training and test Sets 
ntrain2<-round(nrow(cleaned_housing)*0.8)
ntrain2 

set.seed(666)
tindex<-sample(nrow(cleaned_housing),ntrain2)
#Create a train2ing set named train2 consisting of 80% of the rows of the housing data frame. 
train2<-cleaned_housing[tindex,]
#Create a test2 set named test2 consisting of 20% of the rows of the housing data frame.  
test2<-cleaned_housing[-tindex,]

#5. Supervised Machine Learning - Classification 
# we use the randomForest() algorithm to predict the median house value.  
install.packages("randomForest")
library(randomForest)

rf2 = randomForest(median_house_value~.,data = train2, ntree = 500, importance = TRUE) 
rf2$importance
# According to the increase in MSE, we can see that there is a increase in the importance of all predictors 
#and the ocean_proximity is a very importance variable in the refined model, suggesting a better fitting of this model. 
varImpPlot(rf2)

#6. Evaluating Model Performance

# Compute the RMSE
oob_prediction2 = predict(rf2)
tmpt2 <- (oob_prediction2 - train2$median_house_value)^2
train_mse2 = mean(tmpt2) 
oob_rmse2 = sqrt(train_mse2) 
oob_rmse2 
# 0.2226408

# Next, we can see how well the model predicts using the test2 data. 
test2_x <- subset(test2, select = -median_house_value)
test2_y <- subset(test2, select = median_house_value)
y_pred2 = predict(rf2 , test2_x) 
# Now compute the test2 set RMSE 
tmpt2 <- (y_pred2 - test2_y)^2
test2_mse = mean(tmpt2$median_house_value) 
test2_rmse = sqrt(test2_mse) 
test2_rmse
# 0.2199046
