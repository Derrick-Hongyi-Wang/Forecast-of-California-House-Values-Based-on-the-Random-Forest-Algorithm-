# Forecast of California House Values Based on the Random Forest Algorithm 
# by Hongyi Wang

#1. Access the data

getwd()
#Read the data set into R using a data frame named housing.  
housing <- read.table("C:/Users/whydf/Desktop/大学/大三下/UCLA/Data Science/project/housing.csv",sep=",", header=TRUE)
head(housing)
#   longitude latitude housing_median_age total_rooms total_bedrooms population households median_income median_house_value
# 1   -122.23    37.88                 41         880            129        322        126        8.3252             452600
# 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014             358500
# 3   -122.24    37.85                 52        1467            190        496        177        7.2574             352100
# 4   -122.25    37.85                 52        1274            235        558        219        5.6431             341300
# 5   -122.25    37.85                 52        1627            280        565        259        3.8462             342200
# 6   -122.25    37.85                 52         919            213        413        193        4.0368             269700
#   ocean_proximity
# 1        NEAR BAY
# 2        NEAR BAY
# 3        NEAR BAY
# 4        NEAR BAY
# 5        NEAR BAY
# 6        NEAR BAY
summary(housing)
# longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population      households    
# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3   Min.   :   1.0  
# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0  
# Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166   Median : 409.0  
# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425   Mean   : 499.5  
# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0  
# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682   Max.   :6082.0  
#                                                                     NA's   :207                                      
# median_income     median_house_value   ocean_proximity
# Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :15.0001   Max.   :500001 

#2. Data Visualization 

# Histogram shows the frequency of data
hist(housing$longitude,xlab="longitude",col="blue",breaks=10,main="") # 

hist(housing$latitude,xlab="latitude",col="blue",breaks=10,main="") # 

hist(housing$housing_median_age,xlab="housing_median_age",col="blue",breaks=10,main="") # 

hist(housing$total_rooms,xlab="total_rooms",col="blue",breaks=10,main="") #  

hist(housing$total_bedrooms,xlab="total_bedrooms",col="blue",breaks=10,main="") # 

hist(housing$population,xlab="population",col="blue",breaks=10,main="") # 

hist(housing$households,xlab="households",col="blue",breaks=10,main="") # 

hist(housing$median_income,xlab="median_income",col="blue",breaks=10,main="") # 

hist(housing$median_house_value,xlab="median_house_value",col="blue",breaks=10,main="") # 

#Comment: 
# According to the histograms, the distribution of each variables are different from each other. 
# The variables, total_rooms, total_bedrooms, population and households, have large range and are positive-skewed, 
# and the housing_median_age, median_income and median_house_value are an approximately normal distribution. 
# Many blocks concentrate in the geographic position around 122 degrees west longitude, 34 degrees north latitude and 118 west degrees longitude, 37.5 degrees north latitude. 
# Moreover, the scales of the data are various. The unit of median_income is thousands dollar, but the unit of median_house_value is one dollar. 


#3. Data Cleaning 

# We see from the summary() results above that there are many NA values in the total_bedrooms variable
# use median of total_bedroom to fill in the missing data of this variable 

housing.has.na <- apply(housing,1,function(x){any(is.na(x))})
sum(housing.has.na) # have 207 total missing values

clean_housing <- data.frame(housing)
inx <- is.na(clean_housing$total_bedrooms)
clean_housing$total_bedrooms[inx] <- mean(clean_housing$total_bedrooms,na.rm = T)
summary(clean_housing) # no missing values this time

# Next, we split the ocean_proximity variable into a number of binary categorical variables and remove ocean_proximity variable. 
NEAR_BAY<- c()
NEAR_BAY[housing$ocean_proximity=="NEAR BAY"]<- 1
NEAR_BAY[housing$ocean_proximity!="NEAR BAY"]<- 0
NEAR_OCEAN<- c()
NEAR_OCEAN[housing$ocean_proximity=="NEAR OCEAN"]<- 1
NEAR_OCEAN[housing$ocean_proximity!="NEAR OCEAN"]<- 0
oneH_OCEAN<- c()
oneH_OCEAN[housing$ocean_proximity=="<1H OCEAN"]<- 1
oneH_OCEAN[housing$ocean_proximity!="<1H OCEAN"]<- 0
ISLAND<- c()
ISLAND[housing$ocean_proximity=="ISLAND"]<- 1
ISLAND[housing$ocean_proximity!="ISLAND"]<- 0
INLAND<- c()
INLAND[housing$ocean_proximity=="INLAND"]<- 1
INLAND[housing$ocean_proximity!="INLAND"]<- 0

clean_housing<- data.frame(cbind(clean_housing, NEAR_BAY,NEAR_OCEAN, oneH_OCEAN, ISLAND, INLAND))
clean_housing<- subset(clean_housing, select=-ocean_proximity) # remove the ocean_proximity 
summary(clean_housing) 

# Then we replace the total_rooms and total_bedrooms with new variables: "mean_number_bedrooms" and "mean_number_rooms" to let our model make more sense
clean_housing$total_rooms = clean_housing$total_rooms/ clean_housing$population
clean_housing$total_bedrooms = clean_housing$total_bedrooms/ clean_housing$population
names(clean_housing)[4:5] <- c("mean_rooms", "mean_bedrooms")

# Perform feature scaling and scale each numeric column, except for median_house_value (as this is what we will work to predict). 
cleaned_housing <- clean_housing
cleaned_housing[,1:8] <- scale(clean_housing[, 1:8])
summary(cleaned_housing)
# longitude          latitude       housing_median_age   mean_rooms      
# Min.   :-2.3859   Min.   :-1.4475   Min.   :-2.19613   Min.   :-1.72285  
# 1st Qu.:-1.1132   1st Qu.:-0.7968   1st Qu.:-0.84537   1st Qu.:-0.39667  
# Median : 0.5389   Median :-0.6423   Median : 0.02865   Median :-0.03406  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000  
# 3rd Qu.: 0.7785   3rd Qu.: 0.9729   3rd Qu.: 0.66429   3rd Qu.: 0.27846  
# Max.   : 2.6252   Max.   : 2.9580   Max.   : 1.85614   Max.   :46.46103  
# mean_bedrooms       population        households      median_income    
# Min.   :-1.5232   Min.   :-1.2561   Min.   :-1.3040   Min.   :-1.7743  
# 1st Qu.:-0.3392   1st Qu.:-0.5638   1st Qu.:-0.5742   1st Qu.:-0.6881  
# Median :-0.1254   Median :-0.2291   Median :-0.2368   Median :-0.1768  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
# 3rd Qu.: 0.1444   3rd Qu.: 0.2645   3rd Qu.: 0.2758   3rd Qu.: 0.4593  
# Max.   :53.1361   Max.   :30.2496   Max.   :14.6012   Max.   : 5.8581  
# median_house_value    NEAR_BAY        NEAR_OCEAN       oneH_OCEAN         ISLAND         
# Min.   : 14999     Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000000  
# 1st Qu.:119600     1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000000  
# Median :179700     Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000000  
# Mean   :206856     Mean   :0.1109   Mean   :0.1288   Mean   :0.4426   Mean   :0.0002422  
# 3rd Qu.:264725     3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000000  
# Max.   :500001     Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000000  
# INLAND      
# Min.   :0.0000  
# 1st Qu.:0.0000  
# Median :0.0000  
# Mean   :0.3174  
# 3rd Qu.:1.0000  
# Max.   :1.0000  


#4. Create Training and Test Sets 
# Create a random sample index for the cleaned_housing data frame. 
ntrain<-round(nrow(cleaned_housing)*0.8)
ntrain 
#16512
set.seed(666)
tindex<-sample(nrow(cleaned_housing),ntrain)
#Create a training set named train consisting of 80% of the rows of the housing data frame. 
train<-cleaned_housing[tindex,]
#Create a test set named test consisting of 20% of the rows of the housing data frame.  
test<-cleaned_housing[-tindex,]

#5. Supervised Machine Learning - Classification 
# we use the randomForest() algorithm to predict the median house value.  
install.packages("randomForest")
library(randomForest)

rf1 = randomForest(median_house_value~.,data = train, ntree = 500, importance = TRUE) 

# Mean Squared Error (MSE) measures the average distance from prediction of the model to the actual value. 
# The lower the MSE, the more accurate the model is. 
# Display rf$importance to see %InMSE which is defined as the percentage of the increase in MSE of predictions 
# when the given variable is shuffled, thereby acting as a metric of that given variable’s importance in the performance of the model. 
# So a higher number indicates a more important predictor. 
rf1$importance 

# we can also use varImpPlot() to indicate the importance of perdictor.
# varImpPlot() provides a dot chart ofvVariable importance as measure by Random Forest. 
# We see that the median income and housing median age are most important. 
varImpPlot(rf1)

#6. Evaluating Model Performance

# Compute the RMSE
oob_prediction = predict(rf1)
tmpt <- (oob_prediction - train$median_house_value)^2
train_mse = mean(tmpt) 
oob_rmse = sqrt(train_mse) 
oob_rmse 
# 47733.42

# Next, we can see how well the model predicts using the test data. 
test_x <- subset(test, select = -median_house_value)
test_y <- subset(test, select = median_house_value)
y_pred = predict(rf1 , test_x) 
# Now compute the test set RMSE 
tmpt <- (y_pred - test_y)^2
test_mse = mean(tmpt$median_house_value) 
test_rmse = sqrt(test_mse) 
test_rmse
# 45754.81
