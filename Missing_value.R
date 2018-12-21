library(rpart)

# we discussed some techniques to deal with missing data. 
# We will now look at an example where we shall test all the techniques discussed 
# earlier to infer or deal with such missing observations.

# With the information on Visits,Transactions, Operating System, and Gender suppose 
# we need to build a model to predict Revenue. The summary of the information is given below:

setwd("/home/fractaluser/Downloads/Citrix/R/Missing_Value")
data.train <- read.csv("data.train.csv")

summary(data.train)


# We have a total of 7200 missing data points (Transactions: 1800, Gender: 5400) out of 22800 observations. 
# Almost 8% and 24% data points are missing for 'Transactions' and 'Gender' respectively.

### Revenue Prediction

# We will be using a linear regression model to predict 'Revenue'.

dat1 <- read.csv("dat1.csv")
g <- ggplot(dat1, aes(x = x, y = y)) + geom_point(color = "blue")
g + geom_smooth(method = 'lm', se = F, color = "black", formula = y ~ x)


### Missing Value Treatment

# Let's now deal with the missing data using techniques mentioned below and then predict 'Revenue'.

#### 1. Deletion ####

# Delete or ignore the observations that are missing and build the predictive model on the remaining data.
# In the above example, we shall ignore the missing observations totalling 7200 data points for the 2 variables
# i.e. 'Transactions' and 'Gender'.

data.train.del <- data.train[!(is.na(data.train$Gender) | is.na(data.train$Transactions)),]

del <- lm(Revenue ~ Transactions + OS + Gender , data = data.train.del)
summary(del)

#### 2. Impute by Average ####

# We shall  impute the missing data points for 'Transactions' variable by looking at the group means
# of 'Transactions'  by 'OS'

# Since 'Gender' is a categorical variable, we shall use Mode to impute the missing variables. 
# In the given dataset, the Mode for the variable 'Gender' is 'Male' since it's frequency is the highest.
# All the missing data points for 'Gender' will be labeled as 'Male'.

data.train.avg <- data.train
mean.android <- mean(data.train.avg$Transactions[data.train.avg$OS == "Android"], na.rm = T)
mean.ios <- mean(data.train.avg$Transactions[data.train.avg$OS == "iOS"], na.rm = T)

data.train.avg[data.train.avg$OS == "Android" & is.na(data.train.avg$Transactions),]$Transactions <- mean.android
data.train.avg[data.train.avg$OS == "iOS" & is.na(data.train.avg$Transactions),]$Transactions <- mean.ios
data.train.avg$Gender[is.na(data.train.avg$Gender)] <- "Male"

avg <- lm(Revenue ~ Transactions + OS + Gender , data = data.train.avg)
summary(avg)

#### 3. Impute by Predictive Model ####

# Impute 'Gender' by Decision Tree
  
# There are several predictive techniques; statistical and machine learning to impute missing values.
# We will be using Decision Trees to impute the missing values of 'Gender'. 
# The variables used to impute it are 'Visits', 'OS' and 'Transactions'.

# Impute 'Transactions' by Linear Regression
  
# Using a simple linear regression, we will impute 'Transactions' by including the 
# imputed missing values for 'Gender' (imputed from Decision Tree). 
# The variables used to impute it are 'Visits', 'OS' and 'Gender'.

### Imputation by Decision Tree and Linear regression

ind <- which(is.na(data.train$Gender))
dt <- rpart(Gender ~ Transactions + OS, data = data.train[-ind,])

pred <- predict(dt, newdata = data.train[ind,], type = "class")
data.train.impute <- data.train
data.train.impute$Gender[ind] <- pred

ind.tr <- which(is.na(data.train$Transactions))
trans.lm <- lm(Transactions ~ OS + Gender, data = data.train.impute[-ind.tr,])

pred <- predict(trans.lm, newdata = data.train[ind.tr,])
data.train.impute$Transactions[ind.tr] <- pred

imp <- lm(Revenue ~ Transactions + OS + Gender, data = data.train.impute)
summary(imp)

#### Inference:

# It can be observed that 'Deletion' is the worst performing method and the best one
# is 'Imputation by Predictive Model' followed by 'Imputation by Average'.

### Conclusion

# Imputation of missing values is a tricky subject and unless the missing data
# is not observed completely at random, imputing such missing values by a Predictive Model
# is highly desirable since it can lead to better insights and overall increase in 
# performance of your predictive models.







