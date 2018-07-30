setwd("C:\\Users\\User\\Desktop\\Data Science")
# assigning the date to the variable
car <- read.csv("Car Price.csv")
names(car) #checking for the variable names
dim(car) # checking for number of variables and obsevations

sapply(car,function(x) is.na(x))
sum(is.na(car))# number of missinf values
summary(car)

# checking for outliers using box plot and capping the putliers to the nearest maximum value
boxplot(car$symboling)
boxplot(car$wheel_base)
car$wheel_base[which(car$wheel_base<=50)] <- 86.6
boxplot(car$wheel_base)
boxplot(car$length)
car$length[which(car$length>210)] <- 208.1
boxplot(car$length)
boxplot(car$width)
boxplot(car$height)
boxplot(car$curb_weight)
boxplot(car$engine_size)
car$engine_size[which(car$engine_size>210)] <- 209
sum(is.na(car$horsepower))
boxplot(car$horsepower) 
car$horsepower[which(car$horsepower>185)] <- 184
boxplot(car$horsepower)
boxplot(car$peak_rpm)
car$peak_rpm[which(car$peak_rpm>6001)] <- 6000
boxplot(car$peak_rpm)
boxplot(car$city_mpg)
car$city_mpg[which(car$city_mpg>39)] <- 39
boxplot(car$highway_mpg)
boxplot(car$bore_new)
boxplot(car$stroke)
car$stroke[which(car$stroke <2.6)] <- 2.64
car$stroke[which(car$stroke>3.91)] <- 3.9
boxplot(car$compression_ratio)
car$compression_ratio[which(car$compression_ratio>12)] <- 11.5
boxplot(car$horsepower)
boxplot(car$stroke_new)
car$stroke_new[which(car$stroke_new <2.6)] <- 2.64
car$stroke_new[which(car$stroke_new>3.91)] <- 3.9
boxplot(car$horsepower_new)
car$horsepower_new[which(car$horsepower_new>185)] <- 184
boxplot(car$peak_rpm_new)
car$peak_rpm_new[which(car$peak_rpm_new>6001)] <- 6000
#checking the box plot after removing outliers
boxplot(car$symboling)
boxplot(car$normalized_losses)
boxplot(car$wheel_base)
boxplot(car$length)
boxplot(car$width)
boxplot(car$height)
boxplot(car$curb_weight)
boxplot(car$engine_size)
boxplot(car$horsepower) 
boxplot(car$horsepower)
boxplot(car$peak_rpm)
boxplot(car$city_mpg)
boxplot(car$highway_mpg)
boxplot(car$bore_new)
boxplot(car$stroke)
boxplot(car$compression_ratio)
boxplot(car$horsepower)
boxplot(car$stroke_new)
boxplot(car$horsepower_new)
boxplot(car$peak_rpm_new)
str(car)
summary(car)
#treating the missing values
# for continuous variable missing data is replaced with mean
#for catagorical data missing data is replaced with mode
car$num_of_doors[is.na(car$num_of_doors)]="two"
car$engine_location[is.na(car$engine_location)]="front"
car$bore[is.na(car$bore)]=3.298
car$stroke[is.na(car$stroke)]=3.363
car$horsepower[is.na(car$horsepower)]=100.6
car$peak_rpm[is.na(car$peak_rpm)]=5176
sum(is.na(car))
#binary covertion of the target variable
car$Car_Price_category=ifelse(car$Car_Price_category=="Lowprice",0,1)

sapply(car, function(x)sum(is.na(x)))
library(tidyr)
#Seperating continuous data to check correlation
check <-car %>% select_if(is.numeric)
names(check)
cor(check)
check1 <- car %>% select_if(is.factor)
names(check1)
#perform t Test and chi square Test to identify the significant variables to buils model
chisq.test(table(car$make,car$Car_Price_category)) #significant
chisq.test(table(car$Car_Price_category,car$symboling)) #significant
car$Car_Price_category=as.factor(car$Car_Price_category)
str(car$normalized_losses)
t.test(car$normalized_losses~car$Car_Price_category)# not significant
t.test(car$wheel_base~car$Car_Price_category)#significant
chisq.test(table(car$Car_Price_category,car$fuel_type)) #significant
chisq.test(table(car$Car_Price_category,car$aspiration))#not  significant
chisq.test(table(car$Car_Price_category,car$num_of_doors))#not signficant
chisq.test(table(car$Car_Price_category,car$body_style))# significant
chisq.test(table(car$Car_Price_category,car$drive_wheels))#significant
chisq.test(table(car$Car_Price_category,car$engine_location))#significant
chisq.test(table(car$Car_Price_category,car$engine_type))#significant
chisq.test(table(car$Car_Price_category,car$num_of_cylinders))#sigificant
chisq.test(table(car$Car_Price_category,car$num_of_doors_new))#not significant
chisq.test(table(car$Car_Price_category,car$drive_wheel_new))#significant
chisq.test(table(car$Car_Price_category,car$engine_location_new))#significant
t.test(car$length~car$Car_Price_category)#significant
t.test(car$width~car$Car_Price_category)#significant
t.test(car$height~car$Car_Price_category)#significant
t.test(car$curb_weight~car$Car_Price_category)#significant
t.test(car$engine_size~car$Car_Price_category)#significant
t.test(car$bore~car$Car_Price_category)#significant
t.test(car$stroke~car$Car_Price_category)#significant
t.test(car$compression_ratio~car$Car_Price_category)#significant
t.test(car$horsepower~car$Car_Price_category)#significant
t.test(car$peak_rpm~car$Car_Price_category)#not significant
t.test(car$city_mpg~car$Car_Price_category)#significant
t.test(car$highway_mpg~car$Car_Price_category)#significant
t.test(car$horsepower_new~car$Car_Price_category)#significant
t.test(car$peak_rpm_new~car$Car_Price_category)#not significant

library(caret)
#checking the data distribution
prop.table(table(car$Car_Price_category))
#data is unbalance as it has 91.558% of o's and 8.452% od 1's
set.seed(4444)
library(DMwR)
# SMOTE function to re-sample the data and balance the data
car = SMOTE(Car_Price_category~., car, perc.over = 200, k = 5, perc.under = 200)
library(caret)
# seperating the train data and test data
c1 <- createDataPartition(car$Car_Price_category, p = 0.7, list = F )
head(c1)
str(car)
train <- car[c1,]
test <- car[-c1,]
dim(train)
dim(test)
prop.table(table(car$Car_Price_category))
prop.table(table(train$Car_Price_category))
prop.table(table(test$Car_Price_category))

#build the model using all the significant variables

ml <- glm(Car_Price_category~symboling
          +wheel_base+ length+ width+ height
          + engine_size+ highway_mpg
          + bore_new+ stroke_new+ horsepower_new+drive_wheel_new
          +engine_location_new+fuel_system
          +fuel_type+ drive_wheels+ body_style,
          data=train,family = "binomial")
summary(ml)
library(car)
vif(ml) # we were not able to finf VIF as there are aliased coefficients in the model
alias(ml)# checking for the aliasing co-efficients 
#mlstep <- step(ml)
#summary(mlstep)
#vif(mlstep)

#after removing the alias co-efficient
ml2 <- glm(Car_Price_category~symboling+width+height+highway_mpg+bore_new+stroke_new
               +horsepower_new+drive_wheel_new+fuel_type+body_style,data=train,family = "binomial")
  
  


summary(ml2)

vif(ml2)
#remove the highest vif valued variable drive_wheel_new
ml3 <- glm(Car_Price_category~ width
           + highway_mpg
           + horsepower_new,
           data=train,family = "binomial")
summary(ml3)


vif(ml3)

#removed fuel type, number cyli,make
#remove highest VIF

train$CPC <- predict(ml3,train,type="response")
train$CR_gp <- ifelse(train$CPC>=0.5,"high","low")
x1 <- table(train$Car_Price_category,train$CR_gp)
x1
prop.table(x1,1)


test$CPC <- predict(ml3,test,type="response")
test$CR_gp <- ifelse(test$CPC>=0.5,"high","low")
x2 <- table(test$Car_Price_category,test$CR_gp)
x2
prop.table(x2,1)


#sum(diag(x1))/sum(x1)

#concordence and discordence
#install.packages("InformationValue")
library(InformationValue)
Concordance(train$Car_Price_category,train$CPC)
Concordance(test$Car_Price_category,test$CPC)

##Somers'D
somersD(train$Car_Price_category,train$CPC)
somersD(test$Car_Price_category,test$CPC)

##ROC curve
plotROC(train$Car_Price_category,train$CPC)
plotROC(test$Car_Price_category,test$CPC)

##Rank ordering and KS stat
Decile <- cut(train$CPC,
              breaks = quantile(train$CPC,probs = seq(0,1,by=0.1)),
              include.lowest = T)


table(Decile,train$Car_Price_category)
hist(train$CPC)


