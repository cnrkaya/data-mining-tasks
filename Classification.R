##########################################################################
#author: Caner Kaya - K-6211
#Classification task
#
#Objective :To find the best classifier according to the quality measure.
#________________________________________________________
# nbTask | algorithm | Quality measure
# 3a     | C5.0      | Average recall and overall accuracy
#_________________________________________________________
##########################################################################

#import libraries
library(gmodels) #results analysis
library(Hmisc) #results analysis
library(C50) # C5 classifer
library(caret) #conf matrix


#download data
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 'adult.data')

adultSet = read.csv('adult.data',header=FALSE,col.names = c('age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country','yearly-income'))
summary(adult)

# GOAL: Three classes should be created: all Married, Never-married, other values
# While classifying marriage from relationship attribute, 
# the "Own-child" feature does not give precise information. 
# Therefore, the classification was made using the martial status attribute. 
# While Martial status is used for the label of classes, 
# the relationship attribute is also removed from the data set 
# because it is possible to classify the data just by looking at this attribute.

adultSet$marital.status = as.character(adultSet$marital.status)

adultSet$marital.status[adultSet$marital.status ==" Married-civ-spouse"|
                        adultSet$marital.status ==  " Married-spouse-absent" |
                        adultSet$marital.status == " Married-AF-spouse" ] <- "married"


adultSet$marital.status[adultSet$marital.status ==" Divorced"|
                          adultSet$marital.status ==  " Separated" |
                          adultSet$marital.status == " Widowed" ] <- "other"


adultSet$marital.status[adultSet$marital.status ==" Never-married"] <- "never-married"
adultSet$marital.status<-as.factor(adultSet$marital.status)

#deleting relationship column
adultSet <- subset (adultSet, select = -c(relationship))


################################################################
#      DATA VISUALIZATON                                       #
################################################################
library(ggplot2)
ggplot(adultSet, aes(x=yearly.income, y=1, fill=marital.status))+geom_bar(stat = "identity", position = "fill")
#As can be seen from the plot, most of those with yearly income >50K are married.

ggplot(adultSet, aes(x=age, y=1, fill=marital.status))+geom_bar(stat = "identity", position = "fill")+facet_grid(~sex)
#As the age increases in women, the probability of being included in the "other" class increases due to separations.


#creating training and test datasets 
set.seed(123)
sam <- sample(2, nrow(adultSet), replace=TRUE, prob=c(0.7, 0.3))
adultTrain1 <- adultSet[sam==1,]
adultTest1 <- adultSet[sam==2,]

################################################################
#      C5.0 classifier                                         #
################################################################

?C5.0
#model building - a decision tree
adult_C50 <- C5.0(adultTrain1[,-6], adultTrain1$marital.status) 
summary(adult_C50)

# Evaluation on training data (22881 cases):
#   
#   Decision Tree   
# ----------------  
#   Size      Errors  
# 
# 165 5533(24.2%)   <<
#   
#   
#   (a)   (b)   (c)    <-classified as
# ----  ----  ----
#   9404   962   462    (a): class married
# 1379  5478   651    (b): class never-married
# 1478   601  2466    (c): class other
# 
# 
# Attribute usage:
#   
# 100.00%	age
# 100.00%	yearly.income
# 84.24%	sex
# 27.56%	education.num
# 23.33%	capital.gain
# 22.93%	race
# 20.87%	capital.loss
# 10.62%	hours.per.week
# 5.11%	native.country
# 3.69%	occupation
# 2.15%	education
# 2.03%	workclass
# 1.65%	fnlwgt
# 
# 
# Time: 0.3 secs


#quality of classification for training data
adult_C50_trainPred <- predict(adult_C50, adultTrain1)

?CrossTable
CrossTable(adult_C50_trainPred, adultTrain1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

?confusionMatrix
confusionMatrix(adult_C50_trainPred, adultTrain1$marital.status, mode="everything")
#Train quality measures 
avg_recall = mean(c(0.8685,0.7296, 0.5426)) #0.7135667
#accuracy 0.7582 


#quality of classification for test data
adult_C50_testPred <- predict(adult_C50, adultTest1)
CrossTable(adult_C50_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(adult_C50_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c( 0.8531 ,0.6961,  0.5068)) #0.68533
#accuracy 0.7331 



#Ensemble tree - trials 30
adult_C50B <- C5.0(adultTrain1[,-6], adultTrain1$marital.status,trials = 30) 
adult_C50B_testPred <- predict(adult_C50B, adultTest1)
CrossTable(adult_C50B_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
confusionMatrix(adult_C50B_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c( 0.8509,0.7033,0.5136)) 
avg_recall #0.68927
#accuracy 0.7357 

#Ensemble tree - trials 10
adult_C50B <- C5.0(adultTrain1[,-6], adultTrain1$marital.status,trials = 10) 
adult_C50B_testPred <- predict(adult_C50B, adultTest1)
CrossTable(adult_C50B_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
confusionMatrix(adult_C50B_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c( 0.7975 , 0.7284 ,0.5692)) 
avg_recall #0.6983667
#accuracy 0.7354 

#Ensemble tree - trials 5
adult_C50B <- C5.0(adultTrain1[,-6], adultTrain1$marital.status,trials = 5) 
adult_C50B_testPred <- predict(adult_C50B, adultTest1)
CrossTable(adult_C50B_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
confusionMatrix(adult_C50B_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c( 0.8581 , 0.6932 ,0.5115)) 
avg_recall #0.6876
#accuracy 0.7354 

#Rule-Based
adult_C50R <- C5.0(adultTrain1[,-6], adultTrain1$marital.status,rules = TRUE) 
adult_C50R_testPred <- predict(adult_C50R, adultTest1)
CrossTable(adult_C50R_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
confusionMatrix(adult_C50R_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c( 0.8675 , 0.6696 , 0.5141)) 
avg_recall #0.6837333
#accuracy 0.7326 

#Rule-Based and trials 10
adult_C50B <- C5.0(adultTrain1[,-6], adultTrain1$marital.status,trials = 10,rules=TRUE) 
adult_C50B_testPred <- predict(adult_C50B, adultTest1)
CrossTable(adult_C50B_testPred, adultTest1$marital.status, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
confusionMatrix(adult_C50B_testPred, adultTest1$marital.status, mode="everything")
avg_recall = mean(c(0.8462 , 0.7266,0.48069)) 
avg_recall #0.6844967
#accuracy 0.7346 


#    ________________________________________________
#   | Parameters            | Accuracy | Avg(Recall) |
#   |-----------------------|----------|-------------|
#   | default               | 0.7331   | 0.68533     |
#   | trials=30             | 0.7357   | 0.68927     |
#   | trials=10             | 0.7354   | 0.69837     |
#   | trials=5              | 0.7354   | 0.68767     |
#   | rules=TRUE            | 0.7326   | 0.68373     |
#   | rulest=True trials=10 | 0.7346   | 0.68449     |
#   --------------------------------------------------
#                   TEST RESULTS



################################################################
#           CONCLUSION                                         #
################################################################


# The 5 most valuable attributes that determine the relationship status 
# were found as follows: age > yearly.income > sex > education.num > capital.gain 

# The least valuable attributes that determine the relationship status 
# were found as follows: fnlwgt < workclass < education < occupation < native.country

# Quality measures were close to each other. 
# Observed range [0.7326 - 0.7357] for accuracy, [0.68373 - 0.69837] for average recall

# Creating rule based tree didn't work well, it was the experiment with the worst result.

# Considering these results, it is seen that the best parameter is trials=10

# When the confusion matrix is examined in detail, it is seen that the trials=10 parameter test 
# predicts "other" class better than other tests. Altough It is slightly worse in detecting "married" and "never-married" classes,
# due to the success for detecting "other" class, it is selected as the best result when looking at the average.
