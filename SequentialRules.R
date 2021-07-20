###############################
#author: Caner Kaya - K-6211
#Sequential rules discovery task

#Objective : To find out if occurrence of hypoglycemic symptoms may be predicted 
#based on other events

###############################

#set working directory
#setwd("path")
#getwd()

#importing libraries
library(arules) # association rules
library(arulesSequences)

############# DATASET #############

# event ids and descriptions
# 33 = Regular insulin dose
# 34 = NPH insulin dose
# 35 = UltraLente insulin dose
# 48 = Unspecified blood glucose measurement    
# 57 = Unspecified blood glucose measurement
# 58 = Pre-breakfast blood glucose measurement         
# 59 = Post-breakfast blood glucose measurement
# 60 = Pre-lunch blood glucose measurement
# 61 = Post-lunch blood glucose measurement
# 62 = Pre-supper blood glucose measurement       .
# 63 = Post-supper blood glucose measurement
# 64 = Pre-snack blood glucose measurement
# 65 = Hypoglycemic symptoms
# 66 = Typical meal ingestion
# 67 = More-than-usual meal ingestion
# 68 = Less-than-usual meal ingestion
# 69 = Typical exercise activity
# 70 = More-than-usual exercise activity
# 71 = Less-than-usual exercise activity
# 72 = Unspecified special event

# As you can see there are different types of events. 
# These can be classified as measurement, taking insulin, exercising, and meal ingestion.
# The the measurement values will be discreatize. 
# The effect of other events on mesurement will be observed through sequential rule mining.

#downloading the diabet dataset
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
diabeteSet = read.csv('diab_trans.data',sep=',')
head(diabeteSet,5)
View(diabeteSet)
summary(diabeteSet)

#omitting NA values
diabeteSet<-na.omit(diabeteSet)


sort(unique(diabeteSet$code))
#we see 2 more ids with no description from the dataset
 
#Delete the rows containing unexplained activities whose values are not disclosed.
#no description above: 36 ,56
#unspecified special event: 72

dropList=c(diabeteSet[,"code"] =="id_36" | 
          diabeteSet[,"code"] =="id_56" |
          diabeteSet[,"code"] =="id_72")

diabeteSet = diabeteSet[!dropList,]


############## DISCREATIZATION #############
# https://www.medicinenet.com/normal_blood_sugar_levels_in_adults_with_diabetes/article.htm
#By considering the values in the article, the discretization values suitable for the measurements in the data set were determined as follows.

#   | before meal                        | after meal                           | unspecified                      |
#   |------------------------------------|-------------------------------------------------------------------------|
#   | value< 70 : hypo                   | value < 70 : hypo                    |value < 70 : hypo                 |
#   | value >=70 and value<= 130 : normal| value >=70 and value <= 180 : normal |value>=70 and value<= 180: normal |                
#   | value 130 > : hyper                | value > 180 : hyper                  |value>180 : hyper                 |


for(row in 1:nrow(diabeteSet))
{ 
  # before meal 
  if( diabeteSet[row,"code"] == "id_58" | 
      diabeteSet[row,"code"] == "id_60" |
      diabeteSet[row,"code"] == "id_62")
  {
    if (diabeteSet[row,"value"] < 70)
      diabeteSet[row,"value"] = "hypo"
    else if(diabeteSet[row,"value"] > 130)
      diabeteSet[row,"value"] = "hyper"
    else
      diabeteSet[row,"value"] = "normal"
    
  }#after meal
  else if( diabeteSet[row,"code"] == "id_59" | 
           diabeteSet[row,"code"] == "id_61" |
           diabeteSet[row,"code"] == "id_63" |
           diabeteSet[row,"code"] == "id_64")
  {
    if (diabeteSet[row,"value"] < 180)
      diabeteSet[row,"value"] = "normal"
    else
      diabeteSet[row,"value"] = "hyper"
  }#unspecified
  else if(diabeteSet[row,"code"] == "id_48"|
          diabeteSet[row,"code"] == "id_57")
  { 
    if (diabeteSet[row,"value"] < 70)
      diabeteSet[row,"value"] = "hypo"
    else if(diabeteSet[row,"value"] > 180)
      diabeteSet[row,"value"] = "hyper"
    else
      diabeteSet[row,"value"] = "normal"
  }
}
  
write.table(diabeteSet, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))

summary(diabSeq)
# transactions as itemMatrix in sparse format with
# 24923 rows (elements/itemsets/transactions) and
# 70 columns (items) and a density of 0.02857143 
# 
# most frequent items:
#   "hypo" "id_33" "id_34" "id_58" "id_62" (Other) 
# 8816    7910    3308    2998    2638   24176 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 2 
# 24923 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2 
# 
# includes extended item information - examples:
#   labels
# 1    "0"
# 2    "1"
# 3   "10"
# 
# includes extended transaction information - examples:
#   sequenceID  eventID
# 1          1 96772141
# 2          1 96772142
# 3          1 96772143

#information about data concerning times 
timeSeq  = as(diabSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
freqT

spanT= timeFrequency(timeSeq, "span")
spanT

#calculation of frequency of items
freqItem = itemFrequency(diabSeq)
#str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )

head(freqItem,20)

############frequent sequences discovery############
#parameters of Spade algorithm
?cspade
#To observe the effects of short-term activities such as eating and exercising, 
#it is sufficient to choose the maxGap around 4-5 hours. 
#Inusilin intake shows its effect on blood sugar up to 1 - 2 days,
#but the first 5 hours effect was found to be sufficient to shorten the execution time.
#that's why maxGap was chosen as 4.8 hours
seqParam = new ("SPparameter",support = 0.05, maxsize = 5, mingap=600, maxgap =17280, maxlen = 5)
print(seqParam)

#execution of the algorithm
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#information about discoverd sequences
summary(patSeq)
#length(patSeq)

inspect(head(patSeq,100))

# Discovery of sequential rules
seqRules = ruleInduction(patSeq, confidence = 0.5)
length(seqRules)
inspect(head(seqRules, 100))


seqRules2 = subset(seqRules, rhs(seqRules) %in% '"hypo"')
length(seqRules2)

topLift <- sort(seqRules2, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(topLift,100))
# 1 <{"17",                         
#   "id_35"}>  => <{"hypo",    0.06060606  1.0000000 1.2000000 
#     "id_60"}>     
#   2 <{"17"}>     => <{"hypo"}>   0.16666667  1.0000000 1.0000000 
# 3 <{"26"}>     => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 4 <{"id_57"},                     
# {"0",                          
#   "id_69"}>  => <{"hypo"}>   0.07575758  1.0000000 1.0000000 
# 5 <{"hypo",                       
#   "id_57"},                     
# {"0",                          
#   "id_69"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 6 <{"id_57"},                     
# {"id_69"}>  => <{"hypo"}>   0.07575758  1.0000000 1.0000000 
# 7 <{"hypo",                       
#   "id_57"},                     
# {"id_69"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 8 <{"id_58"},                     
# {"hypo",                       
#   "id_58"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 9 <{"hypo",                       
#   "id_58"},                     
# {"hypo",                       
#   "id_58"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 10 <{"17",                         
#   "id_35"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 11 <{"17",                         
#   "id_33"}>  => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 12 <{"id_57"},                     
# {"0"},                         
# {"0"}>      => <{"hypo"}>   0.06060606  1.0000000 1.0000000 
# 13 <{"15"},                        
# {"0"}>      => <{"hypo"}>   0.07575758  1.0000000 1.0000000 
# 14 <{"id_57"},                     
# {"hypo",                       
#   "id_57"},                     
# {"0"}>      => <{"hypo"}>   0.07575758  1.0000000 1.0000000 
# 15 <{"hypo",                       
#   "id_57"},                     
# {"hypo",                       
#   "id_57"},                     
# {"0"}>      => <{"hypo"}>   0.06060606  1.0000000 1.0000000 

############## Conclusion ############## 

# As expected, low blood glucose(hypoglycemia) has been observed in 
# patients taking an overdose of inucillin. This can be understood from hypoglycemia 
# after the number of doses taken,followed by id_35 (UltraLente insulin dose) 
# or hypoglycemia after the number of doses taken.

#In the diagnosis of hypoglycemia, blood measurements taken in the 
#fasted state were more determinant than measurements made after meal.
#Because when we list the best rules according to the lift value, 
#it is encountered in hypoglycemia diagnoses as a result of before meal measurements, 
#while it is rarely seen as a result of after meal measurements.

#When we look at the listed results, we can say that in hypoglycemic patients,
# blood sugar may decrease even after a normal exercise.
# This is also very determinative for diagnosis.

