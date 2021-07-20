#author: Caner Kaya
#Associations -task


#set working directory - adjust a path to your directory with datasets
#setwd("path")
#getwd()

#importing libraries
library(arules) # association rules
library(arulesViz) # visualization of reles

#downloading the supermarket dataset
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
marketSet = read.csv('supermarket.csv',sep=';')
head(marketSet,5)

#convertion to dataset
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))

#about dataset
summary(marketSet)
head(marketSet)
colnames(marketSet)
dim(marketSet)
#View(marketSet)

# We are checking if is there any NA
length(which(is.na(marketSet) == TRUE))
is.na(marketSet)

#the "total" column is full of NA values

#removing "total" column
marketSet = within(marketSet, rm("total"))
dim(marketSet)
length(which(is.na(marketSet)==TRUE))

#There is no NA value now

#converting dataframe to transactions type
marketTR <- as(marketSet, "transactions")
str(marketTR)
summary(marketTR)

#We can see the summary of the marketSet with transactions data type

# 4627 rows (elements/itemsets/transactions) and
# 122 columns (items) and a density of 0.1519272 
# 
# most frequent items:
#   bread.and.cake          fruit     vegetables     milk.cream   baking.needs        (Other) 
# 3330           2962           2961           2939           2795          70775 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25 
# 36  19  22  22  26  46  58  83  97 148 181 198 228 274 271 299 249 260 238 205 172 194 177 162 143 
# 26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  48 
# 124 118 106  95  68  65  57  39  30  28  21  17  10  18   6   3   4   3   4   2   1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   13.00   18.00   18.54   23.00   48.00 


# The most frequent itemsets
# bread.and.cake         
# fruit
# vegetables
# milk.cream
# baking.needs

#Frequency of items
?itemFrequency

#relative freq
freqTbl1  = itemFrequency(marketTR, type = "relative")
length(freqTbl1)
str(freqTbl1)
summary(freqTbl1)

#sorting according  relative support values
freqTbl1 = sort(freqTbl1, decreasing= TRUE)

# the number of element which is more than 75% support
length(freqTbl1[freqTbl1>=0.75])  # no element 

# the number of element which is more than 50% support
length(freqTbl1[freqTbl1>=0.50])   #9 element

# the number of element which is more than 25% support
length(freqTbl1[freqTbl1>=0.25])   #28 element

# the number of element which is more than 10% support
length(freqTbl1[freqTbl1>=0.1])   #50 element

#we can observe that increases of support value decreases the number of elements
# And less than 50% of the elements provide 10% support value

#chart
itemFrequencyPlot(marketTR, type ="relative", support= 0.1)

########## Frequent Itemsets ##########

#setting parameters
?APparameter
aParam  = new("APparameter", "confidence" = 0.8, "support" =0.5, "minlen"= 2) 
aParam@target ="frequent itemsets"
str(aParam)

#Apriori algorithm
?apriori
asets <-apriori(marketTR,aParam)
length(asets) #2

#changing minsupport to 0.4
aParam@support = 0.4
asets <-apriori(marketTR,aParam)
length(asets) #15

#changing minsupport to 0.25
aParam@support = 0.25
asets <-apriori(marketTR,aParam)
length(asets) #196

# Minsup | Number of rules
# 0.5    | 2
# 0.4    | 15
# 0.25   | 196

#As seen above, we can get more rules with low support value.

#analysis of discovered frequent itemsets 
# "confidence" = 0.8, "support" =0.25, "minlen"= 2

str(asets)
summary(asets)
str(asets)

#we can see that the longest itemset length with given parameter is 4 
summary(size(asets))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   2.000   2.444   3.000   4.000 

#itemsets with size of 4
inspect(asets[size(asets)==4])
summary(asets[size(asets)==4])
# items                                               support   count
# [1] {bread.and.cake,biscuits,fruit,vegetables}          0.2628053 1216 
# [2] {bread.and.cake,frozen.foods,fruit,vegetables}      0.2684245 1242 
# [3] {bread.and.cake,baking.needs,milk.cream,fruit}      0.2509185 1161 
# [4] {bread.and.cake,baking.needs,milk.cream,vegetables} 0.2526475 1169 
# [5] {bread.and.cake,milk.cream,fruit,vegetables}        0.2833369 1311 
# [6] {bread.and.cake,baking.needs,fruit,vegetables}      0.2712341 1255 

#summary
# support           count     
# Min.   :0.2509   Min.   :1161  
# 1st Qu.:0.2552   1st Qu.:1181  
# Median :0.2656   Median :1229  
# Mean   :0.2649   Mean   :1226  
# 3rd Qu.:0.2705   3rd Qu.:1252  
# Max.   :0.2833   Max.   :1311  



#itemsets with size of 3
itemsets_3 = asets[size(asets)==3]
summary(itemsets_3)
inspect(head(sort(itemsets_3, by="support"),10))
# items                                    support   count
# [1]  {bread.and.cake,fruit,vegetables}        0.3870759 1791 
# [2]  {bread.and.cake,milk.cream,fruit}        0.3639507 1684 
# [3]  {bread.and.cake,milk.cream,vegetables}   0.3583315 1658 
# [4]  {bread.and.cake,baking.needs,vegetables} 0.3427707 1586 
# [5]  {bread.and.cake,baking.needs,milk.cream} 0.3414740 1580 
# [6]  {milk.cream,fruit,vegetables}            0.3395289 1571 
# [7]  {bread.and.cake,baking.needs,fruit}      0.3380160 1564 
# [8]  {bread.and.cake,frozen.foods,fruit}      0.3345580 1548 
# [9]  {bread.and.cake,frozen.foods,vegetables} 0.3345580 1548 
# [10] {bread.and.cake,biscuits,fruit}          0.3330452 1541 

#summary
# support           count     
# Min.   :0.2505   Min.   :1159  
# 1st Qu.:0.2702   1st Qu.:1250  
# Median :0.2797   Median :1294  
# Mean   :0.2892   Mean   :1338  
# 3rd Qu.:0.3021   3rd Qu.:1398  
# Max.   :0.3871   Max.   :1791  


itemsets_2 = asets[size(asets)==2]
summary(itemsets_2)
#summary
# support           count     
# Min.   :0.2503   Min.   :1158  
# 1st Qu.:0.2753   1st Qu.:1274  
# Median :0.3175   Median :1469  
# Mean   :0.3250   Mean   :1504  
# 3rd Qu.:0.3547   3rd Qu.:1641  
# Max.   :0.5051   Max.   :2337  

#We can observe that As the length decreases, 
#the average and maximum support value increases.

#charts
plot(asets[size(asets)>1], method = "graph")
plot(asets[size(asets)>2], method = "graph")
plot(asets[size(asets)>3], method = "graph")

#First 10 itemsets ordered by support value
inspect(head(sort(asets, by="support"),10))

# items                         support   count
# [1]  {bread.and.cake,milk.cream}   0.5050789 2337 
# [2]  {bread.and.cake,fruit}        0.5024854 2325 
# [3]  {bread.and.cake,vegetables}   0.4966501 2298 
# [4]  {fruit,vegetables}            0.4769829 2207 
# [5]  {bread.and.cake,baking.needs} 0.4735250 2191 
# [6]  {bread.and.cake,frozen.foods} 0.4601254 2129 
# [7]  {bread.and.cake,biscuits}     0.4501837 2083 
# [8]  {milk.cream,fruit}            0.4404582 2038 
# [9]  {milk.cream,vegetables}       0.4376486 2025 
# [10] {baking.needs,vegetables}     0.4212233 1949 



setsfruit <- subset(asets, subset = items %in% "fruit")
inspect(setsfruit)

#It is the interesting result that the fruit item's 3th pair is the departmen137
#We can assume that department137 is preferred by people because its fruit is good.
# [3]  {fruit,department137}             0.2753404 1274 


####### Association Rules ##########

assoRule = new("APparameter", "confidence"=0.5, "support"=0.2, "minlen"=2, target = "rules")
print(assoRule)
assoRules <- apriori(marketSet,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]

#about rules
str(assoRules)
summary(assoRules)
length(assoRules)

# set of 1291 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2   3   4   5 
# 261 712 308  10 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   3.000   3.000   3.052   3.000   5.000 
# 
# summary of quality measures:
#   support         confidence          lift           count     
# Min.   :0.2001   Min.   :0.5026   Min.   :1.011   Min.   : 926  
# 1st Qu.:0.2094   1st Qu.:0.6308   1st Qu.:1.125   1st Qu.: 969  
# Median :0.2336   Median :0.7051   Median :1.163   Median :1081  
# Mean   :0.2498   Mean   :0.6976   Mean   :1.171   Mean   :1156  
# 3rd Qu.:0.2712   3rd Qu.:0.7480   3rd Qu.:1.212   3rd Qu.:1255  
# Max.   :0.5051   Max.   :0.8941   Max.   :1.377   Max.   :2337  
# 
# mining info:
#   data ntransactions support confidence
# marketSet          4627     0.2        0.5

#we can see that lift measure is always greater than 1 
#is means rule body is effect positively on the head of rule


#changing support to 0.1
assoRule@support = 0.1
assoRules <- apriori(marketSet,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]

#about rules
str(assoRules)
summary(assoRules)
length(assoRules)

# set of 24363 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2     3     4     5     6     7 
# 494  4365 10224  7599  1613    68 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   4.000   4.000   4.233   5.000   7.000 

# summary of quality measures:
#   support         confidence          lift            count       
# Min.   :0.1001   Min.   :0.5000   Min.   :0.9402   Min.   : 463.0  
# 1st Qu.:0.1076   1st Qu.:0.6555   1st Qu.:1.1969   1st Qu.: 498.0  
# Median :0.1189   Median :0.7307   Median :1.2675   Median : 550.0  
# Mean   :0.1310   Mean   :0.7182   Mean   :1.2723   Mean   : 606.1  
# 3rd Qu.:0.1400   3rd Qu.:0.7848   3rd Qu.:1.3390   3rd Qu.: 648.0  
# Max.   :0.5051   Max.   :0.9205   Max.   :1.7263   Max.   :2337.0  
# 
# mining info:
#   data ntransactions support confidence
# marketSet          4627     0.1        0.5

#after decreasing support value, the number of discovered rules increased from 1291 to 24363
# and minimum lift measure is became less than 1 it means that there is some rules with negative effect


#itemsets with size of 2
inspect(assoRules[size(assoRules)==2])
summary(assoRules[size(assoRules)==2])
# support         confidence          lift           count   
#  Mean   :0.2400   Mean   :0.7492   Mean   :1.129   Mean   :1110.6 

#itemsets with size of 3
inspect(assoRules[size(assoRules)==3])
summary(assoRules[size(assoRules)==3])
#support         confidence          lift           count     
#Mean   :0.1575   Mean   :0.7513   Mean   :1.191   Mean   : 729 

#itemsets with size of 4
inspect(assoRules[size(assoRules)==4])
summary(assoRules[size(assoRules)==4])
#   support         confidence          lift           count       
#Mean   :0.1317   Mean   :0.7709   Mean   :1.242   Mean   : 609.5 

#We can see that long rules have lower support values than shorts

inspect(head(sort(assoRules, by="support"),10))

# lhs                    rhs              support   confidence lift     count
# [1]  {milk.cream}        => {bread.and.cake} 0.5050789 0.7951684  1.104878 2337 
# [2]  {bread.and.cake}    => {milk.cream}     0.5050789 0.7018018  1.104878 2337 
# [3]  {fruit}             => {bread.and.cake} 0.5024854 0.7849426  1.090670 2325 
# [4]  {vegetables}        => {bread.and.cake} 0.4966501 0.7760892  1.078368 2298 
# [5]  {fruit}             => {vegetables}     0.4769829 0.7451047  1.164336 2207 
# [6]  {vegetables}        => {fruit}          0.4769829 0.7453563  1.164336 2207 
# [7]  {baking.needs}      => {bread.and.cake} 0.4735250 0.7838998  1.089221 2191 
# [8]  {frozen.foods}      => {bread.and.cake} 0.4601254 0.7835848  1.088783 2129 
# [9]  {biscuits}          => {bread.and.cake} 0.4501837 0.7996161  1.111058 2083 
# [10] {juice.sat.cord.ms} => {bread.and.cake} 0.4039334 0.7588307  1.054387 1869 

#Short rules stand out as the results are ranked according to the support value.

#The results show that most of the bodies of the rules
#are consist "bread.and.cake" item which is most frequent item

#There is a 2-way relationship between milk.cream and bread.and.cake, as seen in the 1st and 2nd rows of the rules. 
#As can be understood from the quality values, there is a close relationship between these two.

########## Conclusion ##########

# In this task we discovered frequent itemsets and association rules
# and discussed their relation with different support values

# We observed that the both mining type have similar relations as mentioned below:

# The mean of support values of short rules is higher than long rules

# Increasing the support value results in a decrease in the number of discovered rules

# Increasing the support value causes rules generally consisting of frequent itemsets
#to appear in the discovered rules. 

#Increasing the support value increases the probability that 
#the lift measurement is greater than 1.

# Lift measurement shows the importance of rules. 
# Filtering can be done on the discovered rules according to the lift value.

