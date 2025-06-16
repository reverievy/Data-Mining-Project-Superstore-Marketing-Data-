

#######Data preparation#######
superstore_data[sapply(superstore_data, is.character)]=lapply(superstore_data[sapply(superstore_data, is.character)],as.factor)
superstore_data$Complain=as.factor(superstore_data$Complain)
superstore_data$Response=as.factor(superstore_data$Response)
superstore_data$NumWebVisitsMonth=as.integer(superstore_data$NumWebVisitsMonth)
superstore_data$Kidhome=as.factor(superstore_data$Kidhome)
superstore_data$Teenhome=as.factor(superstore_data$Teenhome)

summary(superstore_data)
#Replace missing values in Income
superstore_data$Income[is.na(superstore_data$Income)]=mean(superstore_data$Income,na.rm = TRUE)

summary((superstore_data))
#Remove id, date
superstore_data_log = superstore_data[,c(-1,-8)]

#######Exploratory Analysis########## 
#start with target
install.packages("ggplot2")
library(ggplot2)
ggplot(data=superstore_data,aes(x=Response))+geom_bar()

#correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)

superstore_data_cor=cor(superstore_data[,9:20])
pvalues=cor_pmat(superstore_data[,9:20])
ggcorrplot(superstore_data_cor,hc.order=TRUE, type="lower",lab=TRUE,p.mat=pvalues)

##numerical variables
hist(superstore_data$Income)
hist(superstore_data$Recency)
hist(superstore_data$NumWebVisitsMonth)
hist(superstore_data$Year_Birth)

ggplot(data=superstore_data, aes(x=Income,y=MntWines))+geom_point(color="blue",size=2)
ggplot(data=superstore_data, aes(x=Income,y=Recency))+geom_point(color="blue",size=2)
ggplot(data=superstore_data, aes(x=NumWebVisitsMonth,y=NumWebPurchases))+geom_point(color="blue",size=2)
ggplot(data=superstore_data, aes(x=NumWebVisitsMonth,y=NumCatalogPurchases))+geom_point(color="blue",size=2)
ggplot(data=superstore_data, aes(x=MntWines,y=MntFruits))+geom_point(color="blue",size=2)

#difference
ggplot(data=superstore_data,mapping=aes(x=Year_Birth))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=Income))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=Recency))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=MntWines))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=MntMeatProducts))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x= NumCatalogPurchases))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x= NumWebVisitsMonth))+geom_histogram()+facet_wrap(~Response,nrow=1)

#not so different
ggplot(data=superstore_data,mapping=aes(x=MntFruits))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=MntFishProducts))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x=MntSweetProducts))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x= MntGoldProds))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x= NumDealsPurchases))+geom_histogram()+facet_wrap(~Response,nrow=1)
ggplot(data=superstore_data,mapping=aes(x= NumWebPurchases))+geom_histogram()+facet_wrap(~Response,nrow=1)


##categorical variables 

ggplot(data=superstore_data, aes(x=Kidhome, fill=Education))+geom_bar(position="fill")
ggplot(data=superstore_data, aes(x=Marital_Status))+geom_bar()
library(dplyr)
thethree <- superstore_data %>% filter(Marital_Status=='Absurd'| Marital_Status=='Alone'| Marital_Status=='YOLO')

ggplot(data=superstore_data, aes(x=Education, fill=Response))+geom_bar()
#lots of variation within these 4
ggplot(data=superstore_data, aes(x=Education, fill=Response))+geom_bar(position="fill")

ggplot(data=superstore_data, aes(x=Marital_Status,fill=Response))+geom_bar(position="fill")
ggplot(data=superstore_data, aes(x=Kidhome,fill=Response))+geom_bar(position="fill")

ggplot(data=superstore_data, aes(x=Teenhome,fill=Response))+geom_bar(position="fill")
#not much w complain
ggplot(data=superstore_data, aes(x=Complain,fill=Response))+geom_bar(position="fill")

#####Splitting training and test set######
set.seed(12345)
sample <- sample(c(TRUE, FALSE), nrow(superstore_data_log), replace = TRUE, prob = c(0.8, 0.2))
superstore_data_train <- superstore_data_log[sample, ]
superstore_data_test<- superstore_data_log[!sample, ]

#validate the training and test sets
#numerical variables
t.test(superstore_data_train$Year_Birth,superstore_data_test$Year_Birth,var.equal=TRUE)
t.test(superstore_data_train$Income,superstore_data_test$Income,var.equal=TRUE)
t.test(superstore_data_train$Recency,superstore_data_test$Recency,var.equal=TRUE)
t.test(superstore_data_train$MntWines ,superstore_data_test$MntWines ,var.equal=TRUE)
t.test(superstore_data_train$MntFruits,superstore_data_test$MntFruits ,var.equal=TRUE)
t.test(superstore_data_train$MntMeatProducts,superstore_data_test$MntMeatProducts ,var.equal=TRUE)
t.test(superstore_data_train$MntFishProducts,superstore_data_test$MntFishProducts,var.equal=TRUE)
t.test(superstore_data_train$MntSweetProducts,superstore_data_test$MntSweetProducts,var.equal=TRUE)
t.test(superstore_data_train$MntGoldProds,superstore_data_test$MntGoldProds,var.equal=TRUE)
t.test(superstore_data_train$NumDealsPurchases,superstore_data_test$NumDealsPurchases,var.equal=TRUE)
t.test(superstore_data_train$NumWebPurchases,superstore_data_test$NumWebPurchases,var.equal=TRUE)
t.test(superstore_data_train$NumCatalogPurchases,superstore_data_test$NumCatalogPurchases,var.equal=TRUE)
t.test(superstore_data_train$NumStorePurchases,superstore_data_test$NumStorePurchases,var.equal=TRUE)

#categorical variables
table1=table(superstore_data_train$Education)
table2=table(superstore_data_test$Education)
table3=rbind(table1,table2)
chisq.test(table3)

table1=table(superstore_data_train$Marital_Status)
table2=table(superstore_data_test$Marital_Status)
table3=rbind(table1,table2)
chisq.test(table3)

table1=table(superstore_data_train$Kidhome)
table2=table(superstore_data_test$Kidhome)
table3=rbind(table1,table2)
chisq.test(table3)

table1=table(superstore_data_train$Teenhome)
table2=table(superstore_data_test$Teenhome)
table3=rbind(table1,table2)
chisq.test(table3)


table1=table(superstore_data_train$Complain)
table2=table(superstore_data_test$Complain)
table3=rbind(table1,table2)
chisq.test(table3)

######Balancing######
#is our training set imbalanced?
table2=table(superstore_data_train$Response)
table2[2]/nrow(superstore_data_train)
#imbalanced

#balance training set to have 15% yes in it
table2[2]/0.16
table2
#we have to brng training set down to 1625 so yes can be 16%
nrow(superstore_data_train)
#how many do I need to remove? to bring 2240 to 2227
nrow(superstore_data_train)-table2[2]/.16
#what proportion is of No being removed is that?
(nrow(superstore_data_train)-table2[2]/.16)/table2[1]

#create training subset with only no 
superstore_data_train_0=subset(superstore_data_train,superstore_data_train$Response=='0')

#randomly select observations to remove-> remove 9.78%
removal=sample(c(TRUE,FALSE),nrow(superstore_data_train_0),replace=TRUE,prob=c(0.0978189,(1-0.0978189)))
superstore_data_train_balanced=superstore_data_train_0[!removal,]
superstore_data_train_balanced=rbind(superstore_data_train_balanced,subset(superstore_data_train,superstore_data_train$Response==1))

table3=table(superstore_data_train_balanced$Response)
table3[2]/nrow(superstore_data_train_balanced)
#balanced

#####C45 tree #########
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

##build C4.5 tree 
c45_tree_balanced=rpart(Response~.,data=superstore_data_train_balanced,parms=list(split="information"))

#look at tree
c45_tree_balanced
rpart.plot(c45_tree_balanced)

#check accuracy with training set 
superstore_data_train_balanced$predresponse=predict(c45_tree_balanced,newdata=superstore_data_train_balanced,type = "class")
train_table2=table(superstore_data_train_balanced$Response,superstore_data_train_balanced$predresponse)
train_table2
(train_table2[1,1]+train_table2[2,2])/nrow(superstore_data_train_balanced)
#our trees predicted correctly 89.35% of the time on training data

#check accuracy with test set
superstore_data_test$predresponse=predict(c45_tree_balanced,newdata = superstore_data_test,type = "class")
test_table2=table(superstore_data_test$Response,superstore_data_test$predresponse)
test_table2
(test_table2[1,1]+test_table2[2,2])/nrow(superstore_data_test)

##### Buid CART tree #####
cart_tree1=rpart(Response~., data=superstore_data_train_balanced)
cart_tree1
rpart.plot(cart_tree1)

cart_tree2=rpart(Response~., data=superstore_data_test)
cart_tree
rpart.plot(cart_tree2)

#Evaluate 
#check accuracy with training set
superstore_data_train_balanced$pred=predict(cart_tree1,newdata=superstore_data_train_balanced,type="class")
train_table=table(superstore_data_train_balanced$Response,superstore_data_train_balanced$pred)
train_table
(train_table[1,1]+train_table[2,2])/nrow(superstore_data_train_balanced)

#check accuracy with test set
superstore_data_test$pred=predict(cart_tree2,newdata=superstore_data_test,type="class")
test_table=table(superstore_data_test$Response,superstore_data_test$pred)
test_table
(test_table[1,1]+test_table[2,2])/nrow(superstore_data_test)

##### model evaluation
precision1=(train_table[2,2]/(train_table[2,2]+train_table[1,2]))
precision1
recall1=(train_table[2,2]/(train_table[2,2]+train_table[2,1]))
recall1

precision2=(test_table[2,2]/(test_table[2,2]+test_table[1,2]))
precision2
recall2=(test_table[2,2]/(test_table[2,2]+test_table[2,1]))
recall2
