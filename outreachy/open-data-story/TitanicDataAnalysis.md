# Analysing Titanic data and Understanding Decision Trees
In this, I have analysed Titanic data from kaggle. The code has been written in R.

#### Dataset Description
The dataset consists of demographic factors like age, gender, class, and whether a person survived or not.

#### Importing dataset 

```R
#libraries
library(ggplot2)
library(dplyr)
require(rpart)
require(rpart.plot)


df_train<-read.csv("/home/reen/Desktop/Titanic/train.csv",header=TRUE,sep=",")
df_test<-read.csv("/home/reen/Desktop/Titanic/test.csv",header=TRUE,sep=",")

#### Basic stats about training and test data
summary(df_train)
summary(df_test)

#number of rows
n_row_train<-nrow(df_train)
n_row_test<-nrow(df_test)

#data types of various variables
str(df_train)

#Adding survived to test data
df_test$Survived<-0
```

#### Understanding Decision Trees
A decision tree is analogus to a graphical structure used for classification and making decisions. At each inermediate node, there has to be
a decision made for splitting the data further into more and more pure classes. The leaf nodes represent the final classification.
The decision made for splitting at each step is based on maximizing the information gain and towards purifying it. The various measures
used for this are gini index, information gain, entropy etc. In this example, the decision making steps are splitting based on
gender, age of a person, class of a person, ticket type etc, whereas the final goal or the leaf nodes would be whether 
the person survived or not.


#### A basic decision tree visualisation
```R
x.rp <- rpart(Survived ~ Age + Sex + Pclass  + SibSp + Parch +Fare + Embarked, data=df_train)
x.rp.pred <- predict(x.rp, type="class", newdata=df_test)
x.rp.prob <- predict(x.rp, type="prob", newdata=df_test)
summary(x.rp)
prp(x.rp, type = 4, extra = 100,cex=0.6)
```


![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/decision_tree.png)

#### Number of male vs female people on board
```R
df_gender <- df_train['Sex']
df_gender_plot<-table(df_gender['Sex'])
df_gender_plot<-as.data.frame(df_gender_plot)
ggplot(data=df_gender_plot, aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="turquoise3") + geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)
```
![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/gender.png)

#### People of different ages
```R
df_age<-df_train['Age']
df_age_table<-table(df_age)
with(df_age, hist(Age,col="turquoise3",main="People of different ages"))
plt_age=qplot(df_train$Age,geom='histogram',fill=I('pink'),xlab='Age', 
            ylab='Number People of different ages')
```
![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/people_ages.png)


#### Survival rate of people from different ages
```R
survival <- data.frame(Age = df_train$Age, Survived = df_train$Survived)
ggplot(survival, aes(Age,fill = factor(Survived))) +
  geom_histogram()
![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/survival_rate.png)
```

#### Age and Sex Violin Plot
```R
p<-ggplot(df_train,aes(factor(Sex),Age)) + geom_violin(aes(fill=Survived))
![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/violin.png)
```

#### Scatter plot for Pclass,Age and Fare 
```R
df_train$Pclass=as.factor(df_train$Pclass)
ggplot(data=df_train,mapping = aes(x=Age,y=Fare,shape=Pclass,color=Survived)) +geom_point()
```

![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/scatter_plot.png.png)

#### Pair wise Scatter Plot
```R
pairs(df_train[, -c(1, 2)], col = as.numeric(df_train[, 2]) + 2, pch = 20)
```
![alt text](https://github.com/avneet14027/TitanicDataAnalysis/blob/master/scatter_plot.png)












