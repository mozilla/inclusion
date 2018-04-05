# Analysis of Data-Science Survey 
In this project, I would be doing an extensive analysis and visual exploration of the various aspects of the data science 
survey conducted by kaggle. This would be done in R. The idea is to analyse various demographics factors, gender disparities
,correlation between various variables, job profiles in various countries, prevelance of ML and data science in various countries, programming languages
prevelance and preference and other key insights that can be dervied from this kind of data.

### Dataset Description and Nature of Dataset
Dataset: 
 - multichoiceresponses.csv : This consists of responses to various questions by various people.
 - schema.csv : This consists of the questions corresponding to various columns within the multichoiceresponces.csv file.
Source: Kaggle - https://www.kaggle.com/kaggle/kaggle-survey-2017

### Import and Preprocessing the data

```R
df<-read.csv("/home/reen/Desktop/Kaggle-DataScience/multipleChoiceResponses.csv",header=TRUE,sep=",")
```
In order to find out the number of rows and columns in a dataset and what kind of columns are there in the dataset, we run the following command.
,here I have displayed just 25 of them.There were total 228.

```R
nrow(df)
ncols(df)
names(df)
```

```
16716 (rows)
228 (cols)

First 25 columns

[1] "GenderSelect"                               
  [2] "Country"                                    
  [3] "Age"                                        
  [4] "EmploymentStatus"                           
  [5] "StudentStatus"                              
  [6] "LearningDataScience"                        
  [7] "CodeWriter"                                 
  [8] "CareerSwitcher"                             
  [9] "CurrentJobTitleSelect"                      
 [10] "TitleFit"                                   
 [11] "CurrentEmployerType"                        
 [12] "MLToolNextYearSelect"                       
 [13] "MLMethodNextYearSelect"                     
 [14] "LanguageRecommendationSelect"               
 [15] "PublicDatasetsSelect"                       
 [16] "LearningPlatformSelect"                     
 [17] "LearningPlatformUsefulnessArxiv"            
 [18] "LearningPlatformUsefulnessBlogs"            
 [19] "LearningPlatformUsefulnessCollege"          
 [20] "LearningPlatformUsefulnessCompany"          
 [21] "LearningPlatformUsefulnessConferences"      
 [22] "LearningPlatformUsefulnessFriends"          
 [23] "LearningPlatformUsefulnessKaggle"           
 [24] "LearningPlatformUsefulnessNewsletters"      
 [25] "LearningPlatformUsefulnessCommunities"
```

### Distribution of Male , female, Non-Binary gender conributors from various countries across the globe.
The idea is to analyse the number of kagglers varying across different gender categoories as well as countries. Here we
can clearly see that , people having a different identity or non-binary gender or oher are much less into data science as
compared to men and women.

```
df_country_gender = subset(df, ,select = c("Country","GenderSelect"))
table_country_gender = with(df_country_gender,table(Country,GenderSelect))
results_male=c()
results_female=c()
results_diff=c()
results_non_binary=c()
for(row in 2:nrow(table_country_gender)) {
  for(col in 2:ncol(table_country_gender)) {
      if(col==2){
        #print(table_country_gender[row,col])
        results_diff<<-append(results_diff,table_country_gender[row,col])
        #print(results_diff)
      }
      if(col==3){
        results_female<<-append(results_female,table_country_gender[row,col])
      }
      if(col==4){
        results_male<<-append(results_male,table_country_gender[row,col])
      }
      if(col==5){
        results_non_binary<<-append(results_non_binary,table_country_gender[row,col])
      }
    }
}

main<-append(results_diff,results_female)
main<-append(main,results_male)
main<-append(main,results_non_binary)

df2<-as.data.frame(table(df$Country))
x<-levels(df2$Var1)
x<-x[-1]
country_names<-c(x)
graph_country_wise<- data.frame(gender=rep(c("A different Identity", "Female","Male","NonBinary or Other"), each=52),countries=country_names,
                    Number=main)
graph_country_gender<-ggplot(data=graph_country_wise, aes(x=countries, y=Number, fill=gender)) +
  geom_bar(stat="identity") + theme(text = element_text(size=8),axis.text.x = element_text(angle=90, hjust=1)) 
```

![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/plot_countries_gender.png)


### Distribution of employment status of various kagglers in relation to their gender

```R
df_employment = subset(df,,select=c("EmploymentStatus","GenderSelect"))
table_employment_gender = with(df_employment,table(EmploymentStatus,GenderSelect))

results_male=c()
results_female=c()
results_diff=c()
results_non_binary=c()
for(row in 2:nrow(table_employment_gender)) {
  for(col in 2:ncol(table_employment_gender)) {
    if(col==2){
      #print(table_country_gender[row,col])
      results_diff<<-append(results_diff,table_employment_gender[row,col])
      #print(results_diff)
    }
    if(col==3){
      results_female<<-append(results_female,table_employment_gender[row,col])
    }
    if(col==4){
      results_male<<-append(results_male,table_employment_gender[row,col])
    }
    if(col==5){
      results_non_binary<<-append(results_non_binary,table_employment_gender[row,col])
    }
  }
}

main<-c()
main<-append(results_diff,results_female)
main<-append(main,results_male)
main<-append(main,results_non_binary)

df2<-as.data.frame(table(df$EmploymentStatus))
x<-levels(df2$Var1)
x<-x[-1]
emp_status_names<-c(x)
graph_country_wise<- data.frame(gender=rep(c("A different Identity", "Female","Male","NonBinary or Other"), each=6),emp_status=emp_status_names,
                                Number=main)
graph_country_gender<-ggplot(data=graph_country_wise, aes(x=emp_status, y=Number, fill=gender)) +
  geom_bar(stat="identity") + theme(text = element_text(size=9),axis.text.x = element_text(angle=90, hjust=1)) 
```
![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/plot_empstatus_gender.png)

#### Age of kagglers around the world
```R
df_age<-subset(df, ,select=c("Age","GenderSelect"))
age_table<-with(df_age,table(Age,GenderSelect))

results_male=c()
results_female=c()
results_diff=c()
results_non_binary=c()
for(row in 2:nrow(age_table)) {
  for(col in 2:ncol(age_table)) {
    if(col==2){
      #print(table_country_gender[row,col])
      results_diff<<-append(results_diff,age_table[row,col])
      #print(results_diff)
    }
    if(col==3){
      results_female<<-append(results_female,age_table[row,col])
    }
    if(col==4){
      results_male<<-append(results_male,age_table[row,col])
    }
    if(col==5){
      results_non_binary<<-append(results_non_binary,age_table[row,col])
    }
  }
}

main<-c()
main<-append(results_diff,results_female)
main<-append(main,results_male)
main<-append(main,results_non_binary)

df2<-as.data.frame(table(df$Age))
x<-levels(df2$Var1)
x<-x[-1]
age_names<-c(x)
graph_age<- data.frame(gender=rep(c("A different Identity", "Female","Male","NonBinary or Other"), each=83),age=age_names,
                                Number=main)
graph_Age<-ggplot(data=graph_age, aes(x=age, y=Number, fill=gender)) +
  geom_bar(stat="identity") + theme(text = element_text(size=9),axis.text.x = element_text(angle=90, hjust=1)) 
  ```
![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/plot_age_kagglers.png)


#### Title fit
Analysisng the title fit.

```R
df_lang <- df %>% filter(GenderSelect %in% c("Male","Female") & Age>=14 & TitleFit!="" & LanguageRecommendationSelect %in% c("R","Python","SQL","Java","C/C++/C#"))%>%droplevels()
new_df_lang <- df_lang[,c(1,2,3,10,14)]

ggplot(new_df_lang,aes(x=Age,fill=TitleFit))+
  geom_histogram(bins=20)+
  facet_wrap(~LanguageRecommendationSelect) + scale_fill_brewer(palette="Blues")

```
![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/plot_title_fit.png)


#### Job Title word Cloud
Word cloud of the prevalent job titles of people

```R
#Job title word cloud
job_titles = subset(df, ,select=c("CurrentJobTitleSelect"))
job_title<-with(job_titles,table(CurrentJobTitleSelect))
df2<-as.data.frame(job_title)
x<-levels(df2$CurrentJobTitleSelect)
x<-x[-1]
title_names<-c(x)

library(wordcloud)
library(tm)
library(RColorBrewer)

x<-wordcloud(words = title_names, freq = df2$Freq, max.words =100,min.freq=20,scale=c(4,.5), 
             random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
```

![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/Rplot02_wordcloud.png)


#### Gender wise programming language preferences

```R
#Gender wise programming language preferences

prolang<-subset(df, ,select=c("GenderSelect","LanguageRecommendationSelect"))
lang_table<-with(prolang,table(LanguageRecommendationSelect,GenderSelect))

results_male=c()
results_female=c()
results_diff=c()
results_non_binary=c()
for(row in 2:nrow(lang_table)) {
  for(col in 2:ncol(lang_table)) {
    if(col==2){
      #print(table_country_gender[row,col])
      results_diff<<-append(results_diff,lang_table[row,col])
      #print(results_diff)
    }
    if(col==3){
      results_female<<-append(results_female,lang_table[row,col])
    }
    if(col==4){
      results_male<<-append(results_male,lang_table[row,col])
    }
    if(col==5){
      results_non_binary<<-append(results_non_binary,lang_table[row,col])
    }
  }
}

main<-c()
main<-append(results_diff,results_female)
main<-append(main,results_male)
main<-append(main,results_non_binary)

df2<-as.data.frame(table(df$LanguageRecommendationSelect))
x<-levels(df2$Var1)
x<-x[-1]
lang_names<-c(x)
graph_lang<- data.frame(gender=rep(c("A different Identity", "Female","Male","NonBinary or Other"), each=13),lang=lang_names,
                       Number=main)
graph_Lang<-ggplot(data=graph_lang, aes(x=lang, y=Number, fill=gender)) +
  geom_bar(stat="identity",position=position_dodge()) + theme(text = element_text(size=9),axis.text.x = element_text(angle=90, hjust=1)) 
```
![altText](https://github.com/avneet14027/Kaggle-DataScience-Survey-Analysis/blob/master/plot_language.png)
