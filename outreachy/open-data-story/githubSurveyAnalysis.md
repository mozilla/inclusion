## Analysis of an individual's participation and contributions to open source and factors affecting them. 
The Github Survey Data 2017 consisits of questions and responses asked to a particular user regarding open source contributions
and participations. The survey responses are in a csv file format with 93 variables and user responses from 6029 people.
Here, I have made an attempt to analyse and understand factors affecting an individual's contribution to open source and learn some ways to analyse given. This 
analysis had been done using R.

#### Shape of Data 
Firstly, in order to understand the nature of data, the csv file was loaded into R and number of rows and columns were found.
Further, head() command was used to see the first few rows and get an idea about various attributes.

```R
library(ggplot2) #for plots 
library(reshape2) #for reshaing data 
library(chorddiag) #chord diagram library
df <- read.csv("/home/reen/Downloads/data_for_public_release/data_for_public_release/survey_data.csv",header = TRUE,na.strings=" ",sep = ",")
#dataframe df, header=True implies that the first row of the csv file is to be taken as header for various columns.
rows<-nrow(df) #number of rows
cols<-ncol(df) #number of columns
head(df) #outputs the first 6 rows of data and all columns(attributes) associated with it

Output:
> rows
[1] 6029
> cols
[1] 93
```

#### Distribution of participants categorized by gender
Here, I wanted to find the gender wise participation for each category including Man, Non-Binary or Other, refer Not to Say
and Woman. I used a pie chart to visualise this data(below).

```R
gender_part<-(df[-which(df$GENDER == ""), ]) #Select rows without blanks in gender
gender_part<-gender_part['GENDER'] #Make a column for only the gender variable
table(gender_part) #Outputs the frequency for each category.
#make a pie chart using pie command
pie(table(gender_part), labels = paste(round(prop.table(table(gender_part))*100), "%", sep = ""), 
    col = heat.colors(5), main = "distribution of gender of participators") 
#label the pie chart and various categories using legend
legend("topright", legend = c("-", "Man", "Non Binary/other", "Prefer Not To Say", "Woman"), 
       fill = heat.colors(5), title = "Categories", cex = 0.5)
```

![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/pie.png)

#### Distribution of types of particpations done by various users
Users participated in various activities like following, applications,dependecies, contributing , or other. Here is a distribution for the
same which was analysed using a bar-graph.

```R
#subset a few columns from the larger dataframe for analysis
types_participations = subset(df, , select = c("PARTICIPATION.TYPE.FOLLOW", "PARTICIPATION.TYPE.USE.APPLICATIONS","PARTICIPATION.TYPE.USE.DEPENDENCIES","PARTICIPATION.TYPE.CONTRIBUTE","PARTICIPATION.TYPE.OTHER","GENDER"))

n_follow<-length(types_participations$PARTICIPATION.TYPE.FOLLOW[types_participations$PARTICIPATION.TYPE.FOLLOW == "1"]) #number of participants of type follow
n_applications<-length(types_participations$PARTICIPATION.TYPE.USE.APPLICATIONS[types_participations$PARTICIPATION.TYPE.USE.APPLICATIONS=="1"]) #number of participants of type appplications
n_dependencies<-length(types_participations$PARTICIPATION.TYPE.USE.DEPENDENCIES[types_participations$PARTICIPATION.TYPE.USE.DEPENDENCIES=="1"]) #number of participants of type dependencies
n_contribute<-length(types_participations$PARTICIPATION.TYPE.CONTRIBUTE[types_participations$PARTICIPATION.TYPE.CONTRIBUTE=="1"]) #number of participants of type contribute
n_other<-length(types_participations$PARTICIPATION.TYPE.OTHER[types_participations$PARTICIPATION.TYPE.OTHER=="1"]) #number of participants of type other

#plot for above
gdf <- data.frame(type_of_particiation=c("FOLLOW", "APPLICATIONS", "DEPENDENCIES","CONTRIBUTE","OTHER"),
                 Number=c(n_follow,n_applications,n_dependencies,n_contribute,n_other))
p<-ggplot(data=gdf, aes(x=type_of_particiation, y=Number)) + geom_bar(stat="identity",fill="steelblue") + geom_text(aes(label=Number), vjust=1.6, color="white", size=3.5)

```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/bar-graph.png)

#### Genderwise articipations data
Next I wanted to find out the genderwise participations in various types of data.

```R
follow_gender<-with(types_participations, table(PARTICIPATION.TYPE.FOLLOW, GENDER)) #Table for genderwise participation of type follow
gender_applications<-with(types_participations, table(PARTICIPATION.TYPE.USE.APPLICATIONS, GENDER)) #Table for genderwise participation of type applications
gender_dependencies<-with(types_participations, table(PARTICIPATION.TYPE.USE.DEPENDENCIES, GENDER)) #Table for genderwise participation of type dependencies
gender_contribute<-with(types_participations, table(PARTICIPATION.TYPE.CONTRIBUTE, GENDER)) #Table for genderwise participation of type contribute
gender_other<-with(types_participations, table(PARTICIPATION.TYPE.OTHER, GENDER)) #Table for genderwise participation of type other

#Gender wise contributions Plot
gPart <- data.frame(supp=rep(c("Men", "Women","Non-Binary or Other","Prefer_Not_To_Say"), each=5),type_of_particiation=c( "APPLICATIONS", "CONTRIBUTE","DEPENDENCIES","FOLLOW","OTHER"),Number=c(follow_gender[2,2],gender_applications[2,2],gender_contribute[2,2],gender_dependencies[2,2],gender_other[2,2],follow_gender[2,3],gender_applications[2,3],gender_contribute[2,3],gender_dependencies[2,3],gender_other[2,3],follow_gender[2,4],gender_applications[2,4],gender_contribute[2,4],gender_dependencies[2,4],gender_other[2,4],follow_gender[2,5],gender_applications[2,5],gender_contribute[2,5],gender_dependencies[2,5],gender_other[2,5]))

# Plot a grouped bar chart for a better understanding.
gpart<-ggplot(data=gPart, aes(x=type_of_particiation, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")
```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/genderwise-part.png)

We can clearly infer from the above graph that men participate the maximum in open source , whereas, particiation by other genders are almost negligible in comparison to men.

#### Intersections - People contributing to more than one type
Next, I used a chord diagram to represent how each individual makes more than one type of participation.
The thickness of each line represents the number of participants common between two types.
```R
A_C<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.CONTRIBUTE==1))
A_D<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.USE.DEPENDENCIES==1))
A_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.FOLLOW==1))
A_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.OTHER==1))
C_D<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.USE.DEPENDENCIES==1))
C_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.FOLLOW==1))
C_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.OTHER==1))
D_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.DEPENDENCIES==1 & PARTICIPATION.TYPE.FOLLOW==1))
D_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.DEPENDENCIES==1 & PARTICIPATION.TYPE.OTHER==1))
F_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.FOLLOW==1 & PARTICIPATION.TYPE.OTHER==1))

Table_chord = matrix(c(0,A_C, A_D, A_F, A_O, A_C,0,C_D, C_F,C_O, A_D, C_D, 0,D_F, D_O,A_F,C_F,D_F,0,F_O,A_O,C_O,D_O,F_O,0),nrow=5,ncol=5,byrow = TRUE)
dimnames(Table_chord) = list(c("Applications","Contributions","Dependencies","Follow","Other"), c("Applications","Contributions","Dependencies","Follow","Other"))
df2 = data.frame(from = rep(rownames(Table_chord), times = ncol(Table_chord)),
                to = rep(colnames(Table_chord), each = nrow(Table_chord)),
                value = as.vector(Table_chord),
                stringsAsFactors = FALSE)
#chordDiagram(Table_chord)
chorddiag(Table_chord, groupnamePadding = 20,showTicks=FALSE)

```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/chord.png)


The thickness of each line represents the number of participants common between two types.This is indicated below by a few examples.
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/ChordDiag.png)


#### Contributions and their frequenct vs gender
After participations, I wanted to analyse the various types of contributions made by various people of different gender categories and how frequently they are involved in contributions. For this, I again used a stacked bar chart for visualisation.

```R
#subsetting the original dataframe
types_contributions = subset(df, , select = c("CONTRIBUTOR.TYPE.CONTRIBUTE.CODE",	"CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS",	"CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE",	"CONTRIBUTOR.TYPE.FILE.BUGS",	"CONTRIBUTOR.TYPE.FEATURE.REQUESTS",	"CONTRIBUTOR.TYPE.COMMUNITY.ADMIN","GENDER","EMPLOYMENT.STATUS"))

#Creating tables
frequency_code<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.CODE, GENDER))
frequency_docs<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS, GENDER))
frequency_maintenance<-with(types_contributions, table(CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE, GENDER))
frequency_bugs<-with(types_contributions, table(CONTRIBUTOR.TYPE.FILE.BUGS, GENDER))
frequency_requests<-with(types_contributions, table(CONTRIBUTOR.TYPE.FEATURE.REQUESTS, GENDER))
frequency_admin<-with(types_contributions, table(CONTRIBUTOR.TYPE.COMMUNITY.ADMIN, GENDER))

#new dataframe with required columns
gContri_frequent <- data.frame(supp=rep(c("Men","Non-Binary or Other","Prefer_Not_To_Say","Women"), each=6),type_of_contribution=c("ADMIN","BUGS","CODE", "DOCS","REQUESTS","MAINTENANCE"),Number=c(frequency_admin[2,2],frequency_bugs[2,2],frequency_code[2,2],frequency_docs[2,2],frequency_maintenance[2,2],frequency_requests[2,2],frequency_admin[2,3],frequency_bugs[2,3],frequency_code[2,3],frequency_docs[2,3],frequency_maintenance[2,3],frequency_requests[2,3],frequency_admin[2,4],frequency_bugs[2,4],frequency_code[2,4],frequency_docs[2,4],frequency_maintenance[2,4],frequency_requests[2,4],frequency_admin[2,5],frequency_bugs[2,5],frequency_code[2,5],frequency_docs[2,5],frequency_maintenance[2,5],frequency_requests[2,5]))

#plotting the graph
gcontri_frequent<-ggplot(data=gContri_frequent, aes(x=type_of_contribution, y=Number, fill=supp)) +
geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")

```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/contri-freq.png)


#### Contributions Frequency vs Employment type?
Here , I wanted to find out how frequently does each individual contributes with resect to employment time? In other words, does being employed full time affect open source contributions? The results were infact quite surprising. People who are employed full time are infact contributing a lot.

```R
#Creating tables
emp_code<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.CODE, EMPLOYMENT.STATUS))
emp_docs<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS, EMPLOYMENT.STATUS))
emp_maintenance<-with(types_contributions, table(CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE, EMPLOYMENT.STATUS))
emp_bugs<-with(types_contributions, table(CONTRIBUTOR.TYPE.FILE.BUGS, EMPLOYMENT.STATUS))
emp_requests<-with(types_contributions, table(CONTRIBUTOR.TYPE.FEATURE.REQUESTS, EMPLOYMENT.STATUS))
emp_admin<-with(types_contributions, table(CONTRIBUTOR.TYPE.COMMUNITY.ADMIN, EMPLOYMENT.STATUS))

#new dataframe with required columns
gContri_emp <- data.frame(supp=rep(c("Employed full time", "Employed part time","Full time student Other","Other","Retired or permanently not working"), each=6),type_of_contribution=c("ADMIN","BUGS","CODE", "DOCS","REQUESTS","MAINTENANCE"),Number=c(emp_admin[2,2],emp_bugs[2,2],emp_code[2,2],emp_docs[2,2],emp_maintenance[2,2],emp_requests[2,2],emp_admin[2,3],emp_bugs[2,3],emp_code[2,3],emp_docs[2,3],emp_maintenance[2,3],emp_requests[2,3],emp_admin[2,4],emp_bugs[2,4],emp_code[2,4],emp_docs[2,4],emp_maintenance[2,4],emp_requests[2,4],emp_admin[2,5],emp_bugs[2,5],emp_code[2,5],emp_docs[2,5],emp_maintenance[2,5],emp_requests[2,5],emp_admin[2,6],emp_bugs[2,6],emp_code[2,6],emp_docs[2,6],emp_maintenance[2,6],emp_requests[2,6]))
    
#plot    
gcontri_emp<-ggplot(data=gContri_emp, aes(x=type_of_contribution, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/emp-contri.png)

#### Future contribution interest vs likelihood
Next, I wanted to analyse, how likely are people to contribute in the future according to their interest.

For this I experimented with a heatmap for visualisation. The lighter the colour, the more is the relationship.

```R
future_contribution <- subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
future_contribution[future_contribution==""] <- NA
likliness<-with(future_contribution,table(FUTURE.CONTRIBUTION.INTEREST,FUTURE.CONTRIBUTION.LIKELIHOOD))
likeliness_matrix <- data.matrix(likliness)
likeliness_matrix<-na.omit(likeliness_matrix)
likeliness_heatmap <- heatmap(likeliness_matrix, Rowv=NA, Colv=NA, col = heat.colors(5,0.5), scale="col", margins=c(10,10),cexRow = 1,cexCol = 1)
```

![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/heatmap.png)

#### Future contribution interest vs Gender
In this, I wanted to analyse, how interest in future contribution varies according to category of the gender. As we can clearly see men have a higher interest as compared to other gender categories like women and non-binary.

```R
#subset of dataframe
future_contribution<-subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
interest<-with(future_contribution,table(GENDER,FUTURE.CONTRIBUTION.INTEREST))

#new dataframe according to columns of interest
ginterest <- data.frame(supp=rep(c("Not at all interested", "Not too interested","Somewhat interested","Very interested"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),Number=c(interest[2,2],interest[3,2],interest[4,2],interest[5,2],interest[2,3],interest[3,3],interest[4,3],interest[5,3],interest[2,4],interest[3,4],interest[4,4],interest[5,4],interest[2,5],interest[3,5],interest[4,5],interest[5,5]))
#Plot                          
gInterest<-ggplot(data=ginterest, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")
```
![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/interest-gender.png)

#### Future contribution Likelihood vs Gender
In this, I wanted to analyse, how likelihood  in future contribution varies according to category of the gender. As we can clearly see men have a higher probability of contributing as compared to other gender categories like women and non-binary.

```R
#subsetting
future_contribution<-subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
likelihood<-with(future_contribution,table(GENDER,FUTURE.CONTRIBUTION.LIKELIHOOD))

#new dataframe
glikelihood <- data.frame(supp=rep(c("Not at all interested", "Not too interested","Somewhat interested","Very interested"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),Number=c(likelihood[2,2],likelihood[3,2],likelihood[4,2],likelihood[5,2],likelihood[2,3],likelihood[3,3],likelihood[4,3],likelihood[5,3],likelihood[2,4],likelihood[3,4],likelihood[4,4],likelihood[5,4],likelihood[2,5],likelihood[3,5],likelihood[4,5],likelihood[5,5]))

#plot
gLikelihood<-ggplot(data=glikelihood, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")

```

![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/likeli-gender.png.png)

#### Employment status vs gender
Lastly I wanted to analyse the status of employement across various gender categories. The role of men is mostly full time and in sharp contrast to the employment status of other gender categories.
```R
emp_gender <- subset(df, , select = c("GENDER","EMPLOYMENT.STATUS"))
g_emp_gender<-with(emp_gender,table(EMPLOYMENT.STATUS,GENDER))

G_emp_gender <- data.frame(supp=rep(c("Employed full time ", "Employed part time","Full time student","Other","Retired or permanently not working (e.g. due to disability)","Temporarily not working"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),Number=c(g_emp_gender[2,2],g_emp_gender[3,2],g_emp_gender[4,2],g_emp_gender[5,2],g_emp_gender[6,2],g_emp_gender[7,2],g_emp_gender[2,3],g_emp_gender[3,3],g_emp_gender[4,3],g_emp_gender[5,3],g_emp_gender[6,3],g_emp_gender[7,3],g_emp_gender[2,4],g_emp_gender[3,4],g_emp_gender[4,4],g_emp_gender[5,4],g_emp_gender[6,4],g_emp_gender[7,4],g_emp_gender[2,5],g_emp_gender[3,5],g_emp_gender[4,5],g_emp_gender[5,5],g_emp_gender[6,5],g_emp_gender[7,5]))
                          
G_Emp_Gender<-ggplot(data=G_emp_gender, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")
```

![alt text](https://github.com/avneet14027/Github-Survey-analysis/blob/master/emp-gender.png)






