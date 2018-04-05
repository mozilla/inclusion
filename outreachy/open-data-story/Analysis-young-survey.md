## Analysis Of Young People Survey - Part 1
Source of data: https://www.kaggle.com/miroslavsabo/young-people-survey<br>
Programming Language: R<br>
Tools: RStudio<br>
The young people survey is a dataset released by kaggle about their hobbies, interests, certain demographic factors, educatinal
quaalification of todays youth, movie and music preferences, personal feelings, personal choices on life sruggles, happiness index etc.
The goal of this analysis is to ponder upon the nature of today's youth who are going to be leaders of tomorrow. Also, the reason
I chose this dataset is to know the general trend among todays youth in terms of music and movie preferences and
analysisng social quality of life based on personal factors and decisions. Also, this dataset gives us an insight into
the gender gaps within various aspects of life that need to be filled by us today. 

This analysis would be carried on as a 3 part series of which this is just the first part. Initially, Ill do some basic data
analysis and visualisation using R and further on dig deeper to gain more insights into the data. So lets get started!

#### Dataset nature , import and preprocess

```R
df<-read.csv("/home/reen/Desktop/young-people-survey/responses.csv",header=TRUE,sep=",",na.strings="NA")
df[df==""] <- NA
df <- na.omit(df)
```

#### Analysis of music preferences - taste and style within todays youth

A bubble chart was used to analyse the music preferences among the youth. It is catogrized according to gender and the 
size indicated the number of people who chose a particular rating. 

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/music1.png)
<br>
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/music2.png)


```R
#df for types of music, gender, age
music= subset(df, , select = c("Music","Slow.songs.or.fast.songs","Dance","Folk","Country","Classical.music","Musical","Pop",	"Rock",	"Metal.or.Hardrock","Punk",	"Hiphop..Rap",	"Reggae..Ska",	"Swing..Jazz","Rock.n.roll",	"Alternative",	"Latino",	"Techno..Trance",	"Opera","Age","Gender"))
music[music==""] <- NA
music <- na.omit(music)
Music<-with(music,table(Music,Gender))
Music<-as.data.frame(as.table(Music))
Music[Music==""] <- NA
Music <- na.omit(Music)
#Music<-as.matrix(Music)
#xm <- melt(Music, id.vars = "Music", variable.name="Gender", value.name = "Number")
#str(xm)
music_plot<-ggplot(Music, aes(x = Music, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
```R 
#Tempo Songs - whether fast or slow is preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
TempoSongs<-with(music,table(Slow.songs.or.fast.songs,Gender))
TempoSongs<-as.data.frame(as.table(TempoSongs))
TempoSongs[TempoSongs==""] <- NA
TempoSongs <- na.omit(TempoSongs)
TempoSongs_plot<-ggplot(TempoSongs, aes(x = Slow.songs.or.fast.songs, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Dance- whether sngs which you can dance to is preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Dance<-with(music,table(Dance,Gender))
Dance<-as.data.frame(as.table(Dance))
Dance[Dance==""] <- NA
Dance <- na.omit(Dance)
Dance_plot<-ggplot(Dance, aes(x = Dance, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
whether flk songs are still poular or even exist within today's youth's playlist on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
#Folk
Folk<-with(music,table(Folk,Gender))
Folk<-as.data.frame(as.table(Folk))
Folk[Folk==""] <- NA
Folk <- na.omit(Folk)
Folk_plot<-ggplot(Folk, aes(x = Folk, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
whether today's youth is still a fan of country music on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
#Country
Country<-with(music,table(Country,Gender))
Country<-as.data.frame(as.table(Country))
Country[Country==""] <- NA
Country <- na.omit(Country)
Country_plot<-ggplot(Country, aes(x = Country, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
whether classic music is known by youth and  preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
#Classical_music
Classical_music<-with(music,table(Classical.music,Gender))
Classical_music<-as.data.frame(as.table(Classical_music))
Classical_music[Classical_music==""] <- NA
Classical_music<- na.omit(Classical_music)
Classical_music_plot<-ggplot(Classical_music, aes(x = Classical.music, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
whether musicals preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
#Musical
Musical<-with(music,table(Musical,Gender))
Musical<-as.data.frame(as.table(Musical))
Musical[Musical==""] <- NA
Musical <- na.omit(Musical)
Musical_plot<-ggplot(Musical, aes(x = Musical, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Pop
whether pop music is on the youths mind on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Pop<-with(music,table(Pop,Gender))
Pop<-as.data.frame(as.table(Pop))
Pop[Pop==""] <- NA
Pop <- na.omit(Pop)
Pop_plot<-ggplot(Pop, aes(x = Pop, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Rock
whether they are a fan of rock and roll? on a scale of 1-5. I vote 10 :P .The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Rock<-with(music,table(Rock,Gender))
Rock<-as.data.frame(as.table(Rock))
Rock[Rock==""] <- NA
Rock <- na.omit(Rock)
Rock_plot<-ggplot(Rock, aes(x = Rock, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Metal or Hardrock - tough choices:P
whether Metal or Hardrock are preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Metal_or_Hardrock<-with(music,table(Metal.or.Hardrock,Gender))
Metal_or_Hardrock<-as.data.frame(as.table(Metal_or_Hardrock))
Metal_or_Hardrock[Metal_or_Hardrock==""] <- NA
Metal_or_Hardrock <- na.omit(Metal_or_Hardrock)
Metal_or_Hardrock_plot<-ggplot(Metal_or_Hardrock, aes(x = Metal.or.Hardrock, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Punk
whether Punk music is preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Punk<-with(music,table(Punk,Gender))
Punk<-as.data.frame(as.table(Punk))
Punk[Punk==""] <- NA
Punk <- na.omit(Punk)
Punk_plot<-ggplot(Punk, aes(x = Punk, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
```R
#Hiphop_Rap
whether Hiphop_Rap music is preferred on a scale of 1-5. The idea is to make a bubble chart according to 
gender preferences, so freuency table is made for the same and plotted as a bubble chart.
Hiphop_Rap<-with(music,table(Hiphop..Rap,Gender))
Hiphop_Rap<-as.data.frame(as.table(Hiphop_Rap))
Hiphop_Rap[Hiphop_Rap==""] <- NA
Hiphop_Rap <- na.omit(Hiphop_Rap)
Hiphop_Rap_plot<-ggplot(Hiphop_Rap, aes(x = Hiphop..Rap, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Reggae_Ska
Reggae_Ska<-with(music,table(Reggae..Ska,Gender))
Reggae_Ska<-as.data.frame(as.table(Reggae_Ska))
Reggae_Ska[Reggae_Ska==""] <- NA
Reggae_Ska <- na.omit(Reggae_Ska)
Reggae_Ska_plot<-ggplot(Reggae_Ska, aes(x = Reggae..Ska, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Swing_Jazz
Swing_Jazz<-with(music,table(Swing..Jazz,Gender))
Swing_Jazz<-as.data.frame(as.table(Swing_Jazz))
Swing_Jazz[Swing_Jazz==""] <- NA
Swing_Jazz <- na.omit(Swing_Jazz)
Swing_Jazz_plot<-ggplot(Swing_Jazz, aes(x = Swing..Jazz, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Rock n roll
Rock_n_roll<-with(music,table(Rock.n.roll,Gender))
Rock_n_roll<-as.data.frame(as.table(Rock_n_roll))
Rock_n_roll[Rock_n_roll==""] <- NA
Rock_n_roll <- na.omit(Rock_n_roll)
Rock_n_roll_plot<-ggplot(Rock_n_roll, aes(x = Rock.n.roll, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
```R
#Alternative
Alternative<-with(music,table(Alternative,Gender))
Alternative<-as.data.frame(as.table(Alternative))
Alternative[Alternative==""] <- NA
Alternative <- na.omit(Alternative)
Alternative_plot<-ggplot(Alternative, aes(x = Alternative, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```

```R
#Latino
Latino<-with(music,table(Latino,Gender))
Latino<-as.data.frame(as.table(Latino))
Latino[Latino==""] <- NA
Latino <- na.omit(Latino)
Latino_plot<-ggplot(Latino, aes(x = Latino, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
```R
#Opera
Opera<-with(music,table(Opera,Gender))
Opera<-as.data.frame(as.table(Opera))
Opera[Opera==""] <- NA
Opera <- na.omit(Opera)
Opera_plot<-ggplot(Opera, aes(x = Opera, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
```R
#Techno_Trance
Techno_Trance<-with(music,table(Techno..Trance,Gender))
Techno_Trance<-as.data.frame(as.table(Techno_Trance))
Techno_Trance[Techno_Trance==""] <- NA
Techno_Trance <- na.omit(Techno_Trance)
Techno_Trance_plot<-ggplot(Techno_Trance, aes(x = Techno..Trance, y = Gender)) +
  geom_point(aes(size = Freq),color="#56B4E9")
```
#### Habitat - Rural vs Urban living categorized by gender

Next, I wanted to find out how much of the youth categorized by gender is actually living in urban vs rural areas.

```R
habitat_gender <- subset(df, , select = c("Gender","Village...town","Age"))
habitat_gender[habitat_gender==""] <- NA
habitat_gender <- na.omit(habitat_gender)
habitat<-with(habitat_gender,table(Village...town,Gender))
habitat_df <-data.frame(supp=rep(c("female","male"),each=2),type_habitat=c("city","village"),Number=c(habitat[2,2],habitat[3,2],habitat[2,3],habitat[3,3]))
habitat_plot<-ggplot(data=habitat_df, aes(x=type_habitat, y=Number,fill=supp)) +
  geom_bar(stat="identity",position=position_dodge())
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/habitat.png)

```R
#### Age group of people living in urban vs rural areas
habitat_age<-with(habitat_gender,table(Village...town,Age))
habitat_age_df <-data.frame(supp=rep(c("city","village"),each=16),type_age=c("15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"),Number=c(habitat_age[2,1],habitat_age[3,1],habitat_age[2,2],habitat_age[3,2],habitat_age[2,3],habitat_age[3,3],habitat_age[2,4],habitat_age[3,4],habitat_age[2,5],habitat_age[3,5],habitat_age[2,6],habitat_age[3,6],habitat_age[2,7],habitat_age[3,7],habitat_age[2,8],habitat_age[3,8],habitat_age[2,9],habitat_age[3,9],habitat_age[2,10],habitat_age[3,10],habitat_age[2,11],habitat_age[3,11],habitat_age[2,12],habitat_age[3,12],habitat_age[2,13],habitat_age[3,13],habitat_age[2,14],habitat_age[3,14],habitat_age[2,15],habitat_age[3,15],habitat_age[2,16],habitat_age[3,16]))
habitat_age_plot<-ggplot(data=habitat_age_df, aes(x=type_age, y=Number,fill=supp)) +
  geom_bar(stat="identity",position=position_dodge())
```

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/age_habitat.png)

#### Life of people living in urban vs rural areas

Here I wanted to analyse certain factors that show the quality of urban vs rural living like education, smoking, consuming alcohol, healthy eating etc.

```R
urban_rural<-subset(df, , select = c("Gender","Age","Village...town", "Smoking","Alcohol","Healthy.eating","Eating.to.survive","Health","Socializing","Happiness.in.life","Life.struggles","Internet.usage","Education"))
```

##### #Smoking - on a sacle of 1-5, how much do people smoke, categorized by gender and habitat.
```R
#Smoking
smoking_village_town_gender<-with(urban_rural,table(Village...town,Smoking,Gender))

test  <- data.frame(level=c("current smoker", "former smoker", "never smoked", "tried smoking"), 
                    city_men=c(smoking_village_town_gender[2,2,3],smoking_village_town_gender[2,3,3],smoking_village_town_gender[2,4,3],smoking_village_town_gender[2,5,3]),     
                    city_women=c(smoking_village_town_gender[2,2,2],smoking_village_town_gender[2,3,2],smoking_village_town_gender[2,4,2],smoking_village_town_gender[2,5,2]) , 
                    village_men=c(smoking_village_town_gender[3,2,3],smoking_village_town_gender[3,3,3],smoking_village_town_gender[3,4,3],smoking_village_town_gender[3,5,3]),
                    village_women=c(smoking_village_town_gender[3,2,2],smoking_village_town_gender[3,3,2],smoking_village_town_gender[3,4,2],smoking_village_town_gender[3,5,2])) 

melted <- melt(test, "level")
melted$cat <- ''
melted[melted$variable != 'village_men' & melted$variable != 'village_women' ,]$cat <- "city"
melted[melted$variable != 'city_men' & melted$variable != 'city_women' ,]$cat <- "village"
ggplot(melted, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level)
```

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/smoking_gender.png)


##### Alcohol: Categorized by gender and habitat.

```R
#Alcohol
alcohol_village_town_gender<-with(urban_rural,table(Village...town,Alcohol,Gender))
alcohol_df  <- data.frame(level=c("drink a lot", "never", "social drinker"), 
                    city_men=c(alcohol_village_town_gender[2,2,3],alcohol_village_town_gender[2,3,3],alcohol_village_town_gender[2,4,3]),     
                    city_women=c(alcohol_village_town_gender[2,2,2],alcohol_village_town_gender[2,3,2],alcohol_village_town_gender[2,4,2]) , 
                    village_men=c(alcohol_village_town_gender[3,2,3],alcohol_village_town_gender[3,3,3],alcohol_village_town_gender[3,4,3]),
                    village_women=c(alcohol_village_town_gender[3,2,2],alcohol_village_town_gender[3,3,2],alcohol_village_town_gender[3,4,2])) 
melted_alcohol <- melt(test, "level")
melted_alcohol$cat <- ''
melted_alcohol[melted_alcohol$variable != 'village_men' & melted_alcohol$variable != 'village_women' ,]$cat <- "city"
melted_alcohol[melted_alcohol$variable != 'city_men' & melted_alcohol$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_alcohol, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "purple1", "coral", "deepskyblue1")) + facet_grid(~ level)
```

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/alcohol_gender.png)


##### Healthy Eating in villages vs cities categorized by gender.

```R
#Healthy.eating
healthy_village_town_gender<-with(urban_rural,table(Village...town,Healthy.eating,Gender))
test_healthy  <- data.frame(level=c("1","2","3","4","5"), 
                    city_men=c(healthy_village_town_gender[2,1,3],healthy_village_town_gender[2,2,3],healthy_village_town_gender[2,3,3],healthy_village_town_gender[2,4,3],healthy_village_town_gender[2,5,3]),     
                    city_women=c(healthy_village_town_gender[2,1,2],healthy_village_town_gender[2,2,2],healthy_village_town_gender[2,3,2],healthy_village_town_gender[2,4,2],healthy_village_town_gender[2,5,2]) , 
                    village_men=c(healthy_village_town_gender[3,1,3],healthy_village_town_gender[3,2,3],healthy_village_town_gender[3,3,3],healthy_village_town_gender[3,4,3],healthy_village_town_gender[3,5,3]),
                    village_women=c(healthy_village_town_gender[3,1,2],healthy_village_town_gender[3,2,2],healthy_village_town_gender[3,3,2],healthy_village_town_gender[3,4,2],healthy_village_town_gender[3,5,2])) 
melted_healthy <- melt(test_healthy, "level")
melted_healthy$cat <- ''
melted_healthy[melted_healthy$variable != 'village_men' & melted_healthy$variable != 'village_women' ,]$cat <- "city"
melted_healthy[melted_healthy$variable != 'city_men' & melted_healthy$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_healthy, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level)
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/healthy_gender.png)


##### Eating To Survive
```R
Survive_village_town_gender<-with(urban_rural,table(Village...town,Eating.to.survive,Gender))
test_survive<-data.frame(level=c("1","2","3","4","5"),city_men=c(Survive_village_town_gender[2,1,3],Survive_village_town_gender[2,2,3],Survive_village_town_gender[2,3,3],Survive_village_town_gender[2,4,3],Survive_village_town_gender[2,5,3]),city_women=c(Survive_village_town_gender[2,1,2],Survive_village_town_gender[2,2,2],Survive_village_town_gender[2,3,2],Survive_village_town_gender[2,4,2],Survive_village_town_gender[2,5,2]),village_men=c(Survive_village_town_gender[3,1,3],Survive_village_town_gender[3,2,3],Survive_village_town_gender[3,3,3],Survive_village_town_gender[3,4,3],Survive_village_town_gender[3,5,3]),                        village_women=c(Survive_village_town_gender[3,1,2],Survive_village_town_gender[3,2,2],Survive_village_town_gender[3,3,2],Survive_village_town_gender[3,4,2],Survive_village_town_gender[3,5,2])) 

melted_survive <- melt(test_survive, "level")
melted_survive$cat <- ''
melted_survive[melted_survive$variable != 'village_men' & melted_survive$variable != 'village_women' ,]$cat <- "city"
melted_survive[melted_survive$variable != 'city_men' & melted_survive$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_survive, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/survive.png)


###### Socializing

```R
Socializing_village_town_gender<-with(urban_rural,table(Village...town,Socializing,Gender))
test_Socializing  <- data.frame(level=c("1","2","3","4","5"),                            city_men=c(Socializing_village_town_gender[2,1,3],Socializing_village_town_gender[2,2,3],Socializing_village_town_gender[2,3,3],Socializing_village_town_gender[2,4,3],Socializing_village_town_gender[2,5,3]),                          city_women=c(Socializing_village_town_gender[2,1,2],Socializing_village_town_gender[2,2,2],Socializing_village_town_gender[2,3,2],Socializing_village_town_gender[2,4,2],Socializing_village_town_gender[2,5,2]) ,                            village_men=c(Socializing_village_town_gender[3,1,3],Socializing_village_town_gender[3,2,3],Socializing_village_town_gender[3,3,3],Socializing_village_town_gender[3,4,3],Socializing_village_town_gender[3,5,3]),                           village_women=c(Socializing_village_town_gender[3,1,2],Socializing_village_town_gender[3,2,2],Socializing_village_town_gender[3,3,2],Socializing_village_town_gender[3,4,2],Socializing_village_town_gender[3,5,2])) 

melted_Socializing <- melt(test_Socializing, "level")
melted_Socializing$cat <- ''
melted_Socializing[melted_Socializing$variable != 'village_men' & melted_Socializing$variable != 'village_women' ,]$cat <- "city"
melted_Socializing[melted_Socializing$variable != 'city_men' & melted_Socializing$variable != 'city_women' ,]$cat<-"village"
ggplot(melted_Socializing, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/socializing.png)


###### Happiness.in.life
```R
happy_village_town_gender<-with(urban_rural,table(Village...town,Happiness.in.life,Gender))

test_happy<-data.frame(level=c("1","2","3","4","5"),city_men=c(happy_village_town_gender[2,1,3],happy_village_town_gender[2,2,3],happy_village_town_gender[2,3,3],happy_village_town_gender[2,4,3],happy_village_town_gender[2,5,3]),                                     city_women=c(happy_village_town_gender[2,1,2],happy_village_town_gender[2,2,2],happy_village_town_gender[2,3,2],happy_village_town_gender[2,4,2],happy_village_town_gender[2,5,2]) ,                                 village_men=c(happy_village_town_gender[3,1,3],happy_village_town_gender[3,2,3],happy_village_town_gender[3,3,3],happy_village_town_gender[3,4,3],happy_village_town_gender[3,5,3]),                               village_women=c(happy_village_town_gender[3,1,2],happy_village_town_gender[3,2,2],happy_village_town_gender[3,3,2],happy_village_town_gender[3,4,2],happy_village_town_gender[3,5,2])) 

melted_happy <- melt(test_happy, "level")
melted_happy$cat <- ''
melted_happy[melted_happy$variable != 'village_men' & melted_happy$variable != 'village_women' ,]$cat <- "city"
melted_happy[melted_happy$variable != 'city_men' & melted_happy$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_happy, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/happy.png)


###### Life.struggles
```R
struggle_village_town_gender<-with(urban_rural,table(Village...town,Life.struggles,Gender))

test_struggle  <- data.frame(level=c("1","2","3","4","5"), city_men=c(struggle_village_town_gender[2,1,3],struggle_village_town_gender[2,2,3],struggle_village_town_gender[2,3,3],struggle_village_town_gender[2,4,3],struggle_village_town_gender[2,5,3]),     
city_women=c(struggle_village_town_gender[2,1,2],struggle_village_town_gender[2,2,2],struggle_village_town_gender[2,3,2],struggle_village_town_gender[2,4,2],struggle_village_town_gender[2,5,2]) , 
village_men=c(struggle_village_town_gender[3,1,3],struggle_village_town_gender[3,2,3],struggle_village_town_gender[3,3,3],struggle_village_town_gender[3,4,3],struggle_village_town_gender[3,5,3]),
village_women=c(struggle_village_town_gender[3,1,2],struggle_village_town_gender[3,2,2],struggle_village_town_gender[3,3,2],struggle_village_town_gender[3,4,2],struggle_village_town_gender[3,5,2])) 

melted_struggle <- melt(test_struggle, "level")
melted_struggle$cat <- ''
melted_struggle[melted_struggle$variable != 'village_men' & melted_struggle$variable != 'village_women' ,]$cat <- "city"
melted_struggle[melted_struggle$variable != 'city_men' & melted_struggle$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_struggle, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```

![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/struggle.png)

###### Internet.usage
```R
internet_village_town_gender<-with(urban_rural,table(Village...town,Internet.usage,Gender))

test_internet  <- data.frame(level=c("few hours a day","less than an hour a day","most of the day","no time at all"),
city_men=c(internet_village_town_gender[2,1,3],internet_village_town_gender[2,2,3],internet_village_town_gender[2,3,3],internet_village_town_gender[2,4,3]),
city_women=c(internet_village_town_gender[2,1,2],internet_village_town_gender[2,2,2],internet_village_town_gender[2,3,2],internet_village_town_gender[2,4,2]) , 
village_men=c(internet_village_town_gender[3,1,3],internet_village_town_gender[3,2,3],internet_village_town_gender[3,3,3],internet_village_town_gender[3,4,3]),
village_women=c(internet_village_town_gender[3,1,2],internet_village_town_gender[3,2,2],internet_village_town_gender[3,3,2],internet_village_town_gender[3,4,2])) 

melted_internet <- melt(test_internet, "level")
melted_internet$cat <- ''
melted_internet[melted_internet$variable != 'village_men' & melted_internet$variable != 'village_women' ,]$cat <- "city"
melted_internet[melted_internet$variable != 'city_men' & melted_internet$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_internet, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/internet.png)


###### Education
```R
Education_village_town_gender<-with(urban_rural,table(Village...town,Education,Gender))

test_Education  <- data.frame(level=c("college/bachelor degree","currently a primary school pupil","doctorate degree"," masters degree","primary school","secondary school"),  city_men=c(Education_village_town_gender[2,1,3],Education_village_town_gender[2,2,3],Education_village_town_gender[2,3,3],Education_village_town_gender[2,4,3],Education_village_town_gender[2,5,3],Education_village_town_gender[2,6,3]),     city_women=c(Education_village_town_gender[2,1,2],Education_village_town_gender[2,2,2],Education_village_town_gender[2,3,2],Education_village_town_gender[2,4,2],Education_village_town_gender[2,5,2],Education_village_town_gender[2,6,2]) ,village_men=c(Education_village_town_gender[3,1,3],Education_village_town_gender[3,2,3],Education_village_town_gender[3,3,3],Education_village_town_gender[3,4,3],Education_village_town_gender[3,5,3],Education_village_town_gender[3,6,3]),village_women=c(Education_village_town_gender[3,1,2],Education_village_town_gender[3,2,2],Education_village_town_gender[3,3,2],Education_village_town_gender[3,4,2],Education_village_town_gender[3,5,2],Education_village_town_gender[3,6,3])) 

melted_Education <- melt(test_Education, "level")
melted_Education$cat <- ''
melted_Education[melted_Education$variable != 'village_men' & melted_Education$variable != 'village_women' ,]$cat <- "city"
melted_Education[melted_Education$variable != 'city_men' & melted_Education$variable != 'city_women' ,]$cat <- "village"
ggplot(melted_Education, aes(x = cat, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ level) + scale_fill_hue(l=70) + scale_fill_manual(values=c("seagreen2", "slateblue1", "coral", "deepskyblue1")) + facet_grid(~ level)
```
![altText](https://github.com/avneet14027/Young-People-Survey/blob/master/education.png)
