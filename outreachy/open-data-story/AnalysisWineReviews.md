# Analysis of wine reviews
In this, I have analysed the wine reviews dataset from Kaggle. The main aim was to try out interesting visualisations
and representations of the data and experiment with various visualising tools in R.

#### Importing data
```R
library(dbplyr)
library(tm)
library(readr)
library(tidyverse)
library(knitr)
library(ggmap)
library(tidytext)
df <- read.csv("/home/reen/Desktop/WineReviews/wine-reviews/winemag-data_first150k.csv",header=TRUE,sep=",")
names(df)
```

#### Wine prducing provinces vs Number of labels of wines they produce
```R
country_wine_num <- df["country"]
barplot(table(country_wine_num),
        main="Country Wise Wine production",
        xlab="Country",
        ylab="Count",
        border="red",
        col="blue",
        density=10,cex.axis=0.2, cex.names=0.5,las=2)
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_country_wine.png)

#### Province wise wine production US

```R
province_wine_num <- subset(df$province,df[,2] == "US")
provinces_wine<-as.data.frame(table(province_wine_num))
provinces_wine<-subset(provinces_wine,provinces_wine[,2] > 0)
barplot(height=(provinces_wine$Freq),
        names.arg=provinces_wine$province_wine_num,
        main="province Wise Wine production in US",
        xlab="province",
        ylab="Count",
        border="red",
        col="blue",
        density=10,cex.axis=0.6, cex.names=0.6,las=2)
 
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_US_wines.png)


#### Price of wine vs points they score
```R
library(ggplot2)
# Basic scatter plot
ggplot(df, aes(x=df$points, y=df$price)) +
  geom_point(size=2, shape=23)
#Hex plot
ggplot(df, aes(df$points,y=df$price)) + 
  geom_hex() + scale_fill_gradient()
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_price_points.png)

![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_price_hex.png)

#### Type of scores given to 5 most cmmon types of wines
```R
wine_counts_df <- read.csv("/home/reen/Desktop/WineReviews/wine-reviews/top-five-wine-score-counts.csv",header=TRUE,sep=",")
require(reshape2)
molten = melt(wine_counts_df, id = c("points"))
#ggplot(molten, aes(x=molten$points,y=molten$value,group=molten$variable,fill=molten$variable)) + geom_area(position="fill")
area_chart<-ggplot(molten, aes(molten$points,molten$value)) + geom_area(aes(colour = molten$variable, fill= molten$variable), position = 'stack')   
```

![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_top5_points.png)

#### wine varieties in the US
```R
wine_varities<-subset(df$variety,df[,2] == "US")
wine_varities1<-as.data.frame(table(wine_varities))
wine_varities2<-subset(wine_varities1,wine_varities1[,2] > 100)
barplot(height=wine_varities2$Freq,
        names.arg = wine_varities2$wine_varities,
        main="Varities Of Wine",
        xlab="Variey",
        ylab="Count",
        border="red",
        col="blue",
        density=10,cex.axis=0.5, cex.names=0.5,las=2)
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/wine_varieties.png)

#### countries with maximum number of wineries and wine producers
```R
countries_data = df %>% group_by(country) %>% count() 
majority = countries_data %>% filter(n>100)
WorldMap = borders("world")
majority $lat = integer(nrow(majority ))
majority $lon = integer(nrow(majority ))

for (i in 1:nrow(majority )) {
  latlon = geocode(as.character(majority $country[i]))
  majority$lon[i] = as.numeric(latlon[1])
  majority$lat[i] = as.numeric(latlon[2])
}
ggplot() + WorldMap + geom_point(aes(x=lon, y=lat, size = n), data= top_countries, col="red")  + ggtitle("Areas from where wines mostly belong and are reviewed") +theme(legend.position="none")
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_wine_areas.png)

#### Countries which having the best and worst of wines they produce
```R
majority_new = countries_data %>% filter(n>2000)
top_wine_countries = df[df$country %in% majority_new$country, ]
top_wines<-top_wine_countries  %>% group_by(country) %>% summarise(Mean_Score = mean(points)) %>% arrange(desc(Mean_Score))%>% kable()
ggplot(top_wine_countries, aes(points, colour = country, fill = country)) + geom_density(alpha = .6) +
  labs(title ="Top Wine Producing Countries and Points given to them", x = "Points", y = "Density")

top_wines<-top_wine_countries  %>% group_by(country) %>% summarise(Mean_Score = mean(points)) %>% arrange(desc(Mean_Score))
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_top_wine_points.png)


#### Top Wine Producing Countries and their mean scores

```R 
barplot(top_wines$Mean_Score, main="Top Wine Producing Countries and their mean scores", horiz=TRUE,
        names.arg=top_wines$country)
ggplot(top_wines, aes(top_wines$country, colour = top_wines$Mean_Score, fill = top_wines$Mean_Score)) + geom_density(alpha = .6) +
  labs(title ="Top Wine Producing Countries and their mean scores", x = "Country", y = "MeanScores")
```

![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_top_wine.png)


#### Biggest Wineries
```R
wineries = subset(df, ,select=c("winery","points"))
wineries_group <- wineries %>% group_by(winery)
wineries_count <- wineries_group %>% count()
good_wineries <-  wineries_count  %>% filter(n>100)
top_wineries <- wineries[wineries$winery %in% good_wineries$winery, ]
top_wineries %>% group_by(winery) %>% summarise(Mean_Score = mean(points)) %>% arrange(desc(Mean_Score))%>% kable() 
ggplot(top_wineries, aes(points, colour = winery, fill = winery)) + geom_density(alpha = .6) +
  labs(title ="Biggest Wineries and their Point Density", x = "Points", y = "Density") 
```
![alt text](https://github.com/avneet14027/WineReviewsAnalysis/blob/master/plot_big_wine.png)





        
       
       
