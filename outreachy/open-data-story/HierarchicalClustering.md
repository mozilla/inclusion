# Hierarchical Clustering in R

In this hierarchical clustering using R has been done and explained. <br>

## Hierarchical Clustering
First, lets understand how this algorithm works. <br>
The goal for this is to hirarchically cluster the data, by findind a hierarchical relationship between the data points.
There are namely two kinds of approaches towards this. One is Agglomerative clustering and the other is divisive clustering.
* Agglomerative: In this kind of clustering, initially all data points are in their own clusters. Gradually after every iteration
they are clustered together in a hierarchy based on some kind of similarity. It is a bottom up approach.

* Divisive: This is opposite t agglomerative clustering. This is a bottom down approach in which all data points initially starrt
in one cluster and are separaed into a variety of clusters based on their similariy and dissimilarity.

Using R for hierarchical clustering

```R
df <- dist(as.matrix(mtcars))
na.omit(df)
hc <- hclust(df)   
plot(hc)
```

![alt text](https://github.com/avneet14027/Clustering/blob/master/hierarchy.png)
