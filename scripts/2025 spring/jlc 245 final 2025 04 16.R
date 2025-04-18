install.packages('cluster')
install.packages('ggpubr')
install.packages('ggrepel')
install.packages('tidyverse')
library(cluster)
library(tidyverse)
library(ggpubr)
library(ggrepel)

##### basketball example

# create data frame
df <- data.frame(points=c(4, 4, 6, 7, 8, 14, 16, 19, 25, 25, 28),
                 assists=c(3, 2, 2, 5, 4, 8, 7, 6, 8, 10, 11),
                 blocks=c(7, 3, 6, 7, 5, 8, 8, 4, 2, 2, 1),
                 rebounds=c(4, 5, 5, 6, 5, 8, 10, 4, 3, 2, 2))

#add row names
row.names(df) <- LETTERS[1:11]

#calculate distance matrix
d <- dist(df)

#perform multidimensional scaling
fit <- cmdscale(d, eig=TRUE, k=2)

#extract (x, y) coordinates of multidimensional scaleing
x <- fit$points[,1]
y <- fit$points[,2]

#create scatter plot
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Multidimensional Scaling Results", type="n")

#add row names of data frame as labels
text(x, y, labels=row.names(df))

##### public health data example

# GET DATA
# Download and unzip the file from here: https://drive.google.com/file/d/1qM0Nj3bk_GMTGIcf6GQOmXnQX6WkA5nX/view?usp=sharing
bigcityhealth <- read.csv("C:/Users/danna/Downloads/BigCitiesHealth.csv",
                          stringsAsFactors = FALSE)

# FILTER THE TABLE
city.2023a <- subset(bigcityhealth, bigcityhealth$date_label == '2023' &
                       bigcityhealth$strata_race_label == 'All' &
                       bigcityhealth$strata_sex_label == 'Both' &
                       bigcityhealth$geo_label_citystate != 'U.S. Total')

# REMOVE EXTRA COLUMNS
city.2023b <- city.2023a[c(1:4)]

# RESHAPE THE TABLE
city.2023c <- city.2023b %>%
  pivot_wider(names_from = metric_item_label, values_from = value)

# CALCULATE MDS
mds.city <- cmdscale(dist(city.2023c[, 3:98]))

# EMPTY PLOT
plot(mds.city[, 1], mds.city[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# ADD POINTS
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = "lightblue")
text(mds.city[, 1], mds.city[, 2], 
     labels = city.2023c$geo_label_citystate, 
     pos = 3, cex = 0.8)

# CALCULATE K MEANS CLUSTERING
clusters <- kmeans(mds.city, centers = 5)$cluster

# ADD CLUSTERS TO THE PLOT
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# UPDATE DATA TABLE
mds.df <- as.data.frame(mds.city)
mds.df$groups <- as.factor(clusters)
mds.df$city <- city.2023c$geo_label_citystate  

# UPDATED PLOT WITH CLUSTERS
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS Cities Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = city), box.padding = 0.5)


