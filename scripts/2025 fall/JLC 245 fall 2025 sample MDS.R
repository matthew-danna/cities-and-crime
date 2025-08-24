install.packages('cluster')
install.packages('ggpubr')
install.packages('ggrepel')
library(cluster)
library(tidyverse)
library(ggpubr)
library(ggrepel)

# GET DATA
# Download and unzip the file from here: https://drive.google.com/file/d/1qM0Nj3bk_GMTGIcf6GQOmXnQX6WkA5nX/view?usp=sharing
bigcityhealth <- read.csv("YOU FILE PATH HERE",
                          stringsAsFactors = FALSE)

# FILTER THE TABLE
city.2023a <- subset(bigcityhealth, bigcityhealth$date_label == 'xxxx' &
                       bigcityhealth$strata_race_label == 'yyy' &
                       bigcityhealth$strata_sex_label == 'zzz' &
                       bigcityhealth$geo_label_citystate != 'aaaaaa')

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
clusters <- kmeans(mds.city, centers = n)$cluster

# ADD CLUSTERS TO THE PLOT
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# UPDATE DATA TABLE
mds.df <- as.data.frame(mds.city)
mds.df$groups <- as.factor(clusters)
mds.df$city <- city.2023c$geo_label_citystate  # Add species information

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




