library(stats)  ## for dist
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist

## There are many clustering libraries
#install.packages("NbClust")
library(NbClust)
library(cluster)
library(mclust)

library(amap)  ## for using Kmeans (notice the cap K)

library(factoextra) ## for cluster vis, silhouette, etc.
library(purrr)

#install.packages("stylo")
library(stylo)  ## for dist.cosine
#install.packages("philentropy")
library(philentropy)  ## for distance() which offers 46 metrics
## https://cran.r-project.org/web/packages/philentropy/vignettes/Distances.html
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(stringr)
library(wordcloud)
library(tm)
library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
library("dendextend")


getwd()

#setting working directory 
setwd("C:/Users/Sriram/Downloads")

my_data = read.csv('train.csv')
my_data

library(dplyr)

# Sample 50% of data based on the 'material' column
sample_size <- 15000

# Randomly sample from the dataset
sample_data <- my_data[sample(nrow(my_data), sample_size), ]

# View the sampled data
head(sample_data)

fviz_nbclust(sample_data, kmeans, method = "silhouette")



 k4 <- kmeans(sample_data, centers = 4, nstart = 25)

fviz_cluster(k4, geom = "point", data = sample_data)+
  ggtitle("k = 4")

library(dplyr)

sample_data$cluster <- as.factor(k4$cluster)

write.csv(sample_data, file = "my_data_sample_clusters.csv", row.names = FALSE)

label_col <- "cluster"
output <- data.frame(cluster = unique(sample_data[[label_col]]))

# iterate over all columns except for the label column
for (col_name in colnames(sample_data)[-which(colnames(sample_data) == label_col)]) {
  # group by label column and get min and max values for the current column
  temp <- aggregate(sample_data[[col_name]], list(sample_data[[label_col]]), range)
  # add min and max values as new columns to the output data frame
  output[paste0(col_name, "_min")] <- temp[, 2]
  output[paste0(col_name, "_max")] <- temp[, 3]
}

sample_data


#################################################################

my_16data <- my_data[, c('number_of_elements','wtd_entropy_atomic_mass',
                       'wtd_std_atomic_mass',
                       'wtd_entropy_fie',
                       'wtd_std_fie',
                       'wtd_entropy_atomic_radius',
                       'wtd_std_atomic_radius',
                       'wtd_entropy_Density',
                       'wtd_std_Density',
                       'wtd_entropy_ElectronAffinity',
                       'wtd_std_ElectronAffinity',
                       'wtd_entropy_FusionHeat',
                       'wtd_std_FusionHeat',
                       'wtd_entropy_ThermalConductivity',
                       'wtd_std_ThermalConductivity',
                       'wtd_entropy_Valence',
                       'wtd_std_Valence','critical_temp')]


fviz_nbclust(sample_my_16data, kmeans, method = "silhouette")


# Sample 50% of data based on the 'material' column
sample_size <- 1000

# Randomly sample from the dataset
sample_my_16data<- my_16data[sample(nrow(my_16data), sample_size), ]



k416 <- kmeans(sample_my_16data, centers = 3, nstart = 25)

fviz_cluster(k416, geom = "point", data = sample_my_16data)+
  ggtitle("k = 3")

sample_my_16data$cluster <- as.factor(k416$cluster)

write.csv(sample_data, file = "my_data_sample16_clusters.csv", row.names = FALSE)
