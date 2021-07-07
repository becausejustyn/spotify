# Load the Songs Features Data
load('SongsData.RData')

library(highcharter)
library(RColorBrewer)
library(shiny)


# Clustering
# Preprocessing before kmeans (Normalization)
feature_data <- data[, c(6,7,9,11:17)]
feature_data <- data.frame(scale(feature_data, center = TRUE, scale = TRUE))

# NbClust, provides ref to decide how many clusters to divide
## It shows that 2 clusters is optimal number 
library('factoextra')
library('NbClust')
set.seed(123)
opt_clus <- NbClust(data = feature_data, distance = "manhattan",
                  min.nc = 2, max.nc = 15, method = "ward.D", index = "all")
opt_clus$Best.nc
opt_clus$Best.partition
fviz_nbclust(opt_clus) + theme_minimal()
opt_clus <- opt_clus$Best.partition

# PCA
## The top 2 principal compoents explains 44.8% of total variance
feature_pca <- prcomp(feature_data, scale = TRUE)
summary(feature_pca)
#save.image('Cluster_result.RData')

# Plot the PCA & Clustering Result
## Ref from RCharlie CoachellaR, need to comprehend
load('Cluster_result.RData')

lam <- feature_pca$sdev[1:2] * sqrt(nrow(feature_pca$x))

hplot <- (feature_pca$x[, 1:2] / lam) %>% 
  as.data.frame %>% 
  mutate(name = data$tracks,
         cluster = opt_clus,
         tooltip = name)

dfobs <- (feature_pca$x[, 1:2] / lam) %>% 
  as.data.frame

dfcomp <- feature_pca$rotation[, 1:2] * lam
mx <- max(abs(dfobs[, 1:2]))
mc <- max(abs(dfcomp)) 
dfcomp <- dfcomp %>% 
{ . / mc * mx } %>% 
  as.data.frame() %>% 
  setNames(c("x", "y")) %>% 
  rownames_to_column("name") %>%  
  as_data_frame() %>% 
  group_by_("name") %>%
  do(data = list(c(0, 0), c(.$x, .$y))) %>%
  list_parse

pal <- brewer.pal(3, 'Accent')[1:2]

clus_result <- hchart(hplot, hcaes(x = PC1, y = PC2, group = cluster), type = 'scatter') %>%
  hc_add_series_list(dfcomp) %>%
  hc_colors(color = c(pal, rep('lightblue', nrow(feature_pca$rotation)))) %>%
  hc_tooltip(pointFormat="{point.name}") %>%
  hc_title(text = 'Clustering Tracks') %>%
  hc_subtitle(text = HTML('<em>PCA and Clusters of tracks in Singles</em>')) %>% 
  hc_xAxis(title = list(text = 'Principle Component 1')) %>% 
  hc_yAxis(title = list(text = 'Principle Component 2')) %>% 
  hc_add_theme(hc_theme_smpl())

htmlwidgets::saveWidget(widget = clus_result, file = "Clustering_result.html")


hchart(princomp(feature_data, cor = TRUE))
