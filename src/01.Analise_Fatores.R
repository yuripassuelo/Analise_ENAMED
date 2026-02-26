
library( tidyverse )
library( stargazer )
library( geobr )
library( sf )

library( cluster )
library( factoextra )

# Le dados

path_int <-
  "./data/inter/"

df_final <-
  readRDS( paste0( path_int, "data_enamed_final.rds"))


#

income_dta <-
  df_final[,c("ate_1_5_sm","de_15_a_3","de_3_a_45","de_45_a_6","de_6_a_10","de_10_a_30")]
  

fviz_nbclust(income_dta, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Semente aleatória
set.seed(123) 

kmean_result <-
  kmeans( income_dta, centers = 3 )

# Visualize the clusters
fviz_cluster(kmean_result, data = income_dta,
             main = "K-Means Clustering Visualization")

data.frame( clust_inc = kmean_result$cluster,
            desemp    = df_final$acerto_med )%>%
  group_by( clust_inc )%>%
  summarise( mean_desemp= mean( desemp ))

