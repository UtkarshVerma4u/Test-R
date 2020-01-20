library(factoextra)
library(clustertend)
library(seriation)

data("faithful")
df <- faithful
head(df)

library("ggplot2")
ggplot(df, aes(x=eruptions, y=waiting)) +
  geom_point() +  # Scatter plot
  geom_density_2d() # Add 2d density estimation

# Generate random dataset
set.seed(123)
n <- nrow(df)

random_df <- data.frame(
  x = runif(nrow(df), min(df$eruptions), max(df$eruptions)),
  y = runif(nrow(df), min(df$waiting), max(df$waiting)))

# random uniform data can be generated in a single line function call as follow:
  
# random_df <- apply(df, 2, 
# function(x, n){runif(n, min(x), (max(x)))}, n)

# Plot the data
ggplot(random_df, aes(x, y)) + geom_point()

library(factoextra)
set.seed(123)
# K-means on faithful dataset
km.res1 <- kmeans(df, 2)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             frame.type = "norm", geom = "point", stand = FALSE)

# K-means on the random dataset
km.res2 <- kmeans(random_df, 2)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             frame.type = "norm", geom = "point", stand = FALSE)

# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 2,  cex = 0.5)


# It can be seen that, k-means algorithm and hierarchical clustering impose a classification
# on the random uniformly distributed dataset even if there are no meaningful clusters present
# in it.Clustering tendency assessment methods are used to avoid this issue.

# R function for computing Hopkins statistic

library(clustertend)
# Compute Hopkins statistic for faithful dataset
set.seed(123)
hopkins(faithful, n = nrow(faithful)-1)

# Compute Hopkins statistic for a random dataset
set.seed(123)
hopkins(random_df, n = nrow(random_df)-1)

# It can be seen that faithful dataset is highly clusterable
# (the H value = 0.15 which is far below the threshold 0.5).
# However the random_df dataset is not clusterable (H=0.53)


# The visual assessment of cluster tendency (VAT) has been originally described 
# by Bezdek and Hathaway (2002). This approach can be used to visually inspect the
# clustering tendency of the dataset.

library("seriation")
# faithful data: ordered dissimilarity image
df_scaled <- scale(faithful)
df_dist <- dist(df_scaled) 
dissplot(df_dist)

# faithful data: ordered dissimilarity image
random_df_scaled <- scale(random_df)
random_df_dist <- dist(random_df_scaled) 
dissplot(random_df_dist)

# Cluster tendency
clustend <- get_clust_tendency(scale(faithful), 100)
# Hopkins statistic
clustend$hopkins_stat

# Customize the plot
clustend$plot + 
  scale_fill_gradient(low = "steelblue", high = "white")

# K-means on the random dataset
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)