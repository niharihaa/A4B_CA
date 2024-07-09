# Install and load necessary packages
install.packages(c("tidyverse", "cluster", "factoextra", "dendextend", "GGally", "pheatmap", "MASS", "Rtsne"))
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(GGally)
library(pheatmap)
library(MASS)
library(Rtsne)

# Load the data
file_path <- "C:\\Users\\nihar\\OneDrive\\Desktop\\Bootcamp\\SCMA 632\\DataSet\\Survey.csv"
survey_data <- read.csv(file_path)

# Handle missing values (if any)
survey_data <- na.omit(survey_data)

# Identify numeric columns
numeric_columns <- sapply(survey_data, is.numeric)

# Separate numeric and non-numeric data
numeric_data <- survey_data[, numeric_columns]
non_numeric_data <- survey_data[, !numeric_columns]

# Standardize the numeric data
survey_data_scaled <- scale(numeric_data)

# Determine the optimal number of clusters using Elbow and Silhouette methods
fviz_nbclust(survey_data_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Determining Optimal Number of Clusters")

fviz_nbclust(survey_data_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Determining Optimal Number of Clusters")

# Perform K-means clustering (assuming 3 clusters as optimal)
set.seed(123)
kmeans_result <- kmeans(survey_data_scaled, centers = 3, nstart = 25)

# Add the cluster assignment to the original data
survey_data$cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data = survey_data_scaled,
             geom = "point", ellipse.type = "convex") +
  labs(title = "Cluster Visualization")

# Interpretation and Characterization
cluster_summary <- survey_data %>%
  select_if(is.numeric) %>%
  bind_cols(cluster = survey_data$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

print(cluster_summary)

