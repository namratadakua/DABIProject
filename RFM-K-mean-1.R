# Load necessary libraries
library(dplyr)
library(ggplot2)
library(kableExtra)
library(factoextra)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data for RFM analysis
customer_data <- data.frame(
  CustomerID = seq(1, 100),
  Recency = sample(1:365, 100, replace = TRUE),  # Recency in days
  Frequency = sample(1:10, 100, replace = TRUE),  # Frequency of purchases
  Monetary = sample(50:500, 100, replace = TRUE)  # Monetary value of purchases
)

# Display the first few rows of the dataset
print(head(customer_data))

# Standardize the RFM data
standardized_data <- scale(customer_data[, c("Recency", "Frequency", "Monetary")])
standardized_data
# Determine the optimal number of clusters using the elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(standardized_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Apply k-means clustering with the chosen number of clusters
k <- 3  # Choose the number of clusters based on the elbow plot
kmeans_model <- kmeans(standardized_data, centers = k, nstart = 25)

# Add cluster assignment to the original dataset
customer_data$Cluster <- as.factor(kmeans_model$cluster)

# Display the first few rows of the dataset with cluster assignments
print(head(customer_data))

# Visualize the clusters in a scatter plot
fviz_cluster(kmeans_model, data = standardized_data, geom = "point", frame.type = "norm")

# Display summary statistics for each cluster
cluster_summary <- customer_data %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Count = n()
  )

# Print the summary statistics
kable(cluster_summary, caption = "Summary Statistics for Each Cluster", format = "html") %>%
  kable_styling(full_width = FALSE)
