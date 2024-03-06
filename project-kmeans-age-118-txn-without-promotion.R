library(rjson)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(factoextra)
set.seed(123)

#### k-means on raw transactions - (START) #####


txn_without_promotion <- read.csv("./txn_without_promotion_age_118.csv")

# age, gender and income not included not available
selected_data <- subset(txn_without_promotion, 
                        select = c('profile_number', 'spent_amount', 'time', 'became_member_on'))


wss<- NULL
for (i in 1:10){
  fit = kmeans(selected_data,centers = i)
  wss = c(wss, fit$tot.withinss)
}
plot(1:10, wss, type = "o")


# Apply k-means clustering with the chosen number of clusters
k <- 4  # Choose the number of clusters based on the elbow plot
kmeans_model <- kmeans(selected_data, centers = k, nstart = 25)

fviz_cluster(kmeans_model, data = selected_data, geom = "point", ellipse.type = "norm")

# Add cluster assignment to the original dataset
selected_data$Cluster <- as.factor(kmeans_model$cluster)

txn_without_promotion$cluster <- NA

for(k in 1:nrow(txn_without_promotion)) {
  cluster <- selected_data[k, ]$Cluster
  print(paste("cluster for row",  k, " is ", cluster))
  txn_without_promotion[k, ]$cluster <- cluster
}


write.csv(txn_without_promotion, 
          "age_118_txn_without_promotion_clustered.csv", 
          row.names = FALSE)

### interpretation 
summary_cluster_wise <- txn_without_promotion %>%
  group_by(cluster) %>%
  summarize(
    avg_spent_amount = mean(spent_amount),
    avg_reward_paid = mean(reward_paid),
    avg_time = mean(time))

# Print the summary statistics
summary_cluster_wise_kable <- kable(summary_cluster_wise, caption = "Summary Statistics for Each Cluster", format = "html") %>%
  kable_styling(full_width = FALSE)

#write.csv(as.data.frame(summary_cluster_wise_kable), "age_118_summary_cluster_wise.csv", row.names = FALSE)

save_kable(summary_cluster_wise_kable, "age_118_summary_cluster_wise_without_promotion.png")

#### k-means on raw transactions - (END) #####


#### k-means on transactions with promotion RFM - (START) #####

transactions_by_person_id <- txn_without_promotion %>%
  group_by(profile_number) %>%
  summarize(
    Monetary = sum(spent_amount),
    Recency = mean(time),
    Frequency = n()
    # You can add more aggregations for other columns if needed
  )

transactions_by_person_id <- as.data.frame(transactions_by_person_id)

standardized_txn_data <- scale(transactions_by_person_id[, c("Recency", "Frequency", "Monetary")])
#standardized_txn_data

wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(standardized_txn_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Within Sum of Squares")


# Apply k-means clustering with the chosen number of clusters
k <- 4  # Choose the number of clusters based on the elbow plot
kmeans_model <- kmeans(standardized_txn_data, centers = k, nstart = 25)

# Add cluster assignment to the original dataset
transactions_by_person_id$Cluster <- as.factor(kmeans_model$cluster)

# Display the first few rows of the dataset with cluster assignments
print(head(standardized_txn_data))

# Visualize the clusters in a scatter plot
fviz_cluster(kmeans_model, data = standardized_txn_data, geom = "point", ellipse.type = "norm")

fviz_cluster(kmeans_model, data = standardized_txn_data,  ellipse.type = "norm") 
#transactions_by_person_event <- transactions_by_person_id[, -which(names(transactions_by_person_id) == "profile_number")]
# Display summary statistics for each cluster
cluster_summary <- transactions_by_person_id %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Count = n()
  )

# Print the summary statistics
rfm_cluster_summary <- kable(cluster_summary, caption = "RFM Summary Statistics for Each Cluster for age 118 profiles", format = "html") %>%
  kable_styling(full_width = FALSE)

#write.csv(as.data.frame(rfm_cluster_summary), "age_118_rfm_summary_cluster_wise.csv", row.names = FALSE)
save_kable(rfm_cluster_summary, "age_118_rfm_summary_cluster_wise_without_promotion.png")

#### k-means on transactions with promotion RFM - (END) #####