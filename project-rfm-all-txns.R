library(rjson)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(factoextra)
set.seed(123)

portfolio <- stream_in(file("portfolio.json"))


portfolio$profolio_number <- seq(from=101 , to=nrow(portfolio)+100)

profile <- stream_in(file("profile.json"))

profile$profile_number <- seq(from =1 , to=nrow(profile))

transcript <- stream_in(file("transcript.json"))
transactions <- data.frame(
  person_id = transcript[["person"]],
  event = transcript[["event"]],
  offerid = transcript[["value"]][["offer id"]],
  offer_id = transcript[["value"]][["offer_id"]],
  spent_amount = transcript[["value"]][["amount"]],
  reward_paid = transcript[["value"]][["reward"]],
  time = transcript[["time"]]
)

transaction_with_profile <- left_join(transactions, profile, by = c("person_id" = "id"))

transaction_with_profile <- transaction_with_profile[order(transaction_with_profile$person_id, 
                                                           transaction_with_profile$offerid, 
                                                           transaction_with_profile$offer_id), ]

txn_only <- subset(transaction_with_profile, event == 'transaction')

transactions_by_person_id <- txn_only %>%
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
kable(cluster_summary, caption = "Summary Statistics for Each Cluster", format = "html") %>%
  kable_styling(full_width = FALSE)

#### k-means on transactions with promotion RFM - (END) #####