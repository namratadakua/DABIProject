library(rjson)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(factoextra)

portfolio <- stream_in(file("portfolio.json"))
profile <- stream_in(file("./profile.json"))

transcript <- stream_in(file("./transcript.json"))
transactions <- data.frame(
  person_id = transcript[["person"]],
  event = transcript[["event"]],
  offerid = transcript[["value"]][["offer id"]],
  offer_id = transcript[["value"]][["offer_id"]],
  spent_amount = transcript[["value"]][["amount"]],
  reward_paid = transcript[["value"]][["reward"]],
  time = transcript[["time"]]
)

transaction_offer_completed <- subset(transactions, event == "offer completed")

transactions_by_person_event <- transaction_offer_completed %>%
  group_by(person_id) %>%
  summarize(
    total_reward_paid = sum(reward_paid),
    avg_time = mean(time),
    total_offer_id = n()
    # You can add more aggregations for other columns if needed
  )

transactions_by_person_event <- as.data.frame(transactions_by_person_event)

# Assign 'ID' column as row names
rownames(transactions_by_person_event) <- transactions_by_person_event$person_id

# Remove the 'ID' column if necessary
transactions_by_person_event <- transactions_by_person_event[, -which(names(transactions_by_person_event) == "person_id")]

wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(transactions_by_person_event, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Within Sum of Squares")


# Apply k-means clustering with the chosen number of clusters
k <- 4  # Choose the number of clusters based on the elbow plot
kmeans_model <- kmeans(transactions_by_person_event, centers = k, nstart = 25)

# Add cluster assignment to the original dataset
transactions_by_person_event$Cluster <- as.factor(kmeans_model$cluster)

# Display the first few rows of the dataset with cluster assignments
print(head(transactions_by_person_event))

# Visualize the clusters in a scatter plot
fviz_cluster(kmeans_model, data = transactions_by_person_event, geom = "point", frame.type = "norm")

# Display summary statistics for each cluster
cluster_summary <- transactions_by_person_event %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Recency = mean(avg_time),
    Avg_Frequency = mean(total_offer_id),
    Avg_Monetary = mean(total_reward_paid),
    Count = n()
  )

# Print the summary statistics
kable(cluster_summary, caption = "Summary Statistics for Each Cluster", format = "html") %>%
  kable_styling(full_width = FALSE)