library(rjson)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(factoextra)
set.seed(123)
#### k-means on raw transactions - (START) #####

portfolio <- stream_in(file("portfolio.json"))
portfolio$profolio_number <- seq(from=101 , to=nrow(portfolio)+100)
txn_with_promotion <- read.csv("./txn_with_promotion2.csv")

txn_with_profile_portfolio <- left_join(txn_with_promotion, portfolio, by = c("offer_id" = "id"))

txn_with_profile_portfolio$profile_number <- as.numeric(txn_with_profile_portfolio$profile_number)
txn_with_profile_portfolio$spent_amount <- as.numeric(txn_with_profile_portfolio$spent_amount)
txn_with_profile_portfolio$reward_paid <- as.numeric(txn_with_profile_portfolio$reward_paid)
txn_with_profile_portfolio$time <- as.numeric(txn_with_profile_portfolio$time)
txn_with_profile_portfolio$age <- as.numeric(txn_with_profile_portfolio$age)
txn_with_profile_portfolio$became_member_on <- as.numeric(txn_with_profile_portfolio$became_member_on)
txn_with_profile_portfolio$income <- as.numeric(txn_with_profile_portfolio$income)

txn_with_profile_portfolio$gender_numeric <- match(txn_with_profile_portfolio$gender, unique(txn_with_profile_portfolio$gender))

selected_data <- subset(txn_with_profile_portfolio, 
                        select = c('profile_number', 'profolio_number', 'spent_amount',
                                  'reward_paid', 'time', 'gender_numeric', 
                                  'age', 'became_member_on', 'income'))


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

txn_with_profile_portfolio$cluster <- NA

for(k in 1:nrow(txn_with_profile_portfolio)) {
  cluster <- selected_data[k, ]$Cluster
  print(paste("cluster for row",  k, " is ", cluster))
  txn_with_profile_portfolio[k, ]$cluster <- cluster
}


write.csv(txn_with_profile_portfolio[, !names(txn_with_profile_portfolio) %in% c("channels")], "txn_with_promotion_clustered1.csv", 
          row.names = FALSE)

### interpretation 
summary_cluster_wise <- txn_with_profile_portfolio %>%
  group_by(cluster) %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    avg_spent_amount = mean(spent_amount),
    avg_reward_paid = mean(reward_paid),
    avg_time = mean(time))
                            
# Print the summary statistics
kable(summary_cluster_wise, caption = "Summary Statistics for Each Cluster", format = "html") %>%
  kable_styling(full_width = FALSE)


### interpretation 
summary_gender_and_cluster_wise <- txn_with_profile_portfolio %>%
  group_by(cluster, gender) %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    avg_spent_amount = mean(spent_amount),
    avg_reward_paid = mean(reward_paid),
    avg_time = mean(time))

# Print the summary statistics
kable(summary_gender_and_cluster_wise, caption = "Summary Statistics for Each Cluster and gender wise", format = "html") %>%
  kable_styling(full_width = FALSE)


#### k-means on raw transactions - (END) #####


#### k-means on transactions with promotion RFM - (START) #####

transactions_by_person_id <- txn_with_profile_portfolio %>%
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
