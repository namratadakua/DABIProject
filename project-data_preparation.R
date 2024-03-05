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

txn_for_profile_age_118 <- subset(transaction_with_profile, age == 118)

txn_for_profile_not_118 <- subset(transaction_with_profile, age != 118)

txn_for_profile_not_118 <- txn_for_profile_not_118[order(txn_for_profile_not_118$person_id, 
                                                         txn_for_profile_not_118$offerid, 
                                                         txn_for_profile_not_118$offer_id), ]

txn_only <- subset(txn_for_profile_not_118, event == 'transaction')

#Define column names
column_names <- c("person_id", "offer_id", "spent_amount", "reward_paid", "time",
                  "gender", "age", "became_member_on", "income", "profile_number")

# Create an empty data frame with specified column names
txn_with_promotion <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(txn_with_promotion) <- column_names
txn_without_promotion <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(txn_without_promotion) <- column_names


for(i in 1:nrow(txn_only)) {
  
  person_id <- txn_only[i, ]$person_id
  time <- txn_only[i, ]$time
  spent_amount <- txn_only[i, ]$spent_amount
  gender <- txn_only[i,]$gender
  age <- txn_only[i, ]$age
  became_member_on <- txn_only[i, ]$became_member_on
  income <- txn_only[i,]$income
  profile_number <- txn_only[i, ]$profile_number
  
  print(paste("Processing ", i , " ", person_id, " + ", time))
  
  offer_completed_rows <- txn_for_profile_not_118[txn_for_profile_not_118$event == 'offer completed' &
                                                    txn_for_profile_not_118$person_id == person_id &
                                                    txn_for_profile_not_118$time == time, ]
  
  
  if(nrow(offer_completed_rows) == 0 ) {
    print(paste("no row found ",  person_id, " + ", time))
    print(paste("txn_without_promotion nrow ",  nrow(txn_without_promotion)))
    new_row <- c(person_id, NA, spent_amount,
                 NA, time, gender, age,
                 became_member_on, income, profile_number)
    print(new_row)
    txn_without_promotion[nrow(txn_without_promotion)+1, ] <- new_row
    
  } else {
    
    print(paste("rows found ",  person_id, " + ", time, " + ", nrow(offer_completed_rows)))
    print(paste("txn_with_promotion nrow ",  nrow(txn_with_promotion)))
    for(j in 1:nrow(offer_completed_rows)) {
      offer_completed_row <- offer_completed_rows[j, ]
      
      new_row <- c(person_id, offer_completed_row$offer_id, spent_amount,
                   offer_completed_row$reward_paid, time, gender, age,
                   became_member_on, income, profile_number)
      txn_with_promotion[nrow(txn_with_promotion)+1, ] <- new_row
      
    }
  }
  
}

#write.csv(txn_with_multiple_time, file = './txn_woth_multiple_time.csv')
write.csv(txn_with_promotion, file = './txn_with_promotion2.csv')
write.csv(txn_without_promotion, file = './txn_without_promotion2.csv')