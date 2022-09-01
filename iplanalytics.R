auction_data <- read.csv("IPLPlayerAuctionData.csv")
standing_path <- "table export.xlsx"

#Coverting Indian Rupee to EURO
auction_data["Euro"] <- auction_data$Amount * 0.012

