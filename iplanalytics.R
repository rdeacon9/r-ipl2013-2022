auction_data <- read.csv("IPLPlayerAuctionData.csv")
standing_path <- "table export.xlsx"

#coverting indian Rupee to EUR
auction_data["Euro"] <- auction_data$Amount * 0.012

