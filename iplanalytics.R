library(dplyr)
library(ggplot2)

?ggplot
setwd("C:\\Users\\Ryan\\Documents\\GitHub\\r-ipl2013-2022")

auction_data <- read.csv("IPLPlayerAuctionData.csv")
standing_path <- "table export.xlsx"

auction_data["Euro"] <- auction_data$Amount * 0.012

summary(auction_data)
str(auction_data)

factor_data <- function(){
  auction_data$Player <- factor(auction_data$Player)
  auction_data$Role <- factor(auction_data$Role)
  auction_data$Team <- factor(auction_data$Team)
  auction_data$Year <- factor(auction_data$Year)
  auction_data$Player.Origin <- factor(auction_data$Player.Origin)
}

role_groupby <- function(){
  grouped <- auction_data %>% group_by(Role) %>%
    summarise(Average_cost = mean(Euro),
      .groups = 'drop'
    )
  View(grouped)
  ggplot(data=grouped, aes(x=Role, y=Average_cost )) +
    geom_bar(stat = "identity")
}

year_groupby <- function(){
  grouped <- auction_data %>% group_by(Year) %>%
    summarise(Average_cost = mean(Euro),
              .groups = 'drop'
    )
  View(grouped)
  
}

origin_groupby <- function(){
  grouped <- auction_data %>% group_by(Player.Origin) %>%
    summarise(Average_Cost = mean(Euro),
              .groups = 'drop'
    )
  View(grouped)
  
}


factor_data()
summary(auction_data)
str(auction_data)
role_groupby()
