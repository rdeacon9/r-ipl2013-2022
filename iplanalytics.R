library(dplyr)
library(ggplot2)
library(scales)

setwd("C:\\Users\\Ryan\\Documents\\GitHub\\r-ipl2013-2022")

auction_data <- read.csv("IPLPlayerAuctionData.csv")
standing_path <- "table export.xlsx"

auction_data["Euro"] <- auction_data$Amount * 0.012

summary(auction_data)
str(auction_data)

col_factors <- c("Player", "Role", "Team", "Year", "Player.Origin")
auction_data[col_factors] <- lapply(auction_data[col_factors], factor)

role_groupby <- function(){
  grouped <- auction_data %>% group_by(Role) %>%
    summarise(Average_cost = mean(Euro),
      .groups = 'drop'
    )
  View(grouped)
  ggplot(data=grouped, aes(x=Role, y=Average_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="Average EURO per Role", 
                       labels = comma)
}

year_groupby <- function(){
  grouped <- auction_data %>% group_by(Year) %>%
    summarise(Average_cost = mean(Euro),
              .groups = 'drop'
    )
  View(grouped)
  ggplot(data=grouped, aes(x=Year, y=Average_cost )) +
    geom_bar(stat = "identity") + 
    scale_y_continuous(name="Average EURO per Year", 
                       labels = comma)
}

origin_groupby <- function(){
  grouped <- auction_data %>% group_by(Player.Origin) %>%
    summarise(Average_cost = mean(Euro),
              .groups = 'drop'
    )
  View(grouped)
  ggplot(data=grouped, aes(x=Player.Origin, y=Average_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="Average EURO per Origin", 
                       labels = comma)
}

summary(auction_data)
str(auction_data)
origin_groupby()

