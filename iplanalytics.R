library(dplyr)
library(ggplot2)
library(scales)

setwd("C:\\Users\\Ryan\\Documents\\GitHub\\r-ipl2013-2022")

auction_data <- read.csv("IPLPlayerAuctionData.csv")
standing_path <- "table export.xlsx"

auction_data["Euro"] <- auction_data$Amount * 0.012

col_factors <- c("Player", "Role", "Team", "Year", "Player.Origin")
auction_data[col_factors] <- lapply(auction_data[col_factors], factor)
rm(col_factors)

role_mean <- function(){
  TableResult <- auction_data %>% group_by(Role) %>%
    summarise(Average_cost = mean(Euro),
      .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Role, y=Average_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="EURO", 
                       labels = comma) +
    xlab('Role')
}

year_mean <- function(){
  TableResult <- auction_data %>% group_by(Year) %>%
    summarise(Average_cost = mean(Euro),
              .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Year, y=Average_cost )) +
    geom_bar(stat = "identity") + 
    scale_y_continuous(name="EURO", 
                       labels = comma) +
    xlab('Year')
}

origin_mean <- function(){
  TableResult <- auction_data %>% group_by(Player.Origin) %>%
    summarise(Average_cost = mean(Euro),
              .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Player.Origin, y=Average_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="EURO", 
                       labels = comma) +
    xlab("Origin")
}

origin_sum <- function(){
  TableResult <- auction_data %>% group_by(Player.Origin) %>%
    summarise(Total_cost = sum(Euro),
              .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Player.Origin, y=Total_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="EURO", 
                       labels = comma) +
    xlab('Origin')
}

year_sum <- function(){
  TableResult <- auction_data %>% group_by(Year) %>%
    summarise(Total_cost = sum(Euro),
              .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Year, y=Total_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="EURO", 
                       labels = comma)
}

role_sum <- function(){
  TableResult <- auction_data %>% group_by(Role) %>%
    summarise(Total_cost = sum(Euro),
              .groups = 'drop'
    )
  View(TableResult)
  ggplot(data=TableResult, aes(x=Role, y=Total_cost )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name="EURO", 
                       labels = comma)
}

player_movement <- function(){
  pg <- auction_data %>% 
    group_by(Player,Team) %>%
    summarise(temphold = n(), .groups = 'drop'
              )
  
  player_transfers <- pg %>% 
    group_by(Player) %>% 
    summarise(unique_teams = n(), .groups = 'drop') %>% 
    ungroup %>%
    arrange(desc(unique_teams))
  View(player_transfers)
}

player_info <- function(){
  TableResult <- auction_data %>% 
    group_by(Player, Role) %>%
    summarise(MaxValue = max(Euro), MinValue = min(Euro), 
              AverageValue = mean(Euro)
              , .groups = 'drop') %>% 
    ungroup %>%
    arrange(desc(MaxValue))
  View(TableResult)
}

total_spent_by_player <- function(){
  TableResult <- auction_data %>% group_by(Player) %>%
    summarise(Total_cost = sum(Euro),
              .groups = 'drop') %>% 
    ungroup %>%
    arrange(desc(Total_cost))
  View(TableResult)
}

four_role_comp <- function(){
  TableResult <- auction_data %>% 
    group_by(Role,Year) %>%
    summarise(Total = sum(Euro), 
              .groups = 'drop')
  View(TableResult)
  ggplot(data=TableResult, aes(x=Year, y=Total, color=Role, group=Role)) +
    geom_line() + 
    scale_y_continuous(name="EURO", 
                       labels = comma) +
    xlab('Year')
}

summary(auction_data)
str(auction_data)

