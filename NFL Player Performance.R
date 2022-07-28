library(tidyverse)
install.packages("data.table")
install.packages("ggplot")
library(data.table)
library(ggplot2)
library(ggimage)
#get data for 2021

install.packages("nflfastR")
library(nflfastR)

pbp <- load_pbp(2021)
pbp <- data.table(pbp)

pbp[receiver_player_name == "J.Meyers"]


fun_wr <- function(player) {
    
    data_player <- pbp[receiver_player_name == player]
    data_yds <- aggregate(data_player$yards_gained, by = list(data_player$week),
                          sum)
    data_yac <- aggregate(data_player$yards_after_catch, by = list(data_player$week),
                          function(x) sum(x, na.rm = TRUE))
    data_airyards <- aggregate(data_player$air_yards, by = list(data_player$week),
                               sum)
    data_fd <- aggregate(data_player$first_down_pass, by = list(data_player$week),
                         sum)
    data_epa <- aggregate(data_player$epa, by = list(data_player$week),
                         sum)
    data_wpa <- aggregate(data_player$wpa, by = list(data_player$week),
                          sum)
    
    
    
    data_return <- data.table(player = player,
                              week = data_yds$Group.1,
                              yds = data_yds$x,
                              yac = data_yac$x,
                              airyards = data_airyards$x,
                              epa = data_epa$x,
                              fd = data_fd$x,
                              wpa = data_wpa$x)
    return(data_return)
}

plot_names <- c('yds' = 'Total Yards',
                'yac' = 'Yards After Catch',
                'airyards' = 'Air Yards',
                'fd' = 'First Down',
                'epa' = 'Expected Points Added',
                'wpa' = 'Win Percentage Added')

data_plot <- fun_wr("J.Jefferson")
data_plot$player <- NULL
data_plot <- melt(data_plot, id.vars = "week")
ggplot(data_plot, aes(x = week, y = value, col = variable)) +
  geom_point(size=3) +
  facet_wrap(~ variable, scales = "free",
             labeller = as_labeller(plot_names)) +
  labs(x = "Week",
       y = "Value",
       title = "Analyzing WR Performance \nin 2021-22",
       subtitle = "By: Dylan Wilkerson") +
  theme_dark() +
  theme(legend.position = 'none') 
  
  
 
  





