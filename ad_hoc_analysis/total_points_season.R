library(dplyr)

historic_ladders %>%
        group_by(Season, Team1) %>%
        filter(Played <= 6, Season >= 1994) %>%
        summarise(total_points = sum(Team1Points), 
                  games = n()) %>%
        arrange(-total_points)



historic_ladders %>%
        group_by(Season, Team1) %>%
        filter(Team1 == "Adelaide") %>%
        summarise(plus140 = sum(ifelse(Team1Points >= 140, 1, 0)), 
                  games = n()) %>%
        arrange(-plus140)