
        
        
library(dplyr)

team_season_accuracy <- afl_tables_match_results %>%
        ungroup() %>%
        mutate(Team1 = ifelse(Team1 == "Footscray", "Western Bulldogs", 
                              ifelse(Team1 == "South Melbourne", "Sydney", 
                                     ifelse(Team1 == "Kangaroos", "North Melbourne", 
                                            ifelse(Team1 == "Brisbane Bears", "Brisbane Lions", Team1))))) %>%
        group_by(Season, Team1) %>%
        mutate(team_season_game_no = cumsum(ifelse(row_number() > 0,1,0))) %>%
        filter(team_season_game_no <= 9) %>%
        summarise(Sum_Goals = sum(Team1Goals),
                  Sum_Behinds = sum(Team1Behinds),
                  count_games = n()) %>%
        mutate(Accuracy = Sum_Goals/(Sum_Goals + Sum_Behinds)) %>%
        arrange(-Accuracy) %>%
        select(Season, Team1, count_games, Sum_Goals, Sum_Behinds, Accuracy) %>%
        


team_season_accuracy_past_season





names <- test %>%
        ungroup() %>%
        distinct(Team1) %>%
        arrange(Team1)