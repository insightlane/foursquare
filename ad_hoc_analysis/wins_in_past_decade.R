library(dplyr)
seasonroundteam <- afl_tables_match_results %>%
        ungroup %>%
        mutate(Roundnumber = ifelse(Round == "EF" | Round == "QF", 25, 
                                    ifelse(Round == "SF", 26, 
                                           ifelse(Round == "PF", 27, 
                                                  ifelse(Round == "GF", 28, Roundadj))))) %>%
        mutate(Team1adj = ifelse(Team1 == "Brisbane Bears", "Brisbane Lions", 
                                 ifelse(Team1 == "Footscray", "Western Bulldogs", 
                                        ifelse(Team1 == "Kangaroos", "North Melbourne", 
                                               ifelse(Team1 == "South Melbourne", "Sydney", Team1))))) %>%
        select(Team1, Team1adj, Roundnumber, Season, Result)

seasonroundteam$winspast10years <- NA
seasonroundteam$gamespast10years <- NA




# Loop through rows to update the 'last goal' prior to every event, and update the goal streak at every event

for(i in 1:nrow(seasonroundteam)){
        seasonroundteam[i, ]$winspast10years <- sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                                                            seasonroundteam$Result == "Team1 win" &
                                                            seasonroundteam$Season < seasonroundteam[i, ]$Season &
                                                            seasonroundteam$Season > (seasonroundteam[i, ]$Season - 10)
                                                    ) +
                sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                            seasonroundteam$Result == "Team1 win" &
                            seasonroundteam$Season == (seasonroundteam[i, ]$Season - 10) &
                            seasonroundteam$Roundnumber > seasonroundteam[i, ]$Roundnumber
                ) +
                
                sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                            seasonroundteam$Result == "Team1 win" &
                            seasonroundteam$Season == (seasonroundteam[i, ]$Season) &
                            seasonroundteam$Roundnumber <= seasonroundteam[i, ]$Roundnumber
                ) 
        
        seasonroundteam[i, ]$gamespast10years <- sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                                                            seasonroundteam$Season < seasonroundteam[i, ]$Season &
                                                            seasonroundteam$Season > (seasonroundteam[i, ]$Season - 10)
        ) +
                sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                            seasonroundteam$Season == (seasonroundteam[i, ]$Season - 10) &
                            seasonroundteam$Roundnumber > seasonroundteam[i, ]$Roundnumber
                ) +
                
                sum(seasonroundteam$Team1adj == seasonroundteam[i, ]$Team1adj &
                            seasonroundteam$Season == (seasonroundteam[i, ]$Season) &
                            seasonroundteam$Roundnumber <= seasonroundteam[i, ]$Roundnumber
                ) 
        
}

seasonroundteam$winratio <- seasonroundteam$winspast10years/seasonroundteam$gamespast10years
        