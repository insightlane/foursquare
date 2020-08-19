library(dplyr)

historic_ladders_alt <- historic_ladders %>%
        mutate(Team1Goals_alt = ifelse(Result == "Team1 win", Team1Goals - 1, 
                                   ifelse(Result == "Team2 win", Team1Goals + 1, 
                                          Team1Goals))) %>%
        mutate(Team1Behinds_alt = ifelse(Result == "Team1 win", Team1Behinds + 1, 
                                   ifelse(Result == "Team2 win", Team1Behinds - 1, 
                                          Team1Behinds))) %>%
        mutate(Team2Goals_alt = ifelse(Result == "Team2 win", Team2Goals - 1, 
                                   ifelse(Result == "Team1 win", Team2Goals + 1, 
                                          Team2Goals))) %>%
        mutate(Team2Behinds_alt = ifelse(Result == "Team2 win", Team2Behinds + 1, 
                                     ifelse(Result == "Team1 win", Team2Behinds - 1, 
                                            Team2Behinds))) %>%
        mutate(Team1Points_alt = 6 * Team1Goals_alt + Team1Behinds_alt) %>%
        mutate(Team2Points_alt = 6 * Team2Goals_alt + Team2Behinds_alt) %>%
        mutate(Team1FinalMargin_alt = Team1Points_alt - Team2Points_alt) %>%
        mutate(AbsFinalMargin_alt = abs(Team1FinalMargin_alt)) %>%
        mutate(Result_alt = ifelse(Result == "No game", "No game",
                                   ifelse((Team1FinalMargin_alt > 0), "Team1 win",
                               ifelse((Team1FinalMargin_alt < 0), "Team2 win",
                                      ifelse((Team1FinalMargin_alt == 0), "Draw",
                                             Result))))) %>%
        mutate(Winner_alt = ifelse(Result == "No game", NA,
                                   ifelse((Team1FinalMargin_alt > 0), Team1,
                                  ifelse((Team1FinalMargin_alt < 0), Team2,
                                         ifelse((Team1FinalMargin_alt == 0), "Draw",
                                                Winner))))) %>%
        mutate(Loser_alt = ifelse(Result == "No game", NA,
                                  ifelse((Team1FinalMargin_alt > 0), Team2,
                                 ifelse((Team1FinalMargin_alt < 0), Team1,
                                        ifelse((Team1FinalMargin_alt == 0), "Draw",
                                               Loser))))) %>%

        group_by(Season, Team1) %>% 
        mutate(PremPoints_alt = ifelse(Result_alt == "Team1 win", 4,
                                   ifelse(Result_alt == "Team2 win", 0,
                                          ifelse(Result_alt == "Draw", 2,
                                                 ifelse(Result_alt == "Points split", 2,
                                                        0))))) %>%
        mutate(RunningPremPoints_alt = cumsum(PremPoints_alt)) %>% 
        mutate(RunningWins_alt = cumsum(ifelse(Result_alt == "Team1 win", 1, 0))) %>% 
        mutate(RunningLosses_alt = cumsum(ifelse(Result_alt == "Team2 win", 1, 0))) %>% 
        mutate(RunningDraws_alt = cumsum(ifelse(Result_alt == "Draw", 1, 0))) %>% 
        mutate(RunningGoals_alt = cumsum(Team1Goals_alt)) %>% 
        mutate(RunningBehinds_alt = cumsum(Team1Behinds_alt)) %>% 
        mutate(RunningFor_alt = cumsum(Team1Points_alt)) %>% 
        mutate(RunningAgainst_alt = cumsum(Team2Points_alt)) %>%
        mutate(AverageFor_alt = RunningFor_alt/Played) %>% 
        mutate(AverageAgainst_alt = RunningAgainst_alt/Played) %>%       
        mutate(RunningPercentage_alt = RunningFor_alt/RunningAgainst_alt * 100) %>%
        mutate(LadderScore_alt = 1000 * RunningPremPoints_alt + RunningPercentage_alt) %>%
        ungroup %>%
        group_by(Season, Roundadj) %>%
        mutate(LadderRank_alt = rank(-LadderScore_alt, ties.method = 'first')) %>%
        mutate(LadderRankBottom_alt = rank(LadderScore_alt, ties.method = 'first')) %>%
        mutate(ForRank_alt = rank(-AverageFor_alt, ties.method = 'min')) %>%
        mutate(ForRankBottom_alt = rank(AverageFor_alt, ties.method = 'min')) %>%
        mutate(AgstRank_alt = rank(AverageAgainst_alt, ties.method = 'min')) %>%
        mutate(AgstRankBottom_alt = rank(-AverageAgainst_alt, ties.method = 'min')) %>%
        mutate(PercentageRank_alt = rank(-RunningPercentage_alt, ties.method = 'first')) %>%
        select(Season, Round, Team1, Played, RunningPremPoints, RunningWins, RunningLosses, RunningDraws, RunningPercentage, LadderRank, RunningPremPoints_alt, RunningWins_alt, RunningLosses_alt, RunningDraws_alt, RunningPercentage_alt, LadderRank_alt)


write.csv(historic_ladders_alt, file="historic_ladders_alt.csv")


        
        