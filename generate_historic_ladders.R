library(dplyr)


gamescount <- afl_tables_match_results %>% 
        group_by(Season, Team1) %>% 
        mutate(Played = row_number())

# Create dataframe with all possible match date and team combinations

merge(unique(afl_tables_match_results$Season), unique(afl_tables_match_results$Team1), all = TRUE)

seasonteam <- afl_tables_match_results %>%
        group_by(Season,Team1) %>%
        summarise() %>%
        select(Season, Team1)

seasonround <- afl_tables_match_results %>%
        filter(!(is.na(Roundadj) == TRUE)) %>%
        group_by(Season,Roundadj) %>%
        summarise() %>%
        select(Season, Roundadj)

ladder <- merge(x = seasonround, y = seasonteam,
      by = c("Season"),
      all.x = TRUE)

rm(seasonround)
rm(seasonteam)

historic_ladders <- merge(x = ladder, y = gamescount,
                         by = c("Season", "Roundadj", "Team1"),
                         all.x = TRUE, 
                      stringsAsFactors=FALSE)

rm(ladder)
rm(gamescount)

historic_ladders[c("Team1Goals", "Team1Behinds", "Team1Points", "Team2Goals", "Team2Behinds", "Team2Points")][is.na(historic_ladders[c("Team1Goals", "Team1Behinds", "Team1Points", "Team2Goals", "Team2Behinds", "Team2Points")])] <- 0
historic_ladders[c("Result")][is.na(historic_ladders[c("Result")])] <- "No game"

historic_ladders$Result <- ifelse((historic_ladders$Season == 2015 & 
                                       historic_ladders$Roundadj == 14 & 
                                       (historic_ladders$Team1 == "Adelaide" | historic_ladders$Team1 == "Geelong") & 
                                       historic_ladders$Result == "No game"), 
                              "Points split", historic_ladders$Result)

historic_ladders <- historic_ladders %>% 
        group_by(Season) %>%
        mutate(RoundsRemaining = max(Roundadj) - Roundadj) %>%
        mutate(SeasonTeams = n_distinct(Team1)) %>%
        ungroup() %>%
        group_by(Season, Team1) %>% 
        mutate(Playedbye = ifelse(Result != "No game", Played, lag(Played))) %>%
        mutate(PremPoints = ifelse(Result == "Team1 win", 4,
                                   ifelse(Result == "Team2 win", 0,
                                          ifelse(Result == "Draw", 2,
                                                 ifelse(Result == "Points split", 2,
                                          0))))) %>%
        mutate(RunningPremPoints = cumsum(PremPoints)) %>% 
        mutate(RunningWins = cumsum(ifelse(Result == "Team1 win", 1, 0))) %>% 
        mutate(RunningLosses = cumsum(ifelse(Result == "Team2 win", 1, 0))) %>% 
        mutate(RunningMarginWins = cumsum(ifelse(Result == "Team1 win", Team1FinalMargin, 0))) %>% 
        mutate(RunningMarginLosses = cumsum(ifelse(Result == "Team2 win", Team1FinalMargin, 0))) %>% 
        mutate(RunningDraws = cumsum(ifelse(Result == "Draw", 1, 0))) %>% 
        mutate(RunningGoals = cumsum(Team1Goals)) %>% 
        mutate(RunningBehinds = cumsum(Team1Behinds)) %>% 
        mutate(RunningFor = cumsum(Team1Points)) %>% 
        mutate(RunningAgainst = cumsum(Team2Points)) %>%
        mutate(AverageFor = RunningFor/Playedbye) %>% 
        mutate(AverageAgainst = RunningAgainst/Playedbye) %>%       
        mutate(RunningPercentage = RunningFor/RunningAgainst * 100) %>%
        mutate(LadderScore = 1000 * RunningPremPoints + RunningPercentage) %>%
        mutate(AverageWinMargin = RunningMarginWins/RunningWins) %>%
        mutate(AverageLossMargin = RunningMarginLosses/RunningLosses) %>%
        ungroup %>%
        group_by(Season, Roundadj) %>%
        mutate(LadderRank = rank(-LadderScore, ties.method = 'first')) %>%
        mutate(LadderRankBottom = rank(LadderScore, ties.method = 'first')) %>%
        mutate(ForRank = rank(-AverageFor, ties.method = 'min')) %>%
        mutate(ForRankBottom = rank(AverageFor, ties.method = 'min')) %>%
        mutate(AgstRank = rank(AverageAgainst, ties.method = 'min')) %>%
        mutate(AgstRankBottom = rank(-AverageAgainst, ties.method = 'min')) %>%
        mutate(PercentageRank = rank(-RunningPercentage, ties.method = 'first')) %>%
        mutate(PercentageRankBottom = rank(RunningPercentage, ties.method = 'first')) %>%
        ungroup %>%
        group_by(Season, Team1) %>% 
        mutate(PreRankTeam1 = lag(LadderRank)) %>%
        mutate(PreRankTeam1Bottom = lag(LadderRankBottom)) %>%
        mutate(PrePercTeam1 = lag(RunningPercentage)) %>%
        mutate(PrePointsTeam1 = lag(RunningPremPoints)) %>%
        mutate(PreAveForTeam1 = lag(AverageFor)) %>%
        mutate(PreAveAgstTeam1 = lag(AverageAgainst)) %>%
        mutate(PreForRankTeam1 = lag(ForRank)) %>%
        mutate(PreForRankBottomTeam1 = lag(ForRankBottom)) %>%
        mutate(PreAgstRankTeam1 = lag(AgstRank)) %>%
        mutate(PreAgstRankBottomTeam1 = lag(AgstRankBottom)) 

        
swappedranks <- historic_ladders[ , c("Season", "Roundadj", "Team1", 
                                   "PreRankTeam1", "PreRankTeam1Bottom", "PrePercTeam1", "PrePointsTeam1", 
                                   "PreAveForTeam1", "PreAveAgstTeam1",
                                   "PreForRankTeam1", "PreForRankBottomTeam1",
                                   "PreAgstRankTeam1", "PreAgstRankBottomTeam1")]

colnames(swappedranks) <- c("Season", "Roundadj", "Team2", 
                            "PreRankTeam2", "PreRankTeam2Bottom", "PrePercTeam2", "PrePointsTeam2",
                            "PreAveForTeam2", "PreAveAgstTeam2",
                            "PreForRankTeam2", "PreForRankBottomTeam2",
                            "PreAgstRankTeam2", "PreAgstRankBottomTeam2")

historic_ladders <- merge(x = historic_ladders, y = swappedranks, 
                   by = c("Season", "Roundadj", "Team2"),
                   all.x = TRUE)


historic_ladders_final <- historic_ladders %>%
        filter(RoundsRemaining == 0) %>%
        select(Season, Team1, Playedbye, RunningPremPoints, RunningWins, RunningLosses, RunningDraws, RunningGoals, RunningBehinds, RunningFor, RunningAgainst, AverageFor, AverageAgainst, RunningPercentage, LadderRank, LadderRankBottom, ForRank, ForRankBottom, AgstRank, AgstRankBottom, PercentageRank, PercentageRankBottom)

colnames(historic_ladders_final) <- c("Season", "Team1", "SeasonT1Played", "SeasonT1PremPoints", "SeasonT1Wins", "SeasonT1Losses", "SeasonT1Draws", "SeasonT1Goals", "SeasonT1Behinds", "SeasonT1For", "SeasonT1Against", "SeasonT1AverageFor", "SeasonT1AverageAgainst", "SeasonT1Percentage", "SeasonT1LadderRank", "SeasonT1LadderRankBottom", "SeasonT1ForRank", "SeasonT1ForRankBottom", "SeasonT1AgstRank", "SeasonT1AgstRankBottom", "SeasonT1PercentageRank", "SeasonT1PercentageRankBottom")

historic_ladders <- merge(x = historic_ladders, y = historic_ladders_final, 
                      by = c("Season", "Team1"),
                      all.x = TRUE)

colnames(historic_ladders_final) <- c("Season", "Team2", "SeasonT2Played", "SeasonT2PremPoints", "SeasonT2Wins", "SeasonT2Losses", "SeasonT2Draws", "SeasonT2Goals", "SeasonT2Behinds", "SeasonT2For", "SeasonT2Against", "SeasonT2AverageFor", "SeasonT2AverageAgainst", "SeasonT2Percentage", "SeasonT2LadderRank", "SeasonT2LadderRankBottom", "SeasonT2ForRank", "SeasonT2ForRankBottom", "SeasonT2AgstRank", "SeasonT2AgstRankBottom", "SeasonT2PercentageRank", "SeasonT2PercentageRankBottom")

historic_ladders <- merge(x = historic_ladders, y = historic_ladders_final, 
                      by = c("Season", "Team2"),
                      all.x = TRUE) %>%
        arrange(Season, Roundadj, ScoresGameID, Status)

rm(historic_ladders_final)
rm(swappedranks)

write.csv(historic_ladders, file="output_files/historic_ladders.csv")

  