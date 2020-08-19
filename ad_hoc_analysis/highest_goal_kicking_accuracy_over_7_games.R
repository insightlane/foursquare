library(dplyr)

test <- afl_tables_match_results %>%
        ungroup() %>%
        mutate(Team1 = ifelse(Team1 == "Footscray", "Western Bulldogs", 
                              ifelse(Team1 == "South Melbourne", "Sydney", 
                                     ifelse(Team1 == "Kangaroos", "North Melbourne", Team1)))) %>%
        group_by(Team1) %>%
        mutate(Sum_Goals = Team1Goals 
               + lag(Team1Goals, 1)
               + lag(Team1Goals, 2)
               + lag(Team1Goals, 3)
               + lag(Team1Goals, 4)
               + lag(Team1Goals, 5)
               + lag(Team1Goals, 6)
               ) %>%
        mutate(Sum_Behinds = Team1Behinds 
               + lag(Team1Behinds, 1)
               + lag(Team1Behinds, 2)
               + lag(Team1Behinds, 3)
               + lag(Team1Behinds, 4)
               + lag(Team1Behinds, 5)
               + lag(Team1Behinds, 6)
               ) %>%
        mutate(First_Round = lag(Round, 6)) %>%
        mutate(Accuracy = Sum_Goals/(Sum_Goals + Sum_Behinds)) %>%
        arrange(-Accuracy) %>%
        select(Season, First_Round, Round, Team1, Sum_Goals, Sum_Behinds, Accuracy)


afl_tables_match_results %>%
        distinct(Team1) %>%
        arrange(Team1)