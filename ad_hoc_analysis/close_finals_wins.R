libary(dplyr) 



afl_tables_match_results %>%
        filter(Round == "RR" | Round == "QF" | Round == "EF" | Round == "SF" | Round == "PF") %>%
        filter(Team1FinalMargin <= 6 & Team1FinalMargin > 0) %>%
        ungroup() %>%
        group_by(Season, Team1) %>%
        summarise(count = n()) %>%
        left_join(y = premiers, by = c("Season" = "Year")) %>%
        arrange(-Season) %>%
        #filter(Team1 == Premier) %>%
        filter(count > 1)