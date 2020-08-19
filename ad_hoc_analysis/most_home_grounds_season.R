library(dplyr)

test <- afl_tables_match_results %>%
        ungroup() %>%
        filter(Status == "Original") %>%
        filter(Round != "EF" & Round != "SF" & Round != "QF" & Round != "PF" & Round != "GF") %>%
        group_by(Season, Team1) %>%
        summarise(count = n_distinct(Venue))
        
names(afl_tables_match_results)



test2 <- afl_tables_match_results %>%
        ungroup() %>%
        filter(Status == "Original") %>%
        filter(Team1 == "Kangaroos" & Season == 2002) %>%
        filter(Round != "EF" & Round != "SF" & Round != "QF" & Round != "PF" & Round != "GF") %>%
        distinct(Venue)
