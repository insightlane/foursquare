perc130 <- historic_ladders %>%
        ungroup %>%
        group_by(Season, Roundadj) %>%
        filter(RunningPercentage > 130) %>%
        summarise(count = n())

perc125 <- historic_ladders %>%
        ungroup %>%
        group_by(Season, Roundadj) %>%
        filter(RunningPercentage > 125) %>%
        summarise(count = n())

top8wins <- historic_ladders %>%
        filter(LadderRank <= 8) %>%
        ungroup %>%
        group_by(Season, Roundadj) %>%
        summarise(points = sum(RunningPremPoints))

library(dplyr)