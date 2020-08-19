highest_percentages <- historic_ladders %>%
        filter(RoundsRemaining == 0 &
                       Season <= 2018) %>%
        ungroup() %>%
        group_by(Season) %>%
        summarise(max_perc = max(RunningPercentage))