biggest_yoy_changes <- historic_ladders %>%
        filter(RoundsRemaining == 0) %>%
        ungroup() %>%
        group_by(Team1) %>%
        mutate(delta_for = RunningFor - lag(RunningFor),
               delta_against = RunningAgainst - lag(RunningAgainst),
               delta_ave_for = AverageFor - lag(AverageFor),
               delta_ave_against = AverageAgainst - lag(AverageAgainst),
               perc_delta_ave_for = delta_ave_for/lag(AverageFor),
               perc_delta_ave_against = delta_ave_against/lag(AverageAgainst),
               delta_percentage = RunningPercentage - lag(RunningPercentage),
               delta_season = Season - lag(Season)) %>%
filter(delta_season == 1)

biggest_yoy_changes %>%
        select(delta_percentage, Team1, Season) %>%
        arrange(delta_percentage) %>%
        filter(Season>= 1919)