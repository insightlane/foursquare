library(dplyr)
library(ggplot2)


historic_ladders %>%
        filter(Season >= 2016, Team1 == "Geelong") %>%
        ggplot(aes(x = Roundadj, y = PreRankTeam2, color = Team1FinalMargin, label = Team2)) +
        geom_point(aes(size = 3)) +
        facet_wrap(~ Season) +
        scale_colour_gradient2(limits=c(-120, 120)) +
        scale_y_continuous(trans = "reverse")