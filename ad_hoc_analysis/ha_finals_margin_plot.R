library(extrafont)
library(dplyr)
library(ggplot2)
library(ggrepel)

afl_tables_match_results %>%
        filter(Status == "Original") %>%
        group_by(Season) %>%
        summarise(
                sumFmargin = sum(ifelse((Round == "QF" | Round == "EF" | Round == "SF" | Round == "PF" | Round == "GF"), AbsFinalMargin, 0)),
                countFgames = sum(ifelse((Round == "QF" | Round == "EF" | Round == "SF" | Round == "PF" | Round == "GF"), 1, 0)),
                aveFmargin = sumFmargin/countFgames,
                sumHAmargin = sum(ifelse((Round == "QF" | Round == "EF" | Round == "SF" | Round == "PF" | Round == "GF"), 0, AbsFinalMargin)),
                countHAgames = sum(ifelse((Round == "QF" | Round == "EF" | Round == "SF" | Round == "PF" | Round == "GF"), 0, 1)),
                aveHAmargin = sumHAmargin/countHAgames
        ) %>%
        ggplot(aes(x = aveHAmargin, y = aveFmargin)) +
        geom_point() + 
        geom_text_repel(aes(label=ifelse((
                ((aveFmargin - aveHAmargin) > 10) |
                        ((aveFmargin - aveHAmargin) < -15) |
                        ((aveFmargin + aveHAmargin) < 45) |
                        ((aveFmargin + aveHAmargin) > 75)
        ), 
        as.character(paste(Season, sep = " ")),
        '')
        ), 
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.4, "lines"), 
        size = 5) + 
        geom_abline(slope = 1, intercept = 0)
