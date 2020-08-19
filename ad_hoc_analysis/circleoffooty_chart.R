

library(tidyr)
library(dplyr)


circle_chart <- circle %>%
        mutate(combined = paste(Col1,
                                Col2,
                                Col3,
                                Col4,
                                Col5,
                                Col6,
                                Col7,
                                Col8,
                                Col9,
                                Col10,
                                Col11,
                                Col12,
                                Col13,
                                Col14,
                                Col15,
                                Col16,
                                Col17,
                                Col18,
                                sep = '","')) %>%
        mutate(combined = paste('"', combined, '",', sep = "")) %>%
        select(combined) %>%
        mutate(before = gsub(x = combined, pattern = '"Adelaide",.*', replacement = ""),
               after = gsub(x = combined, pattern = '.*"Adelaide",', replacement = ""),
               new = paste('"Adelaide",', after, before, sep = "")) %>%
        select(new) %>%
        distinct(new, .keep_all = TRUE) %>%
        mutate(new = gsub(new, pattern = '\"', replacement = '')) %>%
        dplyr::mutate(id = row_number()) %>%
        separate(new, sep = ",", into = c("Col01",
                                          "Col02",
                                          "Col03",
                                          "Col04",
                                          "Col05",
                                          "Col06",
                                          "Col07",
                                          "Col08",
                                          "Col09",
                                          "Col10",
                                          "Col11",
                                          "Col12",
                                          "Col13",
                                          "Col14",
                                          "Col15",
                                          "Col16",
                                          "Col17",
                                          "Col18")) %>%
        mutate(Col19 = "Adelaide")





circle_locations <- as.data.frame(t(sapply(0:17,function(r)c(cos(2*r*pi/18),sin(2*r*pi/18))))) %>%
        dplyr::mutate(id = row_number()) 

circle_chart_long <- circle_chart %>%
        select(id, Col01:Col19) %>%
        gather("position", Col01:Col19, -id) %>%
        arrange(id) %>%
        dplyr::rename(team = `Col01:Col19`) 

teams <- circle_chart_long %>%
        select(team) %>%
        distinct(team) %>%
        #arrange(team) %>%
        dplyr::mutate(id = row_number()) %>%
        left_join(y = circle_locations, by = c("id"))

circle_chart_long_teams <- circle_chart_long %>%
        ungroup() %>%
        left_join(y = teams, by = c("team")) %>%
        mutate(team_code = case_when(
                team == "Adelaide" ~ "AD",
                team == "Brisbane Lions" ~ "BL",
                team == "Carlton" ~ "CA",
                team == "Collingwood" ~ "CO",
                team == "Essendon" ~ "ES",
                team == "Fremantle" ~ "FR",
                team == "Geelong" ~ "GE",
                team == "Greater Western Sydney" ~ "GW",
                team == "Gold Coast" ~ "GC",
                team == "Hawthorn" ~ "HA",
                team == "Melbourne" ~ "ME",
                team == "North Melbourne" ~ "NM",
                team == "Port Adelaide" ~ "PA",
                team == "Richmond" ~ "RI",
                team == "St Kilda" ~ "SK",
                team == "Sydney" ~ "SY",
                team == "Western Bulldogs" ~ "WB",
                team == "West Coast" ~ "WC"
                
        )
)

library(ggplot2)

#install.packages("extrafont")

#font_import(pattern="[T/t]rebuc")
library(extrafont)
library(dplyr)
library(ggplot2)

circle_chart_long_teams %>%
        ggplot(aes(x = V2, y = V1, group = id.x, label = team_code)) +
        geom_path(aes(colour = position), size = 2) +
        geom_text(size = 4, alpha = 0.7) +
        facet_wrap(~ id.x, nrow = 7, ncol = 13) +
        theme_minimal() +
        theme(plot.title = element_text(color="#555555", face = "bold", size = 56),
              plot.subtitle = element_text(color="#555555", size = 38),
              strip.background = element_blank(),
              strip.text = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(), 
              plot.caption = element_text(color="#555555", size = 20),
              legend.position = "none"
        ) +
        coord_fixed() +
        labs(title = "Footy is beautiful",
             subtitle = "All 86 Circles of Footy to this stage in the season | 2018 AFL season Rounds 1-9",
             caption = "Source: AFL Tables") 


write.csv(circle_chart, file="circle_chart.csv")