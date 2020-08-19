library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

afl_tables_match_results %>%
        group_by(Season, Team1, Status) %>%
        summarise(sum_for = sum(Team1Points),
                  sum_against = sum(Team2Points),
                  percentage = sum_for/sum_against*100) %>%
        select(-sum_for, -sum_against) %>%
        spread(Status, percentage) %>%
        mutate(point_colour = ifelse(Season == 2018, "2018", "Other")) %>%
        #mutate(Original - Flipped) %>%
        ggplot(aes(x = Original, y = Flipped, group = interaction(Season, Team1), colour = point_colour)) +
        geom_point()
        
test<-

historic_ladders %>%
        group_by(Season, Team1, Status) %>%
        summarise(sum_prem_points = sum(PremPoints),
                  count = n(),
                  ave_prem_points = 0.25*sum_prem_points/count
                  ) %>%
        select(-sum_prem_points, -count) %>%
        spread(Status, ave_prem_points) %>%
        mutate(point_colour = ifelse(Season == 2018, "2018", "Other")) %>%
        mutate(Original - Flipped) %>%
        mutate(interstate = ifelse(Team1 == "Adelaide", "Non-Victorian",
                                   ifelse(Team1 == "Brisbane Bears", "Non-Victorian",
                                          ifelse(Team1 == "Brisbane Lions", "Non-Victorian",
                                                 ifelse(Team1 == "Fremantle", "Non-Victorian",
                                                        ifelse(Team1 == "Greater Western Sydney", "Non-Victorian",
                                                               ifelse(Team1 == "Gold Coast", "Non-Victorian",
                                                                      ifelse(Team1 == "Port Adelaide", "Non-Victorian",
                                                                             ifelse(Team1 == "Sydney", "Non-Victorian",
                                                                                    ifelse(Team1 == "West Coast", "Non-Victorian",
                                                                                           "Victorian")))))))))) %>%
        ggplot(aes(x = Original, y = Flipped, group = interaction(Season, Team1))) +
        geom_point(aes(colour = interstate), size = 4, alpha = 0.5) +
        
        geom_text_repel(aes(label=ifelse((
                (Season == 1940 & Team1 == "Richmond") |
                        (Season == 2018 & Team1 == "Richmond")  |
                        (Season == 2018 & Team1 == "Sydney") |
                        (Season == 2013 & Team1 == "West Coast") 
                #        (Season == 2018) 
                #& (Team1 == "Richmond" | Team1 == "Melbourne")
        ), 
        as.character(paste(Season, Team1, sep = " ")),
        '')
        ), 
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.5, "lines"), 
        size = 5) +
        geom_abline(slope =  1,  linetype = "dashed") +
        theme_minimal() +
        theme(plot.title = element_text(color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(color="#555555", size = 18),
              axis.text.x=element_text(size = 14, angle = 90, vjust = 0.5), 
              axis.text.y=element_text(size = 14), 
              axis.title = element_text(size = 16, face = "bold"), 
              strip.text = element_text(size = 16), 
              plot.caption = element_text(size = 14),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16, face = "bold")
        ) +
        scale_x_continuous(labels = scales::percent,
                           breaks = c(0, 0.25, 0.5, 0.75, 1)) + 
        scale_y_continuous(labels = scales::percent,
                           breaks = c(0, 0.25, 0.5, 0.75, 1)) + 
        scale_color_manual(values = c("#1f77b4", "#f8d159")) +
        labs(title = "Are the Swans more at home away from home?",
             subtitle = "Win percentage in home and away matches | 1897-2018 VFL/AFL home and away seasons",
             caption = "Source: AFL Tables",
             colour = "Team home",             
             x = "Home winning percentage", 
             y = "Away winning percentage")

        