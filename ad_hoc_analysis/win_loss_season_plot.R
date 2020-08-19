#install.packages("extrafont")

#font_import(pattern="[T/t]rebuc")
library(extrafont)
library(dplyr)
library(ggplot2)
library(ggrepel)

historic_ladders %>%
        filter(!is.na(Team2)) %>%
        group_by(Season, Team1) %>%
        summarise(margin_wins = sum(ifelse(Result == "Team1 win", Team1FinalMargin, 0)),
                  margin_losses = sum(ifelse(Result == "Team2 win", Team1FinalMargin, 0)),
                  NoGames = n_distinct(ScoresGameID)
        ) %>%
        filter(NoGames >= 20) %>%
        mutate(colourdot = ifelse(Season == 2018, "2018", "Other")) %>%
        ggplot(aes(y = margin_wins, x = margin_losses)) +
        geom_point(aes(colour = colourdot), size = 4, alpha = 0.5) +
        
        
        geom_text_repel(aes(label=ifelse((
                #(Season == 1975 & Team1 == "Fitzroy") |
                                                  (Season == 2018) & (Team1 == "Geelong" | Team1 == "Port Adelaide" | Team1 == "Adelaide" | Team1 == "Sydney" | Team1 == "Brisbane Lions" | Team1 == "Gold Coast" | Team1 == "Fremantle")
        ), 
        as.character(paste(Team1, sep = " ")),
        '')
        ), 
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.4, "lines"), 
        size = 5) +
        

        geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
        
        theme_minimal() +
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 18),
              axis.text.x=element_text(size = 14, vjust = 0.5), 
              axis.text.y=element_text(size = 14), 
              axis.title = element_text(size = 16, face = "bold"), 
              strip.text = element_text(size = 16), 
              plot.caption = element_text(size = 14),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16, face = "bold")
        ) +
        scale_y_continuous(breaks = c(0, 500, 1000)) +
        scale_color_manual(values = c("#1f77b4", "#f8d159")) +
        labs(title = "Did Collingwood have one of the most middling seasons ever?",
             subtitle = "Sum of margins in losses compared to wins by team | All 22-game VFL/AFL seasons (1970-)",
             caption = "Source: AFL Tables",
             colour = "Season",             
             x = "Sum of margins in all losses (points)", y = "Sum of margins in all wins (points)")




