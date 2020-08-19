#install.packages("extrafont")

#font_import(pattern="[T/t]rebuc")
library(extrafont)
library(dplyr)
library(ggplot2)
library(ggrepel)

historic_ladders %>%
        filter(!is.na(Team2)) %>%
        mutate(TopBottom = ifelse(SeasonT2LadderRank <= SeasonTeams/2, "TopHalf", "BottomHalf")) %>%
        
        group_by(Season, Team1) %>%
        summarise(PercTopHalf = sum(ifelse(TopBottom == "TopHalf", Team1Points, 0))
                             /sum(ifelse(TopBottom == "TopHalf", Team2Points, 0)),
                  PercBottomHalf = sum(ifelse(TopBottom == "BottomHalf", Team1Points, 0))
                  /sum(ifelse(TopBottom == "BottomHalf", Team2Points, 0)),
                  ratio = PercBottomHalf/PercTopHalf,
                  NoGames = n_distinct(ScoresGameID)
                  ) %>%
        filter(NoGames >= 18) %>%
        mutate(colourdot = ifelse(Season == 2018, "2018", "Other")) %>%
        ggplot(aes(x = PercBottomHalf, y = PercTopHalf)) +
        geom_point(aes(colour = colourdot), size = 4, alpha = 0.5) +

        
        geom_text_repel(aes(label=ifelse((
                # (Season == 1991 & Team1 == "West Coast") |
                #                                   (Season == 1952 & Team1 == "Geelong") |
                #                                   (Season == 2011 & Team1 == "Collingwood") |
                #                                   (Season == 1958 & Team1 == "Fitzroy") |
                #                                   (Season == 1978 & Team1 == "Carlton") |
                #                                   (Season == 1979 & Team1 == "Collingwood") |
                #                                   (Season == 2014 & Team1 == "West Coast") |
                #                                   (Season == 2000 & Team1 == "Essendon") |
                #                                   (Season == 2011 & Team1 == "Geelong") |
                #                                   (Season == 1950 & Team1 == "Essendon") |
                #                                   (Season == 2000 & Team1 == "Essendon") |
                #                                   (Season == 1966 & Team1 == "St Kilda") |
                #                                   (Season == 1942 & Team1 == "St Kilda") |
                #                                   (Season == 1983 & Team1 == "Fitzroy") |
                #                                   (Season == 1995 & Team1 == "Geelong") |
                #                                   (Season == 1911 & Team1 == "Essendon") |
                #                                   (Season == 1910 & Team1 == "Carlton") |
                #                                   (Season == 1908 & Team1 == "Carlton") |
                #                                   (Season == 1909 & Team1 == "South Melbourne") |
                #                                   (Season == 1909 & Team1 == "Melbourne") |
                #                                   (Season == 1911 & Team1 == "Collingwood") |
                                                  (Season == 2017 & Team1 == "Port Adelaide") |
                                                  (Season == 2018) 
                                          #& (Team1 == "Richmond" | Team1 == "Melbourne")
                                          ), 
                                         as.character(paste(Season, Team1, sep = " ")),
                                         '')
        ), 
        box.padding = unit(0.4, "lines"),
        point.padding = unit(0.5, "lines"), 
        size = 5) +
        
        geom_vline(xintercept = 1, colour = "grey") +
        geom_hline(yintercept = 1, colour = "grey") +
        
        theme_minimal() +
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 18),
              axis.text.x=element_text(size = 14, angle = 90, vjust = 0.5), 
              axis.text.y=element_text(size = 14), 
              axis.title = element_text(size = 16, face = "bold"), 
              strip.text = element_text(size = 16), 
              plot.caption = element_text(size = 14),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16, face = "bold")
        ) +
        scale_x_continuous(labels = scales::percent) + 
        scale_y_continuous(labels = scales::percent,
                           breaks = c(0.5, 1, 1.5)) + 
        scale_color_manual(values = c("#1f77b4", "#f8d159")) +
        labs(title = "Is 2018 Melbourne a little Dee-ceiving?",
             subtitle = "Percentage against eventual top versus bottom half-ranked opponents | 18+ game VFL/AFL seasons",
             caption = "N.B. Where season had an odd number of teams, the extra is allocated to 'bottom half'
             Source: AFL Tables",
             colour = "Season",             
             x = "Percentage against bottom half opponents", y = "Percentage against top half opponents")




