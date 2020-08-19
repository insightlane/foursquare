# Look at data structure
str(afl_tables_match_results)

# Check variable classes
sapply(afl_tables_match_results, class)

library(ggplot2)
library(dplyr)

# Scatterplot
afl_tables_match_results %>%
        ungroup() %>%
        filter(Status == "Original" &
                       (Venue == "Kardinia Park" | 
                                Venue == "M.C.G." | 
                                Venue == "Princes Park" | 
                                Venue == "Victoria Park")) %>%
        ggplot(
               aes(Team1Points, Team2Points)) +
        geom_point(aes(color = Venue)) + 
        facet_wrap(~ Venue) + 
        theme(legend.position="none")

# Histogram

afl_tables_match_results %>%
        ungroup() %>%
        group_by(Status) %>%
        mutate(grpmean = mean(Team1Points)) %>%
        #filter(Status == "Original") %>%
        ggplot(
                aes(Team1Points, color = Status)) + 
#        geom_histogram(aes(y=..density..), binwidth = 6, color="black", fill="white") + 
        geom_histogram(aes(y=..density.., fill = Status), binwidth = 6, alpha=0.1, position="identity") + 
        geom_density(alpha=.4) +
        geom_vline(aes(group = Status, color = Status, xintercept = grpmean),
                   linetype = "dashed", size = 1) +
        labs(title = "Total score (points) histogram | 1897-2016 VFL/AFL seasons",
             x = "Team score (points)",
             y = "Density")


# Heat map
afl_tables_match_results %>%
        ggplot(
                aes(Team1Points, Team2Points)) +
        #geom_point() +
        stat_density2d(aes(fill = ..density.., alpha = ..density..), geom = "tile", contour = FALSE) +
        scale_fill_gradient(low = "red", high = "blue") +
        geom_density_2d(colour = "black", alpha = 0.2)

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
        
# Correlogram

library(corrgram)

corrgram(afl_tables_match_results, order=NULL,
         panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")

# Tabplot

install.packages("tabplot")

library(tabplot)
tableplot(afl_tables_match_results)

