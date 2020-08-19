

####

# Splits into seasons, teams and points in previous season and current season per game played

slopegraph0 <- historic_ladders %>%
        select(Season, Roundadj, Round, Team1, Played, RunningPremPoints)


slopegraph1 <- slopegraph0

slopegraph0$SGseason <- slopegraph0$Season
slopegraph1$SGseason <- slopegraph1$Season + 1

slopegraph0$SGSeasonLT <- "This"
slopegraph1$SGSeasonLT <- "Last"

slopegraph <- rbind(slopegraph0, slopegraph1)

write.csv(slopegraph, file="slopegraph.csv")

#######

install.packages("tidyr")

library(dplyr)

slopegraphc0 <- historic_ladders %>%
        select(Season, Roundadj, Round, Team1, Played, RunningPremPoints)

slopegraphc0$Team1 <- ifelse((slopegraphc0$Team1 == "South Melbourne") | (slopegraphc0$Team1 == "Sydney"), "South Melbourne/Sydney",
                             ifelse((slopegraphc0$Team1 == "North Melbourne") | (slopegraphc0$Team1 == "Kangaroos"), "North Melbourne/Kangaroos",
                                    ifelse((slopegraphc0$Team1 == "Footscray") | (slopegraphc0$Team1 == "Western Bulldogs"), "Footscray/Western Bulldogs",
                                           slopegraphc0$Team1)
                                    )
                            )

slopegraphc1 <- slopegraphc0

slopegraphc0$SeasonPost <- slopegraphc0$Season + 1

slopegraphc <- merge(x = slopegraphc0, y = slopegraphc1, 
                     by.x = c("SeasonPost", "Team1", "Played"), 
                     by.y = c("Season", "Team1", "Played"), 
                     all.x = TRUE)

slopegraphcount <- slopegraphc %>%
        group_by(Played, RunningPremPoints.x, RunningPremPoints.y) %>%
        summarise(Count = n())

slopegraphfinal <- merge(x = slopegraphc, y = slopegraphcount, 
                     by = c("Played", "RunningPremPoints.x", "RunningPremPoints.y"),
                     all.x = TRUE)

slopegraphfinal$Difference <- slopegraphfinal$RunningPremPoints.x - slopegraphfinal$RunningPremPoints.y

write.csv(slopegraphfinal, file="slopegraphfinal.csv")
