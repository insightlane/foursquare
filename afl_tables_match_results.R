#***********************************************************************************************
# TITLE:        VFL/AFL match results 1897- from AFL Tables big list
#
# DESCRIPTION:  Scrape of results from AFL Tables and transformation of raw score data into useable formats for VFL/AFL matches (1897-)
#
# AUTHOR:       InsightLane
#
# CREATED:      April 2016
# MODIFIED:   
#
# INPUTS:       Match result data from AFL Tables (http://afltables.com/afl/stats/biglists/bg3.txt)
#
# OUTPUTS:      ...
#
# STEPS:
#				1. Download extract data from AFL Tables
#				2. Transform data into a shape where all team-match combinations are listed (i.e. both home and away views)
#				3. Make hard coded fixes to team names
#				4. Cleanse data into usable score fields
#				5. Create additional columns for other match attributes
#				6. Import list of premiers from flat file and join to table
#				7. Add legend for FourSquare Tableau file at a season-team level
#				8. Write output file out to CSV format
#       9. Export data
#				
#				(N.B. The transformations are mostly written in base R and have only been converted to the tidyverse (dplyr) in parts
#
#***********************************************************************************************

library(dplyr)
rm(list = ls())

# -----------------------------------------------------------------------------------------------
# 1. Download extract data from AFL Tables
# -----------------------------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

### Import scores from big list

# Import scores from http://afltables.com/afl/stats/biglists/bg3.txt, using fixed widths

#rm(homescores, awayscores, afl_tables_match_results)

scores <- read.fwf("http://afltables.com/afl/stats/biglists/bg3.txt", 
                   skip = 2, header = FALSE, 
                   widths = c(7, 17, 5, 18, 17, 18, 18, 18), 
                   col.names = c("ScoresGameID","Date","Round","Team1",
                                 "Team1Score","Team2","Team2Score","Venue"))

scores$Roundadj <- ifelse((grepl("R", scores$Round) == TRUE), as.numeric(substring(scores$Round, 2)), NA)

# -----------------------------------------------------------------------------------------------
# 2. Transform data into a shape where all team-match combinations are listed (i.e. both home and away views)
# -----------------------------------------------------------------------------------------------

originalscores <- data.frame(scores, Status = "Original")

flippedscores <- originalscores[ , c("ScoresGameID", "Date", "Round", "Team2", "Team2Score", "Team1", "Team1Score", "Venue", "Roundadj", "Status")]
flippedscores$Status <- "Flipped"
colnames(flippedscores) <- c("ScoresGameID","Date","Round","Team1",
                             "Team1Score","Team2","Team2Score","Venue", "Roundadj", "Status")

afl_tables_match_results <- rbind(originalscores, flippedscores)

rm(originalscores)
rm(flippedscores)

# -----------------------------------------------------------------------------------------------
# 3. Make hard coded fixes to team names
# -----------------------------------------------------------------------------------------------

# Fix GW Sydney to Greater Western Sydney

afl_tables_match_results$Team1 <- ifelse((afl_tables_match_results$Team1 == "GW Sydney"), "Greater Western Sydney", afl_tables_match_results$Team1)
afl_tables_match_results$Team2 <- ifelse((afl_tables_match_results$Team2 == "GW Sydney"), "Greater Western Sydney", afl_tables_match_results$Team2)

# -----------------------------------------------------------------------------------------------
# 4. Cleanse data into usable score fields
# -----------------------------------------------------------------------------------------------

# Split scores into goals/behinds/points for both teams

team1scores <- data.frame(do.call('rbind', strsplit(as.character(afl_tables_match_results$Team1Score),"\\.")))
team2scores <- data.frame(do.call('rbind', strsplit(as.character(afl_tables_match_results$Team2Score),"\\.")))

# Convert new goals/behinds/points columns into numeric for both teams

team1scores <- data.frame("Team1Goals" = as.numeric(as.character(team1scores$X1)), 
                         "Team1Behinds" = as.numeric(as.character(team1scores$X2)), 
                         "Team1Points" = as.numeric(as.character(team1scores$X3)))
team2scores <- data.frame("Team2Goals" = as.numeric(as.character(team2scores$X1)), 
                         "Team2Behinds" = as.numeric(as.character(team2scores$X2)), 
                         "Team2Points" = as.numeric(as.character(team2scores$X3)))

# Combine goals/behind/points into new data frame 

afl_tables_match_results <- data.frame(afl_tables_match_results, 
                        Dateadj = as.Date(scores$Date, "%d-%b-%Y"), 
                        Season = as.numeric(format(as.Date(scores$Date, "%d-%b-%Y"), "%Y")), 
                        team1scores, team2scores)

rm(team1scores)
rm(team2scores)

afl_tables_match_results$Team1 <- trimws(as.character(afl_tables_match_results$Team1), which = "right")
afl_tables_match_results$Team2 <- trimws(as.character(afl_tables_match_results$Team2), which = "right")
afl_tables_match_results$Round <- trimws(as.character(afl_tables_match_results$Round), which = "right")
afl_tables_match_results$Venue <- trimws(as.character(afl_tables_match_results$Venue), which = "right")

# -----------------------------------------------------------------------------------------------
# 5. Create additional columns for other match attributes
# -----------------------------------------------------------------------------------------------

# Create new column for margin (relative to team 1) for each match

afl_tables_match_results$Team1FinalMargin <- afl_tables_match_results$Team1Points - afl_tables_match_results$Team2Points 

# Create new column for margin (absolute) for each match

afl_tables_match_results$AbsFinalMargin <- abs(afl_tables_match_results$Team1Points - afl_tables_match_results$Team2Points)

# Create new column for result (team1 win/team2 win/draw) for each match

afl_tables_match_results$Result <- ifelse((afl_tables_match_results$Team1FinalMargin > 0), "Team1 win",
                           ifelse((afl_tables_match_results$Team1FinalMargin < 0), "Team2 win",
                                  ifelse((afl_tables_match_results$Team1FinalMargin == 0), "Draw",
                                         NA)))

# Create new column for victor (or draw) for each match

afl_tables_match_results$Winner <- ifelse((afl_tables_match_results$Team1FinalMargin > 0), as.character(afl_tables_match_results$Team1),
                           ifelse((afl_tables_match_results$Team1FinalMargin < 0),  as.character(afl_tables_match_results$Team2),
                                  ifelse((afl_tables_match_results$Team1FinalMargin == 0), "Draw",
                                         NA)))

afl_tables_match_results$Loser <- ifelse((afl_tables_match_results$Team1FinalMargin > 0), as.character(afl_tables_match_results$Team2),
                          ifelse((afl_tables_match_results$Team1FinalMargin < 0),  as.character(afl_tables_match_results$Team1),
                                 ifelse((afl_tables_match_results$Team1FinalMargin == 0), "Draw",
                                        NA)))

# -----------------------------------------------------------------------------------------------
# 6. Import list of premiers from flat file and join to table
# -----------------------------------------------------------------------------------------------

premiers <- read.csv("C:/Local Code/insightlane/shared_tables/premiers.csv")

afl_tables_match_results <- merge(x = afl_tables_match_results, y = premiers, 
                   by.x = c("Season"), 
                   by.y = c("Year"), 
                   all.x = TRUE)
				   
# -----------------------------------------------------------------------------------------------
# 7. Add legend for FourSquare Tableau file at a season-team level
# -----------------------------------------------------------------------------------------------

afl_tables_match_results$Legend <- ifelse((afl_tables_match_results$Season == 2021), "2021 team",
                           ifelse((afl_tables_match_results$Team1 == afl_tables_match_results$Premier), "Premier",
                                  ifelse((afl_tables_match_results$Team1 == afl_tables_match_results$Runnerup), "Runner-up",
                                         ifelse((afl_tables_match_results$Team1 == afl_tables_match_results$Last), "Wooden spoon",
                                                "Other"))))

afl_tables_match_results <- afl_tables_match_results[order(afl_tables_match_results$ScoresGameID, afl_tables_match_results$Status), ]

# -----------------------------------------------------------------------------------------------
# 8. Write output file out to CSV format
# -----------------------------------------------------------------------------------------------

write.csv(afl_tables_match_results, file="C:/Local Code/insightlane/foursquare/output_files/afl_tables_match_results.csv")



