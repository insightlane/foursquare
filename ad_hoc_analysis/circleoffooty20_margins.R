library(plyr)
library(dplyr)

# Do this once

foursquare <- read.csv("C:/Local Code/insightlane/foursquare/output_files/foursquare.csv",
                       stringsAsFactors = FALSE)
#circle <- read_csv("C:/Local documents/Analysis/FourSquare/circle.csv")

results2020 <- foursquare %>%
  filter(Season == 2020, Status == "Original") %>%
  filter(Winner != "Draw") %>%
  select(Round, Winner, Loser, AbsFinalMargin)

circle <- results2020 %>%
  left_join(results2020, c("Loser" = "Winner")) %>%
  select(-Round.x, -Round.y)

circle <- plyr::rename(circle, c("Winner"="Col1", 
                                 "Loser"="Col2", 
                                 "Loser.y"="Col3",
                                 "AbsFinalMargin.x"="Mar1",
                                 "AbsFinalMargin.y"="Mar2"))

circle <- circle %>%
  filter(Col1 == "Adelaide")

### 3 

circle<-circle[!(circle$Col1 == circle$Col3
                 | circle$Col2 == circle$Col3
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col3" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col4",
                                 "AbsFinalMargin"="Mar3"))

### 4 

circle<-circle[!(circle$Col1 == circle$Col4
                 | circle$Col2 == circle$Col4
                 | circle$Col3 == circle$Col4
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col4" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col5",
                                 "AbsFinalMargin"="Mar4"))

### 5 

circle<-circle[!(circle$Col1 == circle$Col5
                 | circle$Col2 == circle$Col5
                 | circle$Col3 == circle$Col5
                 | circle$Col4 == circle$Col5
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col5" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col6",
                                 "AbsFinalMargin"="Mar5"))

### 6 

circle<-circle[!(circle$Col1 == circle$Col6
                 | circle$Col2 == circle$Col6
                 | circle$Col3 == circle$Col6
                 | circle$Col4 == circle$Col6
                 | circle$Col5 == circle$Col6
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col6" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col7",
                                 "AbsFinalMargin"="Mar6"))

### 7

circle<-circle[!(circle$Col1 == circle$Col7
                 | circle$Col2 == circle$Col7
                 | circle$Col3 == circle$Col7
                 | circle$Col4 == circle$Col7
                 | circle$Col5 == circle$Col7
                 | circle$Col6 == circle$Col7
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col7" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col8",
                                 "AbsFinalMargin"="Mar7"))

### 8

circle<-circle[!(circle$Col1 == circle$Col8
                 | circle$Col2 == circle$Col8
                 | circle$Col3 == circle$Col8
                 | circle$Col4 == circle$Col8
                 | circle$Col5 == circle$Col8
                 | circle$Col6 == circle$Col8
                 | circle$Col7 == circle$Col8
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col8" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col9",
                                 "AbsFinalMargin"="Mar8"))

### 9

circle<-circle[!(circle$Col1 == circle$Col9
                 | circle$Col2 == circle$Col9
                 | circle$Col3 == circle$Col9
                 | circle$Col4 == circle$Col9
                 | circle$Col5 == circle$Col9
                 | circle$Col6 == circle$Col9
                 | circle$Col7 == circle$Col9
                 | circle$Col8 == circle$Col9
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col9" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col10",
                                 "AbsFinalMargin"="Mar9"))

### 10

circle<-circle[!(circle$Col1 == circle$Col10
                 | circle$Col2 == circle$Col10
                 | circle$Col3 == circle$Col10
                 | circle$Col4 == circle$Col10
                 | circle$Col5 == circle$Col10
                 | circle$Col6 == circle$Col10
                 | circle$Col7 == circle$Col10
                 | circle$Col8 == circle$Col10
                 | circle$Col9 == circle$Col10
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col10" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col11",
                                 "AbsFinalMargin"="Mar10"))

### 11

circle<-circle[!(circle$Col1 == circle$Col11
                 | circle$Col2 == circle$Col11
                 | circle$Col3 == circle$Col11
                 | circle$Col4 == circle$Col11
                 | circle$Col5 == circle$Col11
                 | circle$Col6 == circle$Col11
                 | circle$Col7 == circle$Col11
                 | circle$Col8 == circle$Col11
                 | circle$Col9 == circle$Col11
                 | circle$Col10 == circle$Col11
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col11" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col12",
                                 "AbsFinalMargin"="Mar11"))

### 12

circle<-circle[!(circle$Col1 == circle$Col12
                 | circle$Col2 == circle$Col12
                 | circle$Col3 == circle$Col12
                 | circle$Col4 == circle$Col12
                 | circle$Col5 == circle$Col12
                 | circle$Col6 == circle$Col12
                 | circle$Col7 == circle$Col12
                 | circle$Col8 == circle$Col12
                 | circle$Col9 == circle$Col12
                 | circle$Col10 == circle$Col12
                 | circle$Col11 == circle$Col12
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col12" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col13",
                                 "AbsFinalMargin"="Mar12"))

### 13

circle<-circle[!(circle$Col1 == circle$Col13
                 | circle$Col2 == circle$Col13
                 | circle$Col3 == circle$Col13
                 | circle$Col4 == circle$Col13
                 | circle$Col5 == circle$Col13
                 | circle$Col6 == circle$Col13
                 | circle$Col7 == circle$Col13
                 | circle$Col8 == circle$Col13
                 | circle$Col9 == circle$Col13
                 | circle$Col10 == circle$Col13
                 | circle$Col11 == circle$Col13
                 | circle$Col12 == circle$Col13
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col13" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col14",
                                 "AbsFinalMargin"="Mar13"))

### 14

circle<-circle[!(circle$Col1 == circle$Col14
                 | circle$Col2 == circle$Col14
                 | circle$Col3 == circle$Col14
                 | circle$Col4 == circle$Col14
                 | circle$Col5 == circle$Col14
                 | circle$Col6 == circle$Col14
                 | circle$Col7 == circle$Col14
                 | circle$Col8 == circle$Col14
                 | circle$Col9 == circle$Col14
                 | circle$Col10 == circle$Col14
                 | circle$Col11 == circle$Col14
                 | circle$Col12 == circle$Col14
                 | circle$Col13 == circle$Col14
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col14" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col15",
                                 "AbsFinalMargin"="Mar14"))


### 15

circle<-circle[!(circle$Col1 == circle$Col15
                 | circle$Col2 == circle$Col15
                 | circle$Col3 == circle$Col15
                 | circle$Col4 == circle$Col15
                 | circle$Col5 == circle$Col15
                 | circle$Col6 == circle$Col15
                 | circle$Col7 == circle$Col15
                 | circle$Col8 == circle$Col15
                 | circle$Col9 == circle$Col15
                 | circle$Col10 == circle$Col15
                 | circle$Col11 == circle$Col15
                 | circle$Col12 == circle$Col15
                 | circle$Col13 == circle$Col15
                 | circle$Col14 == circle$Col15
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col15" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col16",
                                 "AbsFinalMargin"="Mar15"))

### 16

circle<-circle[!(circle$Col1 == circle$Col16
                 | circle$Col2 == circle$Col16
                 | circle$Col3 == circle$Col16
                 | circle$Col4 == circle$Col16
                 | circle$Col5 == circle$Col16
                 | circle$Col6 == circle$Col16
                 | circle$Col7 == circle$Col16
                 | circle$Col8 == circle$Col16
                 | circle$Col9 == circle$Col16
                 | circle$Col10 == circle$Col16
                 | circle$Col11 == circle$Col16
                 | circle$Col12 == circle$Col16
                 | circle$Col13 == circle$Col16
                 | circle$Col14 == circle$Col16
                 | circle$Col15 == circle$Col16
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col16" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col17",
                                 "AbsFinalMargin"="Mar16"))

### 17

circle<-circle[!(circle$Col1 == circle$Col17
                 | circle$Col2 == circle$Col17
                 | circle$Col3 == circle$Col17
                 | circle$Col4 == circle$Col17
                 | circle$Col5 == circle$Col17
                 | circle$Col6 == circle$Col17
                 | circle$Col7 == circle$Col17
                 | circle$Col8 == circle$Col17
                 | circle$Col9 == circle$Col17
                 | circle$Col10 == circle$Col17
                 | circle$Col11 == circle$Col17
                 | circle$Col12 == circle$Col17
                 | circle$Col13 == circle$Col17
                 | circle$Col14 == circle$Col17
                 | circle$Col15 == circle$Col17
                 | circle$Col16 == circle$Col17
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col17" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col18",
                                 "AbsFinalMargin"="Mar17"))

### 18

circle<-circle[!(circle$Col1 == circle$Col18
                 | circle$Col2 == circle$Col18
                 | circle$Col3 == circle$Col18
                 | circle$Col4 == circle$Col18
                 | circle$Col5 == circle$Col18
                 | circle$Col6 == circle$Col18
                 | circle$Col7 == circle$Col18
                 | circle$Col8 == circle$Col18
                 | circle$Col9 == circle$Col18
                 | circle$Col10 == circle$Col18
                 | circle$Col11 == circle$Col18
                 | circle$Col12 == circle$Col18
                 | circle$Col13 == circle$Col18
                 | circle$Col14 == circle$Col18
                 | circle$Col15 == circle$Col18
                 | circle$Col16 == circle$Col18
                 | circle$Col17 == circle$Col18
                 
),]

circle <- circle %>%
  left_join(results2020, c("Col18" = "Winner")) %>%
  select(-Round)

circle <- plyr::rename(circle, c("Loser"="Col19",
                                 "AbsFinalMargin"="Mar18"))

#####################

circle <- circle[(circle$Col1 == circle$Col19
),]

circle <- circle %>%
  mutate(total_margin = Mar1 + Mar2 + Mar3 + Mar4 + Mar5 + Mar6 + Mar7 + Mar8 + Mar9 + Mar10 + Mar11 + Mar12 + Mar13 + Mar14 + Mar15 + Mar16 + Mar17 + Mar18)

write.csv(circle, file="circle19.csv")
