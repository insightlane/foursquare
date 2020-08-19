library(plyr)
library(dplyr)

# Do this once

foursquare <- read.csv("C:/Local documents/Analysis/FourSquare/foursquare.csv",
                       stringsAsFactors = FALSE)
#circle <- read_csv("C:/Local documents/Analysis/FourSquare/circle.csv")

results2018 <- foursquare %>%
        filter(Season == 2018, Status == "Original") %>%
        select(Round, Winner, Loser)

circle <- results2018 %>%
        left_join(results2018, c("Loser" = "Winner")) %>%
        select(-Round.x, -Round.y)

circle <- rename(circle, c("Winner"="Col1", 
                         "Loser"="Col2", 
                         "Loser.y"="Col3"))

### 3 

circle<-circle[!(circle$Col1 == circle$Col3
                | circle$Col2 == circle$Col3

        ),]

circle <- circle %>%
        left_join(results2018, c("Col3" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col4"))

### 4 

circle<-circle[!(circle$Col1 == circle$Col4
               | circle$Col2 == circle$Col4
               | circle$Col3 == circle$Col4
               
),]

circle <- circle %>%
        left_join(results2018, c("Col4" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col5"))

### 5 

circle<-circle[!(circle$Col1 == circle$Col5
               | circle$Col2 == circle$Col5
               | circle$Col3 == circle$Col5
               | circle$Col4 == circle$Col5
               
),]

circle <- circle %>%
        left_join(results2018, c("Col5" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col6"))

### 6 

circle<-circle[!(circle$Col1 == circle$Col6
               | circle$Col2 == circle$Col6
               | circle$Col3 == circle$Col6
               | circle$Col4 == circle$Col6
               | circle$Col5 == circle$Col6
               
),]

circle <- circle %>%
        left_join(results2018, c("Col6" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col7"))

### 7

circle<-circle[!(circle$Col1 == circle$Col7
               | circle$Col2 == circle$Col7
               | circle$Col3 == circle$Col7
               | circle$Col4 == circle$Col7
               | circle$Col5 == circle$Col7
               | circle$Col6 == circle$Col7
               
),]

circle <- circle %>%
        left_join(results2018, c("Col7" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col8"))

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
        left_join(results2018, c("Col8" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col9"))

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
        left_join(results2018, c("Col9" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col10"))

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
        left_join(results2018, c("Col10" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col11"))

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
        left_join(results2018, c("Col11" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col12"))

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
        left_join(results2018, c("Col12" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col13"))

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
        left_join(results2018, c("Col13" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col14"))

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
        left_join(results2018, c("Col14" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col15"))


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
        left_join(results2018, c("Col15" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col16"))

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
        left_join(results2018, c("Col16" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col17"))

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
        left_join(results2018, c("Col17" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col18"))

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
        left_join(results2018, c("Col18" = "Winner")) %>%
        select(-Round)

circle <- rename(circle, c("Loser"="Col19"))

#####################

circle <- circle[(circle$Col1 == circle$Col19
),]

write.csv(circle, file="circle.csv")
