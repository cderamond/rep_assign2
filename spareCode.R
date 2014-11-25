# loading libraries
library(ggplot2, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(stringr, warn.conflicts = F)


# data load
data <- read.csv("./data/repdata-data-StormData.csv", stringsAsFactors=FALSE)
# create small table with the factors for number conversion
exp <- data.frame(exp = c("B", "K", "M", "H"), 
                  multiplier = c(1E9, 1E3, 1E6, 1E2), 
                  stringsAsFactors = F)
# we make inner join in order to filter bad input and transform damage to real number
data <- inner_join(data, exp, by = c("PROPDMGEXP" = "exp")) %>%
        mutate(PROPDMG = PROPDMG*multiplier) %>%
        select(-multiplier, -1)
data <- inner_join(data, exp, by = c("CROPDMGEXP" = "exp")) %>%
        mutate(CROPDMG = CROPDMG*multiplier) %>%
        select(-multiplier, -1) #this should've been a function, but well...
# moving on totals and date figures
data <- data %>%
        separate(BGN_DATE, c("date", "garbage"), sep = -8, remove = T) %>%
        mutate(date = mdy(date),
               year = year(date),
               month = month(date),
               quarter = quarters(date, abbreviate = T),
               Total_HealthCases = FATALITIES + INJURIES, 
               Total_EconDmg = CROPDMG + PROPDMG) %>%
        select(-garbage)

# distRaw <- as.data.table(data[, c("EVTYPE", "Total_EconDmg", "Total_HealthCases")])
# distMatrix <- dist(distRaw[, list(sum(Total_EconDmg), sum(Total_HealthCases)), by = EVTYPE])
# clust <- hclust(distMatrix)
# kdata <- kmeans(select(distRaw2, 2:3), centers = 20)

gr <- ggplot(data = dataFinal)
qplot(EVTYPE, EconDmg, data = head(dataFinal[ order(-EconDmg)], n= 10), geom = "bar", stat = "identity") +
        qplot(EVTYPE, Healthcases, data = head(dataFinal[ order(-HealthCases)], n= 10), geom = "bar", stat = "identity")
