library(tidyverse)
setwd("C:\\Users\\TOsosanya\\Desktop\\Electricity\\Danish DNOs\\Evonet\\Data\\Asset Bases")
old <- read.csv("old.csv",, check.names = FALSE)
old <- old %>% select(Asset_Number,Year_Of_Manufacture)

new0 <- read.csv("HV Non-Pressurised Cables (Non-CNAIM) Raw Asset Base v2 0608.csv", check.names = FALSE)
new <- new0 %>% select(Asset_Number,Year_Of_Manufacture) %>%  filter(Year_Of_Manufacture == "Default")
new1 <- new %>% left_join(old, by = "Asset_Number")
new1 <- new1 %>% mutate(Year_Of_Manufacture = replace_na(Year_Of_Manufacture.y,"Default")) %>%  select(Asset_Number,Year_Of_Manufacture)
new1a <- new0  %>% left_join(unique(new1), by = "Asset_Number") #%>% select(Asset_Number,Year_Of_Manufacture.x,Year_Of_Manufacture.y)
new1a$Year_Of_Manufacture.x <- as.numeric(as.character(new1a$Year_Of_Manufacture.x))
new1a$Year_Of_Manufacture.y <- as.numeric(as.character(new1a$Year_Of_Manufacture.y))
str(new1a)

new2 <- new1a %>% mutate(Year_Of_Manufacture1 = ifelse(is.na(Year_Of_Manufacture.y), Year_Of_Manufacture.x, Year_Of_Manufacture.y)) %>% 
  rename(Year_Of_Manufacture = Year_Of_Manufacture.x)
#new2$Year_Of_Manufacture <- as.factor(new2$Year_Of_Manufacture)
summary(new2)
new2$Year_Of_Manufacture <- new2$Year_Of_Manufacture1
new2$Year_Of_Manufacture.y <- NULL
new2$Year_Of_Manufacture1 <- NULL

write.csv(new2, "HV Non-Pressurised Cables (Non-CNAIM) Raw Asset Base v3 0608.csv")
