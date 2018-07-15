library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
library(lubridate)
library(ggplot2)

ks2018 <- read_csv("ks-projects-201801.csv") %>% as.tibble()

glimpse(ks2018)
summary(ks2018)

ks18_1 <- ks2018[,-2]
#categorise columns correctly and view levels
factor_names <- c("category", "main_category", "currency", "state", "country")
ks18_1[factor_names] <- lapply(ks18_1[factor_names], factor)
sapply(ks18_1[factor_names], levels)

ks18_1$ID <- as.character(ks18_1$ID)

#usd_pledged is the only category with NAs. It only has 1% NAs
summary(ks18_1)
mean(is.na(ks18_1$`usd pledged`))
sapply(ks18_1, function(x) {sum(is.na(x))})
sapply(ks18_1, function(x) {mean(is.na(x))})


#add a duration column
#floor round the ymd_hms values to ymd. ymd() converts from from "POSIXct" "POSIXt" to "Date" 
# and also removed the UTC part
ks18_1$launched <- ks18_1$launched %>% floor_date(unit = "day") %>% ymd()
ks18_1 <- ks18_1 %>% mutate(duration = deadline - launched)

glimpse(ks18_1)

#which are the highest funded projects
ks18_1 %>% arrange(desc(usd_pledged_real)) %>% head(15)

#which are the most backed projects
ks18_1 %>% arrange(desc(backers)) %>% head(15)

#which are the most popular types of main-category
#rearrange factors in main_category according to number of projects in that category

pop_main_cats <- ks18_1 %>% group_by(main_category) %>% 
  summarise(number_of_projects = n()) %>%
  arrange(desc(number_of_projects))
  
#if you just factor a column without specifying the levels it will create levels in alphabetical order
#and this will be the order of the bars in the charts too

pop_main_cats$main_category <- factor(pop_main_cats$main_category, levels = pop_main_cats$main_category)

pop_main_cats %>% ggplot(aes(main_category, number_of_projects)) +
geom_col(aes(fill = main_category)) +
  ggtitle("Number of Projects per Category") +
  xlab("Main Category") +
  ylab("Number of Projects") +
  theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5), axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

ks18_1 %>% select(category, main_category) %>% head(20)

pop_sub_cats <- ks18_1 %>% group_by(category, main_category) %>% 
  summarise(number_of_projects = n()) %>%
  arrange(desc(number_of_projects)) %>%
  head(20)

pop_sub_cats$category <- factor(pop_sub_cats$category, levels = pop_sub_cats$category)
  
pop_sub_cats %>% ggplot(aes(category, number_of_projects)) +
  geom_col(aes(fill = category)) +
  ggtitle("Number of Projects per Sub-Category") +
  xlab("Sub Category") +
  ylab("Number of Projects") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

#which cats of projects get most money abs, most backers, highest percentage of goal

sum(ks18_1$state == "successful")
sum(ks18_1$pledged == ks18_1$goal)

?wday


