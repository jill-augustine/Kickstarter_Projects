ks2016 <- read_csv("ks-projects-201612.csv") %>% as.tibble()

glimpse(ks2016)
summary(ks2016)

#remove the title and blank columns
ks16_1 <- ks2016[, -c(2, 14:17)]
glimpse(ks16_1)
summary(ks16_1)

#categorise columns correctly and view levels
factor_names <- c("main_category", "currency", "state", "country")
ks16_1[factor_names] <- lapply(ks16_1[factor_names], factor)
lapply(ks16_1[factor_names], levels)

ks16_1$ID <- as.character(ks16_1$ID)
ks16_1$backers <- as.integer(ks16_1$backers)

#add a duration column
ks16_1 <- ks16_1 %>% mutate(duration = deadline - launched)

glimpse(ks16_1)
summary(ks16_1)

#calculate the proportion of NAs in columns containing NAs
na_names <- c("category", "deadline", "goal", "launched", "pledged", "backers", "usd pledged")
lapply(ks16_1[na_names], function(x) {mean(is.na(x))})
# only usd pledged contains more than 1% NAs (1.36%)


