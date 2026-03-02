#install.packages(c("readr", "tidyverse", "lubridate"))

library(readr)
library(tidyverse)
library(lubridate)

# load data
#wvs_data <- read_csv("wvs.csv")
wvs <- read_csv("wvs.csv")
head(wvs)

#isolate Canada and specefic survey year
canada <- wvs %>% filter(S003 == 124)
#canada <- canada %>% mutate(survey_year = as.numeric(str_sub(S012, -4, -1)))

# create categories by year, by age (generation)
canada <- canada %>% mutate(
  age = X003, 
  survey_year = S020,
  birth_year = survey_year - age,
  cohort = cut(birth_year, 
               breaks = c(1900, 1945,1965, 1980, 1996, 2012),
               labels = c("Silent", "Boomer", "GenX", "Millenial", "GenZ"))
)


# building our index on authoritarian values via different questions/variables in the survey
auth_parts <- canada %>% select(A124_02, A124_09, A124_08, E018, E039)
#( authroitarian threat index : auth_parts <- canada %>% select(Q235, Q45, Q237)
canada <- canada %>% mutate(auth_index = rowMeans(auth_parts, na.rm = TRUE)) %>%
  summarise(mean_auth = mean(auth_index, na.rm = TRUE))



#plot
plot(age ~ mean_auth, data= canada, pch=19,col="blue")



# Load packages
library(readr)
library(dplyr)

# 1. Load your CSV
wvs <- read_csv("wvs.csv")

# 2. Filter to Canada (country code 124)
canada <- wvs[wvs$S003 == 124, ]

# 3. Construct survey year and birth year
# (Age will now come from X003, not from birth_year)
canada$survey_year <- canada$S010
canada$birth_year  <- ifelse(canada$X002 < 1900 | canada$X002 > 2025, NA, canada$X002)

# 4. Use X003 as the age variable
canada$age <- canada$X003

# 5. Build authoritarianism index
# Select the authoritarianism items you used earlier
auth_parts <- canada[, c("A124_02", "A124_09", "A124_08", "E018", "E039")]

# Compute row means
canada$auth_index <- rowMeans(auth_parts, na.rm = TRUE)

# 6. Compute mean authoritarianism for each age (NO PIPES)
mean_by_age <- aggregate(auth_index ~ age,
                         data = canada,
                         FUN = mean,
                         na.rm = TRUE)

# 7. View the result
print(mean_by_age)

plot(mean_by_age$age,
     mean_by_age$auth_index,
     type = "l",
     xlab = "Age (X003)",
     ylab = "Mean Authoritarianism Score",
     main = "Authoritarianism by Age in Canada",
     col = "blue",
     lwd = 2)
axis( side = 1, at = seq(min(mean_by_age$age, na.rm = TRUE),
           max(mean_by_age$age, na.rm = TRUE),
           by = 5)
)
