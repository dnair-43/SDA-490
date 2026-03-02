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
canada <- canada %>% mutate(auth_index = rowMeans(auth_parts, na.rm = TRUE))

# run the linear mixed effects model 
library(lme4) 
apc_model <- lmer(auth_index ~ age + (1 | cohort) + (1 | survey_year), 
                  data = canada)
summary(apc_model)

## Visualizations
# cohort differences 
canada %>% 
  group_by(cohort) %>%
  #summarise (mean_auth = auth_index, na.rm = TRUE) %>%
  ggplot(aes(cohort, auth_index)) + geom_col() + 
  labs(title = "Authoritarianism by Cohort in Canada")

# Period trends
canada %>%
  group_by(cohort) %>% 
  summarise(mean_auth = mean(auth_index, na.rm = TRUE)) %>%
  ggplot(aes(cohort, mean_auth)) + geom_col() +
  labs(titate = "Authoritarianism Over Time in Canada")
O

change x axia intervals 