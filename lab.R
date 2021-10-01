# Importing libraries
library(tidyverse)
library(janitor)

# Reading and assigning the dataset to edx
edx <- read_csv("data/edx_courses.csv")

# Reading and assigning the dataset to edx_extended
edx_extended <- read_csv("data/edx.csv")

# Changing "null" to NA in edx
edx[edx == "null"] = NA

# Changing "null" to NA in edx_extended
edx_extended[edx_extended == "null"] = NA

# Removing all na's from edx_extended
edx_clean <- edx_extended[!is.na(edx_extended$course_name), ]

# Removing the pagination variable in edx
edx$pagination = NULL

# Getting all na's from edx
sum(is.na(edx))

# Getting data na's from data variables in edx
edx_nulls <- edx %>% 
  gather(key = variable, value= value, -c(`web-scraper-order`, `web-scraper-start-url`)) %>% 
  group_by(variable) %>% 
  summarise(total_nas = sum(is.na(value))) %>% 
  ungroup() %>% 
  View()

# Declaring a dataframe with the variable course_name of edx
edx_courses <- edx %>% 
  transmute(course_name = course_name)

# Declaring a dataframe with the variable course_name of edx_clean
edx_clean_courses <- edx_clean %>% 
  transmute(course_name = course_name)

# Showing the courses that are in edx_clean_courses but not in edx edx_courses
unique_courses_edx_clean <- anti_join(edx_clean_courses, edx_courses)

# Showing the courses that are in edx_courses but not in edx edx_clean_courses
unique_courses_edx <- anti_join(edx_courses, edx_clean_courses)


# Getting number of courses per topic
edx_clean_freq_topics <- edx_clean %>% 
  group_by(topics) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) 

edx_clean_first_15 <- edx_clean_freq_topics %>% 
  filter(freq >= 14)

edx_clean_first_15 %>% 
  ggplot() +
  geom_bar(aes(x=freq, y=reorder(topics, freq), fill = topics), stat = 'identity')

edx_clean_other <- edx_clean_freq_topics %>% 
  filter(freq <= 13)

edx_clean_other %>%
  summarise(sum_of_courses = sum(freq))

edx_clean_summary <- edx_clean_first_15 %>% 
  rows_insert(tibble(topics='other', freq=1222))

edx_clean_summary %>% 
  ggplot() +
  geom_bar(aes(x=freq, y=reorder(topics, freq), fill = topics), stat = 'identity') +
  scale_x_log10()


