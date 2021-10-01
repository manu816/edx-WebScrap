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
edx_clean <- na.omit(edx_extended)

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
anti_join(edx_clean_courses, edx_courses) %>% 
  view()

# Showing the courses that are in edx_courses but not in edx edx_clean_courses
anti_join(edx_courses, edx_clean_courses) %>% 
  view()

# Getting number of courses per topic
edx_clean %>% 
  group_by(topics) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  View()







