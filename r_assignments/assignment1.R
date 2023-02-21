# install.packages("tidyverse")
# install.packages("stringr")
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library("stringr")                 

data <- read_csv("C:/Users/yang/Documents/code/tutoring/Saige_r_and_python/r_assignments/chipotle_reduced.csv")

chars_to_remove <- c("{", "}", "\\")

function1 <- function(store){ 
  data %>% 
  select(placekey, popularity_by_day) %>% 
  mutate(clean_popular = gsub("\\{|\\}", "", popularity_by_day)) %>%
  mutate(V2 = strsplit(as.character(clean_popular), ",")) %>% 
  unnest(V2) %>%
  separate(V2, c("day", "count"), ":") %>%
  mutate(clean_day = gsub('"', '', day)) %>%
  mutate(count = as.numeric(count)) %>%
  select(placekey, clean_day, count) %>%
  filter(placekey == store)
}

output1 <- function1("zzw-222@5vg-nwf-mp9")
  
function2 <- function(input_dataframe) {
    input_dataframe %>%
    arrange(desc(count)) %>%
    top_n(1) %>%
    select(clean_day)
}

function2(output1)  


