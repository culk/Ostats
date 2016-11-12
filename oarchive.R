library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

# FUTURE: add support for rank data past 2010
#   links: https://www.orienteeringusa.org/rankings/archive.php
#   ranks has: Rank, Name, Club, # Events, Pts, Time, Rank, Course, Class
#   started breaking it out by course in 2003
#   started doing intermediate rankings in 2006

o_archive_dates <- function() {
  # this is going to be a huge pain because of inconsistent format
  url <- "https://www.orienteeringusa.org/rankings/archive.php"
  page <- read_html(url)
  years <- page %>%
    html_nodes("ul#treemenu1.treeview > li") %>%
    html_text() %>%
    substr(1, 13)
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as_tibble()
  result_links <- links %>%
    filter(str_detect(value, "12_31"))
  return(NULL)
}

o_archive_file <- function(course, rank_year) {
  # error checking and conversion for course selections
  courses <- list(
    # crs_num | course
    "70" = "blue",
    "60" = "red",
    "50" = "green",
    "40" = "brown",
    "30" = "orange",
    "20" = "yellow",
    "10" = "white"
  )
  if(str_to_lower(course) %in% courses) {
    course_name <- course
  } else if (as.character(course) %in% names(courses)) {
    course_name <- courses[as.character(course)] %>% as.character()
  } else {
    stop(str_c("Course '", course, 
               "' is not a valid course. Try 'blue' or '70'."))
  }
  # 2002 and earlier are not supported yet
  filename <- switch(as.character(rank_year),
                     "2009" = str_c("2009_12_31_",course_name),
                     "2008" = str_c("2008_eoy_",course_name),
                     "2007" = str_c("2007_",course_name,"_official"),
                     "2006" = str_c("2006_",course_name,"_official"),
                     "2005" = str_c("2005_",course_name,"_official"),
                     "2004" = str_c("2004_",course_name,"_official"),
                     "2003" = str_c("2003_",course_name,"_official"))
  return(filename)
}

# supports 2009 only
o_archive_course <- function(course, rank_year) {
  filename <- o_archive_file(course, rank_year)
  
  #### Given filename, return one data frame for that course with correct columns
  url <- str_c("https://www.orienteeringusa.org/rankings/rslt/",
               filename, ".html")
  page <- read_html(url)
  rank <- page %>%
    html_table() %>%
    .[[1]] %>%
    as_tibble()
  if(ncol(rank) > 14) {
    new_names <- rank[10, -(15:ncol(rank))]
  } else {
    new_names <- rank[10, ]
  }
  names(rank)[1:ncol(new_names)] <- new_names
  # rank <- rank %>%
  #   mutate() %>%
  #   select() %>%
  
  ####
  
  #### change variable types, filter out bad data
  rank <- rank %>%
    select(-starts_with("USOF"), -starts_with("X"), -Patch) %>%
    rename(Overall_Rank = `Overall Rank`, Count_Events = `# of Events`) %>%
    unite(Name, `First Name`, Surname, sep = " ") %>%
    mutate(Overall_Rank = as.integer(Overall_Rank),
           Birth_Year = rank_year - as.numeric(Age),
           Score = as.numeric(Score),
           Count_Events = as.integer(Count_Events),
           Time = ifelse(Score != 0, str_c(Time, ":00"), ""),
           Age = NULL) %>%
    filter(!is.na(Count_Events))
  if(course == "blue") {
    rank <- mutate(rank, Class = "M-21+", Class_Rank = Overall_Rank)
  } else {
    rank <- rank %>%
      rename(Class_Rank = `Class Rank`) %>%
      mutate(Class_Rank = as.integer(Class_Rank))
  }
  rank <- select(rank, Class_Rank, Overall_Rank, Name, Birth_Year,
                 Club, Score, Time, Count_Events, Class)
  return(rank)
}