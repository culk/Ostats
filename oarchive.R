library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

# FUTURE: add support for rank data past 2010
#   links: https://www.orienteeringusa.org/rankings/archive.php
#   ranks has: Rank, Name, Club, # Events, Pts, Time, Rank, Course, Class
#   started breaking it out by course in 2003
#   started doing intermediate rankings in 2006

# early attempt, not a good start
# o_archive_dates <- function() {
#   # this is going to be a huge pain because of inconsistent format
#   url <- "https://www.orienteeringusa.org/rankings/archive.php"
#   page <- read_html(url)
#   years <- page %>%
#     html_nodes("ul#treemenu1.treeview > li") %>%
#     html_text() %>%
#     substr(1, 13)
#   links <- page %>%
#     html_nodes("a") %>%
#     html_attr("href") %>%
#     as_tibble()
#   result_links <- links %>%
#     filter(str_detect(value, "12_31"))
#   return(NULL)
# }

o_archive_data <- function(course, rank_year) {
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
  url <- str_c("https://www.orienteeringusa.org/rankings/rslt/",
               filename, ".html")
  page <- read_html(url)
  df <- page %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    as_tibble()
  return(df)
}

# supports 2009-2007 only
o_archive_course <- function(course, rank_year) {
  o_archive_2009 <- function(df) {
    # set column names
    if(ncol(df) > 14) {
      new_names <- df[10, -(15:ncol(df))]
    } else {
      new_names <- df[10, ]
    }
    names(df)[1:ncol(new_names)] <- new_names
    # clean up columns
    df <- df %>%
      rename(Overall_Rank = `Overall Rank`, Count_Events = `# of Events`) %>%
      unite(Name, `First Name`, Surname, sep = " ") %>%
      mutate(Birth_Year = rank_year - as.numeric(Age)) %>%
      select(-starts_with("X"), -Patch, -Age, -starts_with("USOF"))
    if(!("Class" %in% names(df))) {
      df <- mutate(df, Class = "M-21+", Class_Rank = Overall_Rank)
    } else {
      df <- rename(df, Class_Rank = `Class Rank`)
    }
    return(df)
  }
  o_archive_2008 <- function(df) {
    # set column names
    if(ncol(df) > 14) {
      new_names <- df[10, -(15:ncol(df))]
    } else {
      new_names <- df[10, ]
    }
    names(df)[1:ncol(new_names)] <- new_names
    # clean up columns
    df <- df %>%
      rename(Overall_Rank = `Overall\n    Rank`, Count_Events = `# of\n    Events`) %>%
      mutate(Birth_Year = rank_year - as.numeric(Age)) %>%
      select(-starts_with("X"), -Award, -Age, -starts_with("USOF"),
             First = starts_with("First"), Last = starts_with("Last")) %>%
      unite(Name, First, Last, sep = " ")
    if(!("Class" %in% names(df))) {
      df <- mutate(df, Class = "M-21+", Class_Rank = Overall_Rank)
    } else {
      df <- rename(df, Class_Rank = `Class\n    Rank`)
    }
    return(df)
  }
  o_archive_2007 <- function(df) {
    # set column names
    df <- df[, -10] # fixes parsing of blue columns
    if(ncol(df) > 14) {
      new_names <- df[8, -(15:ncol(df))]
    } else {
      new_names <- df[8, ]
    }
    names(df)[1:ncol(new_names)] <- new_names
    # clean up columns
    df <- df %>%
      rename(Count_Events = `# of\n    Events`) %>%
      mutate(Birth_Year = rank_year - as.numeric(Age)) %>%
      select(-starts_with("X"), -Award, -Age, -starts_with("USOF"),
             Overall_Rank = starts_with("Overall")) %>%
      # will not extract single word club names, broken clubs:
      # Coureurs de Bois
      # OK Linné
      # OLC Kapreolo
      extract(Name, c("Name", "Club"), regex = "(.*)\\s+([^ ]+)$")
    if(!("Class" %in% names(df))) {
      df <- mutate(df, Class = "M-21+", Class_Rank = Overall_Rank)
    } else {
      df <- rename(df, Class_Rank = `Class Rank`)
    }
    return(df)
  }
  # get data with correct columns
  df <- o_archive_data(course, rank_year)
  rank <- switch(as.character(rank_year),
                 "2009" = o_archive_2009(df),
                 "2008" = o_archive_2008(df),
                 "2007" = o_archive_2007(df))
  # correct variable type and clean missing data
  rank <- rank %>%
    mutate(Overall_Rank = as.integer(Overall_Rank),
           Class_Rank = as.integer(Class_Rank),
           Score = as.numeric(Score),
           Count_Events = as.integer(Count_Events),
           Time = ifelse(Score != 0, str_c(Time, ":00"), "")) %>%
    filter(!is.na(Count_Events))
  # reorder the coumns
  if("Birth_Year" %in% names(rank)) {
    rank <- select(rank, Class_Rank, Overall_Rank, Name, Birth_Year,
                   Club, Score, Time, Count_Events, Class)
  } else {
    rank <- select(rank, Class_Rank, Overall_Rank, Name,
                   Club, Score, Time, Count_Events, Class)
  }
  return(rank)
}