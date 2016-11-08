library(tidyverse)
library(rvest)
library(lubridate)

# http://r4ds.had.co.nz/

# runner: https://www.orienteeringusa.org/rankings/runner_show.php?db_id=5415
# courses: https://www.orienteeringusa.org/rankings/crs_sum.php
# ranks: https://www.orienteeringusa.org/rankings/index.php

# returns all available dates for which ranks have been published
# info obtained from: https://www.orienteeringusa.org/rankings/index.php
o_rank_dates <- function() {
  dates <- read_html("https://www.orienteeringusa.org/rankings/index.php") %>%
    html_nodes("div#rank a.submenu") %>%
    html_text() %>%
    .[-length(.)] %>%
    mdy() %>%
    as.character()
}

# takes in a date and course and returns the combined rankings table 
# for all classes on that course
# info obtained from: https://www.orienteeringusa.org/rankings/index.php
o_rank_course <- function(rank_date = "current", course) {
  # error checking and conversion for course selections
  courses <- list(
    # course | crs_num
    "blue"   = 70,
    "red"    = 60,
    "green"  = 50,
    "brown"  = 40,
    "orange" = 30,
    "yellow" = 20,
    "white"  = 10
  )
  if(course %in% names(courses)) {
    crs_num <- courses[[course]]
  } else if (course %in% courses) {
    crs_num <- course
  } else {
    stop(paste0("Course '", course, "' is not a valid course. Try 'blue' or '70'."))
  }
  # rank_date must be a valid date in format YYYY-MM-DD or "current" for the most recent ranks
  if(rank_date == "current") {
    rank_date <- read_html("https://www.orienteeringusa.org/rankings/index.php") %>%
      html_node("div#rank a.submenu") %>%
      html_text() %>%
      mdy() %>%
      as.character()
  } else if (!(rank_date %in% o_rank_dates())) {
    stop(paste0("Date '", rank_date, "' is not available or not a valid date. Try 'current' ", 
                "or a different date in 'YYYY-MM-DD' format."))
  }
  # get page of rank data
  url <- paste0("https://www.orienteeringusa.org/rankings/rank_show.php?rank_date=",
                rank_date,"&crs_num=",crs_num,"&show=cls")
  page <- read_html(url)
  # read in the table for each class
  dfs <- page %>%
    html_nodes("div#content table") %>%
    html_table()
  # make column names more readable
  dfs <- dfs %>%
    lapply(function(df) rename(df, Class_Rank = `Class Rank`, 
                               Overall_Rank = `Overall Rank`, 
                               Count_Events = `# Events`,
                               Birth_Year = YB))
  # convert rank columns to type integers and complete birth year
  dfs <- suppressWarnings(
    lapply(dfs, function(df) mutate(df, Class_Rank = as.integer(Class_Rank), 
                                    Overall_Rank = as.integer(Overall_Rank),
                                    Birth_Year = if_else(Birth_Year <= year(Sys.Date()) - 2000, 
                                                         Birth_Year + 2000, 
                                                         Birth_Year + 1900))))
  # read in class values to apply to each rank table
  classes <- page %>%
    html_nodes("div#content h4") %>%
    html_text()
  if(substring(classes[1], 1, 8) == "Rankings") {
    classes <- classes[-1]
  }
  # set the class column for each table and combine
  add_class_field <- function(df, class) mutate(df, Class = class)
  ranks <- mapply(add_class_field, dfs, classes, SIMPLIFY = FALSE) %>%
    do.call(rbind, .) %>%
    as_tibble()
  return(ranks)
}

# takes in a runner's last and first name and returns the event results history
# use current to specify if you want to limit results to current ranking only (past year)
# info obtained from: https://www.orienteeringusa.org/rankings/find.php
o_runner_results <- function(last, first, current = FALSE) {
  # Bug: will return no results for most recent date if the most recent date is not the current rankings
  
  # navigate to the runner selection page
  first_to_upper <- function(t) {
    f <- substr(t, 1, 1)
    rest <- substring(t, 2)
    return(paste0(toupper(f), tolower(rest)))
  }
  first <- first_to_upper(first)
  last <- first_to_upper(last)
  name <- paste(first, last, sep = " ")
  url <- paste0("https://www.orienteeringusa.org/rankings/find.php?order=last&init=",
                substr(last, 1, 1))
  # select runner based on input
  s <- html_session(url)
  
  # tryCatch()
  page <- tryCatch({
    follow_link(s, name)
  }, error = function(e) {
    e$message <- paste0("Runner '", name, "' could not be found on page: ", url)
    stop(e)
  })
  dfs <- page %>%
    html_nodes("div#content table") %>%
    html_table()
  if(current) {
    # return only the event results included in the current rank
    results <- dfs[[1]]
  } else {
    # identify all dates with results
    dates <- dfs[[2]] %>%
      select(Date) %>%
      mutate(Value = Date, Date = mdy(Date)) %>%
      arrange(desc(Date))
    # select one date out of every year with results
    values <- dates %>%
      group_by(Year = year(Date)) %>%
      summarise(Value = first(Value)) %>%
      arrange(-Year) %>%
      select(Value) %>%
      t() %>%
      as.list()
    # read the course results for every year and combine
    get_date_result <- function(d) {
      page %>%
        follow_link(d) %>%
        html_node("div#content table") %>%
        html_table()
    }
    results <- lapply(values[-1], get_date_result) %>% 
      c(list(dfs[[1]]), .) %>%
      do.call(rbind, .) %>%
      unique()
  }
  results <- results %>%
    select(-Diff) %>%
    as_tibble()
  return(results)
}


