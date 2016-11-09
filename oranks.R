library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

# FUTURE: add support for rank data past 2010
#   links: https://www.orienteeringusa.org/rankings/archive.php
#   ranks has: Rank, Name, Club, # Events, Pts, Time, Rank, Course, Class
#   started breaking it out by course in 2003
#   started doing intermediate rankings in 2006

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
  # Note: scores from individual events are recalculated by OUSA whenever new rankings are done until
  # the event has rolled off of the current rankings (after 12 months). This can cause the score
  # from an event to change +/- 10 points over a 12 month period. Pulling the last reported score 
  # (at 12 months) seems to be the most elegant way to work around this with the side affect of
  # having to pull data from every ranking page which can be slow.
  
  # navigate to the runner selection page
  name <- paste(first, last, sep = " ") %>%
    str_to_title()
  url <- paste0("https://www.orienteeringusa.org/rankings/find.php?order=last&init=",
                substr(last, 1, 1))
  # select runner based on input
  s <- html_session(url)
  page <- tryCatch({
    follow_link(s, name)
  }, error = function(e) {
    e$message <- paste0("Runner '", name, "' could not be found on page: ", url)
    stop(e)
  })
  # dfs[[1]] contains latest ranking event results
  # dfs[[2]] contains a table of dates for previous rankings
  dfs <- page %>%
    html_nodes("div#content table") %>%
    html_table()
  if(current) {
    # return only the event results included in the current rank
    results <- dfs[[1]]
  } else {
    # identify all dates with results
    dates <- dfs[[2]] %>%
      mutate(Date = mdy(Date)) %>%
      select(Date) %>%
      arrange(desc(Date)) %>%
      t() %>%
      as.list()
    # read the course results for all past rankings and add only the newest scores
    get_date_result <- function(d) {
      paste0(page$url, "&rank_date=", d) %>%
        read_html() %>%
        html_node("div#content table") %>%
        html_table()
    }
    results <- lapply(dates, get_date_result) %>%
      do.call(rbind, .)
    old_scores <- results %>%
      select(Event, Date, Course, InRank) %>%
      duplicated()
    results <- results[!old_scores, ]
  }
  results <- results %>%
    select(-Diff) %>%
    filter(InRank == "Y") %>%
    as_tibble()
  return(results)
}

o_events <- function() {
  # https://www.orienteeringusa.org/rankings/results.php
  # ID, Name, Date, Club
  # goes back to Georgia Navigator Cup Day 1, Jan 16, 2010 (GAOC)
  url <- "https://www.orienteeringusa.org/rankings/results.php"
  page <- read_html(url)
  lines <- page %>%
    html_nodes("div#content ul li")
  ids <- lines %>% 
    html_node("a") %>% 
    html_attr("href") %>%
    str_split("ev_id=") %>%
    sapply(function(x) x[2]) %>%
    tibble(ID = .)
  events <- lines %>%
    html_text() %>%
    as_tibble() %>%
    separate(value, into = c("Name", "md", "other"), sep = ", ") %>%
    separate(other, into = c("y", "Club"), sep = "  ") %>%
    unite(Date, md, y, sep = ", ") %>%
    mutate(Club = str_sub(Club, 2, -2)) %>%
    bind_cols(ids, .)
  return(events)
}

o_courses <- function() {
  # https://www.orienteeringusa.org/rankings/crs_sum.php
  # ID, Name, Date, Club, Course, Length, Climb, Controls, CGV, [count runners, top3 avg time, top3 avg pace, count [finish/mp/dnf]]
  # not sorted by time looks like it goes back to 2010 as well
}

o_clubs <- function() {
  # http://www.us.orienteering.org/clubs/all
  # Name, Code, Website, City, State, Zip
}
