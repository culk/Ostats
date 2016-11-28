library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

# returns a character vector of dates for which ranks have been published
# source: https://www.orienteeringusa.org/rankings/index.php
# output format: YYYY-MM-DD
o_rank_dates <- function() {
  dates <- read_html("https://www.orienteeringusa.org/rankings/index.php") %>%
    html_nodes("div#rank a.submenu") %>%
    html_text() %>%
    head(length(.)-1) %>%
    mdy() %>%
    as.character()
  return(dates)
}

# takes in a date and course and returns a data frame of the combined 
# rankings for all classes on that course
# source: https://www.orienteeringusa.org/rankings/index.php
# input format: course: 70 or "blue", rank_date: YYYY-MM-DD
o_rank_course <- function(course, rank_date = "current") {
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
  if (str_to_lower(course) %in% names(courses)) {
    crs_num <- courses[[course]]
  } else if (course %in% courses) {
    crs_num <- course
  } else {
    stop(str_c("Course '", course, 
               "' is not a valid course. Try 'blue' or '70'."))
  }
  # rank_date must be a valid date in format YYYY-MM-DD 
  # or "current" for the most recent ranks
  available_dates <- o_rank_dates()
  if (rank_date == "current") {
    rank_date <- available_dates %>%
      first()
  } else if (!(rank_date %in% available_dates)) {
    if (str_length(rank_date) == 4 && 
        any(str_detect(available_dates, as.character(rank_date)))) {
      rank_date <- available_dates %>%
        .[str_detect(., as.character(rank_date))] %>%
        first()
    } else if (rank_date <= 2009 && rank_date >= 1994) {
      # use the archive function for these dates
      return(o_archive_course(crs_num, rank_date))
    } else {
      stop(str_c("Date '", rank_date, 
                 "' is not available or not a valid date. Try 'current' ", 
                 "or a different date in 'YYYY-MM-DD' format."))
    }
  }
  # get page of rank data
  url <- str_c("https://www.orienteeringusa.org/rankings/rank_show.php",
               "?rank_date=", rank_date,"&crs_num=",crs_num,"&show=cls")
  page <- read_html(url)
  # read in the table for each class
  dfs <- page %>%
    html_nodes("div#content table") %>%
    html_table()
  # read in class values to apply to each rank table
  classes <- page %>%
    html_nodes("div#content h4") %>%
    html_text()
  if(str_sub(classes[1], 1, 8) == "Rankings") {
    classes <- classes[-1]
  }
  # set the class column for each table and combine
  add_class_field <- function(df, class) mutate(df, Class = class)
  ranks <- mapply(add_class_field, dfs, classes, SIMPLIFY = FALSE) %>%
    bind_rows() %>%
    as_tibble() %>%
    rename(Class_Rank = `Class Rank`,
           Overall_Rank = `Overall Rank`,
           Count_Events = `# Events`,
           Birth_Year = YB)
  # convert rank columns to type integers and complete birth year
  ranks <- suppressWarnings(
    mutate(ranks, Class_Rank = as.integer(Class_Rank),
           Overall_Rank = as.integer(Overall_Rank),
           Birth_Year = if_else(Birth_Year <= year(Sys.Date()) - 2000,
                                Birth_Year + 2000,
                                Birth_Year + 1900)))
  return(ranks)
}

# takes in a runner's last and first name and logical value for current
# rank only; returns a data frame of event results used in rank calculations
# source: https://www.orienteeringusa.org/rankings/find.php
# input: 'current' limits results to current ranking results only (past year)
o_runner_results <- function(last, first, current = FALSE) {
  # Note: Orienteering USA recalculates scores from individual events
  # as they are used to calculate the current rankings. This will cause
  # event scores to shift +/-5 over the 12 months after they are posted.
  # This function only returns events from end of year totals and the current
  # partial rank results to obtain the most accurate event scores.
  
  # navigate to the runner selection page
  name <- str_c(first, last, sep = " ") %>%
    str_to_title()
  url <- str_c("https://www.orienteeringusa.org/rankings/find.php",
               "?order=last&init=", str_sub(last, 1, 1))
  # select runner based on input
  s <- html_session(url)
  page <- tryCatch({
    follow_link(s, name)
  }, error = function(e) {
    e$message <- str_c("Runner '", name, "' could not be found on page: ", url)
    stop(e)
  })
  # dfs[[1]] contains latest ranking event results
  # dfs[[2]] contains a table of dates for previous rankings
  dfs <- page %>%
    html_nodes("div#content table") %>%
    html_table()
  if (current) {
    # return only the event results included in the current rank
    results <- dfs[[1]]
  } else {
    # select current rank date and the end of year rank date 
    # for all years with results
    dates <- dfs[[2]] %>%
      mutate(Date = mdy(Date)) %>%
      group_by(Year = year(Date)) %>%
      summarise(Date = max(Date)) %>%
      select(Date) %>%
      arrange(desc(Date)) %>%
      t() %>%
      as.list()
    # read the course results for previous end of year rankings
    get_date_result <- function(d) {
      str_c(page$url, "&rank_date=", d) %>%
        read_html() %>%
        html_node("div#content table") %>%
        html_table()
    }
    results <- lapply(dates, get_date_result) %>%
      bind_rows()
    # remove duplicated events
    overlap <- results %>%
      select(Event, Date, Course, Time) %>%
      duplicated()
    results <- results[!overlap, ]
  }
  results <- results %>%
    select(-Diff) %>%
    filter(InRank == "Y") %>%
    mutate(Date = mdy(Date)) %>%
    as_tibble()
  return(results)
}

# returns data frame of all orienteering events since 2010
# source:https://www.orienteeringusa.org/rankings/results.php
# fields: ID, Name, Date, Club
o_events <- function() {
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
    separate(value, into = c("Name", "MonthDay", "other"), sep = ", ") %>%
    separate(other, into = c("Year", "Club"), sep = "  ") %>%
    unite(Date, MonthDay, Year, sep = ", ") %>%
    mutate(Date = mdy(Date), Club = str_sub(Club, 2, -2)) %>%
    bind_cols(ids, .)
  return(events)
}

# returns data frame of all orienteering courses since 2010
# source: https://www.orienteeringusa.org/rankings/crs_sum.php
# fields: Name, Date, Club, Course, Length, Climb, Ctrls, CGV
o_courses <- function() {
  url <- "https://www.orienteeringusa.org/rankings/crs_sum.php"
  page <- read_html(url)
  courses <- page %>%
    html_node("div#content table") %>%
    html_table() %>%
    as_tibble() %>%
    rename(Name = `Event Name`, Date = `Event Date`) %>%
    mutate(Date = mdy(Date)) %>%
    arrange(desc(Date))
  return(courses)
}

# returns data frame of all orienteering clubs
# source: http://www.us.orienteering.org/clubs/all
# fields: Name, Code, Website, City, State, Zip
o_clubs <- function() {
  url <- "http://www.us.orienteering.org/clubs/all"
  page <- read_html(url)
  content <- page %>%
    html_nodes("div.views-field-nothing span.field-content") %>%
    html_node("div")
  # extract club name (i.e. "Bay Area Orienteering Club (BAOC)")
  club.name <- content %>%
    html_node("b a") %>%
    html_text()
  # extract club website (i.e. "http://qoc.us.orienteering.org/")
  club.site <- content %>%
    html_node(":nth-child(3)") %>%
    html_text()
  # regular expression to extact club location: (city), (state) (zip)
  # (i.e. "Hopewell Junction", "New York", "12533")
  club.location <- content %>%
    html_text() %>%
    str_match("((?:\\w+ )*(?:\\w+)), ((?:\\w+ )+)(\\d{5}(?:-\\d{4})?)") %>%
    as_tibble() %>%
    select(City = V2, State = V3, Zipcode = V4)
  # combine all columns; separate out club code from name
  clubs <- tibble(Name = club.name, Website = club.site) %>%
    bind_cols(club.location) %>%
    unique() %>%
    separate(Name, c("Name", "Code"), sep = "  ") %>%
    mutate(Code = str_sub(Code, 2, -2))
  return(clubs)
}
