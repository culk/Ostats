library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)

# The below functions scrape ranking data from the OrienteeringUSA ranking
# archives. The process of scraping the data and returning a formated dataframe
# is done in three steps because of the large variation in formats for each 
# year. 
# Step 1: Scrape data for a course/year and combine into a single dataframe.
#         This is done by the `o_archive_data` function.
# Step 2: Remove unneeded rows and columns, add missing information, and
#         put all the information into standardized columns.
#         This is done by the `o_archive_YYYY` functions.
# Step 3: Convert each column to the correct format and reorder the columns.
#         This is done by the second half of the `o_archive_course` function.
# source: https://www.orienteeringusa.org/rankings/archive.php
# input format: course: 70 or "blue", rank_year: YYYY
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
  if (str_to_lower(course) %in% courses) {
    course_name <- course
  } else if (as.character(course) %in% names(courses)) {
    course_name <- courses[as.character(course)] %>% as.character()
  } else {
    stop(str_c("Course '", course, 
               "' is not a valid course. Try 'blue' or '70'."))
  }
  # 1997 and earlier are not supported yet
  filename <- switch(as.character(rank_year),
                     "2009" = str_c("2009_12_31_",course_name),
                     "2008" = str_c("2008_eoy_",course_name),
                     "2007" = str_c("2007_",course_name,"_official"),
                     "2006" = str_c("2006_12_31_",course_name),
                     "2005" = str_c("2005_",course_name,"_official"),
                     "2004" = str_c("2004_",course_name,"_official"),
                     "2003" = str_c("2003_",course_name,"_official"),
                     "2002" = str_c("2002_",course_name,"_unofficial"),
                     "2001" = str_c("2001_",course_name,"_unofficial"),
                     "2000" = str_c("2000_",course_name,"_unofficial"),
                     "1999" = str_c("1999_",course_name,"_unofficial"),
                     "1998" = str_c("1998_",course_name,"_unofficial"),
                     stop(str_c("Invalid year '", rank_year, "' selected. ",
                                "This function only supports archived data ",
                                "from 2009-1998.")))
  url <- str_c("https://www.orienteeringusa.org/rankings/rslt/",
               filename, ".html")
  # scrape data on the page depending on the format used at the time
  if (rank_year >= 2005) {
    # formatted as single html table with all classes
    page <- read_html(url)
    df <- page %>%
      html_table(fill = TRUE) %>%
      first() %>%
      as_tibble()
  } else if (rank_year >= 2003) {
    # formatted as an html table for each class
    page <- read_html(url)
    dfs <- page %>%
      html_table(fill = TRUE, header = TRUE)
    # fix for edge case with one additional runner on M/F-White
    if (rank_year == 2004 && course_name == "white") {
      dfs[[length(dfs)]] <- dfs[[length(dfs)]][, c(-2:-4)]
      names(dfs[[length(dfs)]]) <- names(dfs[[1]][, -ncol(dfs[[1]])])
    }
    # the open categories do not have an award column
    dfs <- lapply(dfs, function(df) mutate(df, Award = NA))
    df <- dfs %>%
      do.call(rbind, .) %>%
      as_tibble()
  } else if (rank_year >= 1998) {
    # formatted as fixed width, encoded using "windows-1252"
    page <- read_html(url, encoding = "windows-1252")
    temp_text <- page %>% 
      html_node("pre") %>% 
      html_text()
    if (rank_year == 1998) {
      name_width <- 20
    } else {
      name_width <- 24
    }
    # fix for edge case where age was included
    if (rank_year == 1998 && course_name == "blue") {
      df <- read.fwf(textConnection(temp_text), 
                     widths = c(7, name_width, 9, 5, 7, 9, 9),
                     strip.white = TRUE, as.is = TRUE, fill = TRUE,
                     col.names = c("Class_Rank", "Name", "Club", "Age", 
                                   "Count_Events", "Score", "Time")) %>%
        select(-Age) %>%
        as_tibble() %>%
        mutate(Class = NA) %>%
        filter(!is.na(Class_Rank))
    } else {
    df <- read.fwf(textConnection(temp_text), 
                   widths = c(7, name_width, 7, 7, 9, 10),
                   strip.white = TRUE, as.is = TRUE, fill = TRUE,
                   col.names = c("Class_Rank", "Name", "Club", "Count_Events",
                                 "Score", "Time")) %>%
      as_tibble() %>%
      mutate(Class = NA) %>%
      filter(!is.na(Class_Rank))
    }
    # loop over rows adding class information
    cur_class <- NA
    for (i in 1:nrow(df)) {
      if (is.na(df[i, 1])) {
        df[i, 7] <- cur_class
      } else if (df[i, 1] == "Rank") {
        df[i, 7] <- "Class"
      } else if (str_sub(df[i, 1], 1, 1) == "M" ||
                 str_sub(df[i, 1], 1, 1) == "F") {
        cur_class <- df[i, 1]
      } else {
        df[i, 7] <- cur_class
      }
    }
  }
  return(df)
}

o_archive_course <- function(course, rank_year) {
  # a separate function is needed to clean the data from each year
  # due to the differences in formating
  o_archive_2009 <- function(df) {
    # set column names
    new_names <- df[df[, 3] == "Class Rank" | df[, 3] == "Surname", ][1, ]
    names(df)[1:ncol(new_names)] <- new_names
    df <- df[, str_length(names(df)) > 1]
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
      rename(Count_Events = `# of\n    Events`) %>%
      mutate(Birth_Year = rank_year - as.numeric(Age)) %>%
      select(-starts_with("X"), -Award, -Age, -starts_with("USOF"),
             First = starts_with("First"), Last = starts_with("Last"),
             Overall_Rank = starts_with("Overall")) %>%
      unite(Name, First, Last, sep = " ")
    if(!("Class" %in% names(df))) {
      df <- mutate(df, Class = "M-21+", Class_Rank = Overall_Rank)
    } else {
      df <- df %>%
        select(Class_Rank = matches("Class(\\\\n)?\\s+Rank"), everything())
    }
    return(df)
  }
  o_archive_2007 <- function(df) {
    # set column names
    df <- df[, 1:9] # fixes parsing of blue columns
    new_names <- df[df[, 1] == "USOF\n    Rank", ][1, ]
    names(df)[1:ncol(new_names)] <- new_names
    # clean up columns
    df <- df %>%
      mutate(Birth_Year = rank_year - as.numeric(Age)) %>%
      select(-starts_with("X"), -Age, -starts_with("USOF"),
             Overall_Rank = starts_with("Overall"),
             Count_Events = ends_with("Events"))
    # some clubs have more than one word for their name
    # the below code checks the club name and adds a separator '|'
    # this is not vectorized so it needs to be applied to the Name field
    sep_name_club <- function(x) {
      clubs <- c("OK Linné", "Coureurs de Bois", "OK Linne", "Östersunds OK",
                 "Bussola OK", "OLC Kapreolo", "Heming Norway", "CORK OC")
      x.split <- unlist(str_split(x, " "))
      x.last3 <- str_c(tail(x.split, 3), collapse = " ")
      x.last2 <- str_c(tail(x.split, 2), collapse = " ")
      if (x.last3 %in% clubs) {
        club <- x.last3
        name <- str_sub(x, 1, -(str_length(club) + 2))
      } else if (x.last2 %in% clubs) {
        club <- x.last2
        name <- str_sub(x, 1, -(str_length(club) + 2))
      } else {
        club <- str_c(tail(x.split, 1), collapse = " ")
        name <- str_sub(x, 1, -(str_length(club) + 2))
      }
      return(str_c(name, club, sep = "|"))
    }
    df$Name <- sapply(df$Name, sep_name_club)
    df <- separate(df, Name, c("Name", "Club"), "\\|")
    if(!("Class" %in% names(df))) {
      df <- df %>%
        mutate(Class = "M-21+", Class_Rank = Overall_Rank) %>%
        select(-Award)
    } else {
      df <- rename(df, Class_Rank = `Class Rank`)
    }
    return(df)
  }
  o_archive_2006 <- function(df) {
    # set column names
    new_names <- df[df[, 1] == "Class Rank", ][1, ]
    names(df)[1:ncol(new_names)] <- new_names
    df <- df[, str_length(names(df)) > 1]
    # clean up columns
    df <- df %>%
      rename(Count_Events = Events, Class_Rank = `Class Rank`,
             Score = Result) %>%
      select(-`USOF?`) %>%
      unite(Name, First, Last, sep = " ")
    if(!("Overall Rank" %in% names(df))) {
      df <- df[, 1:7] %>%
        mutate(Overall_Rank = Class_Rank)
    } else {
      df <- df %>%
        rename(Overall_Rank = `Overall Rank`)
    }
    return(df)
  }
  o_archive_2005 <- function(df) {
    # set column names
    if(str_sub(as.character(df[4, 8]), -6, -1) == "to 100") {
      df[4:31, 8:10] <- df[4:31, 9:11]
      df <- df[, 1:10]
    }
    new_names <- df[df[, 2] == "Overall Rank" | df[, 2] == "First", ][1, ]
    names(df)[1:ncol(new_names)] <- new_names
    df <- df[, str_length(names(df)) > 1]
    # clean up columns
    df <- df %>%
      rename(Score = Result, Count_Events = Events) %>%
      mutate(Count_Events = ifelse(str_detect(Count_Events, "\\*"), 
                                   str_sub(Count_Events, 1, -2), 
                                   Count_Events)) %>%
      select(-Award, Class_Rank = contains("Class Rank")) %>%
      unite(Name, First, Last, sep = " ")
    if(!("Overall Rank" %in% names(df))) {
      df <- df[, 1:7] %>%
        mutate(Overall_Rank = Class_Rank)
    } else {
      df <- df %>%
        rename(Overall_Rank = `Overall Rank`)
    }
    return(df)
  }
  o_archive_2004 <- function(df) {
    # set column names
    names(df)[1:3] <- c("Class_Rank", "First", "Last")
    # clean up columns
    df <- df %>%
      rename(Score = Result, Count_Events = Events) %>%
      select(-Award) %>%
      unite(Name, First, Last, sep = " ") %>%
      arrange(-Score) %>%
      mutate(Overall_Rank = row_number())
    return(df)
  }
  o_archive_2003 <- function(df) {
    # set column names
    names(df)[1:4] <- c("Class_Rank", "Overall_Rank", "First", "Last")
    # clean up columns
    df <- df %>%
      rename(Score = Result, Count_Events = Events) %>%
      select(-Award) %>%
      unite(Name, First, Last, sep = " ") %>%
      arrange(-Score) %>%
      mutate(Overall_Rank = row_number())
    return(df)
  }
  o_archive_02_98 <- function(df) {
    # clean up columns
    df <- df %>%
      mutate(Score = as.numeric(Score)) %>%
      arrange(desc(Score)) %>%
      mutate(Overall_Rank = row_number())
    return(df)
  }
  # get data with correct information, clean and format data depending on year
  df <- o_archive_data(course, rank_year)
  rank <- switch(as.character(rank_year),
                 "2009" = o_archive_2009(df),
                 "2008" = o_archive_2008(df),
                 "2007" = o_archive_2007(df),
                 "2006" = o_archive_2006(df),
                 "2005" = o_archive_2005(df),
                 "2004" = o_archive_2004(df),
                 "2003" = o_archive_2003(df),
                 "2002" = o_archive_02_98(df),
                 "2001" = o_archive_02_98(df),
                 "2000" = o_archive_02_98(df),
                 "1999" = o_archive_02_98(df),
                 "1998" = o_archive_02_98(df),
                 stop(str_c("Invalid year '", rank_year, "' selected. This ",
                            "function only formats data from 2009-1998.")))
  # correct variable type and remove missing data
  rank <- rank %>%
    mutate(Overall_Rank = as.integer(Overall_Rank),
           Class_Rank = as.integer(Class_Rank),
           Score = as.numeric(Score),
           Count_Events = as.integer(Count_Events)) %>%
    filter(!is.na(Count_Events))
  # function to convert the time string from mmm:ss to hh:mm:ss for
  # consistency with current format
  fix_time <- function(t) {
    if (str_length(t) > 7 || str_length(t) < 2) {
      return("")
    }
    ms <- as.integer(str_split(t, ":", simplify = TRUE))
    m <- ms[1] %% 60
    h <- as.integer(ms[1] / 60)
    if (m < 10) {
      m <- str_c("0", m)
    }
    if (ms[2] < 10) {
      s <- str_c("0", ms[2])
    } else {
      s <- ms[2]
    }
    if (h < 10) {
      h <- str_c("0", h)
    }
    str_c(h, m, s, sep = ":")
  }
  rank$Time <- sapply(rank$Time, fix_time)
  # reorder the columns
  if (!("Birth_Year" %in% names(rank))) {
    rank <- mutate(rank, Birth_Year = NA)
  }
  rank <- select(rank, Class_Rank, Overall_Rank, Name, Birth_Year,
                 Club, Score, Time, Count_Events, Class)
  return(rank)
}
