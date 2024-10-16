load_scdb <- function(centered = "justice", organized = "Citation") {
  library(tidyverse)
  library(rvest)
  url <- "http://scdb.wustl.edu/_brickFiles/"
  html <- read_html(url)
  table <- html %>% html_node("table") %>% html_table
  latest <- table %>%
    select(Name, `Last modified`) %>%
    filter(`Last modified` != "") %>%
    mutate(mod = ymd_hm(`Last modified`)) %>%
    select(-`Last modified`) %>%
    slice_max(order_by = mod, n = 1)
  url2 <- paste0(url,
                latest$Name[1],
                "SCDB_",
                str_replace(latest$Name[1], "/", ""),
                "_",
                centered,
                "Centered_",
                organized,
                ".csv.zip"
                )
  temp <- tempfile()
  download.file(url2, temp)
  data <- read_csv(temp, col_types = "cccccfcccciffccccffffffffffffffffffffffffffffffcfffffffffffff")
  data <- data %>%
    mutate(dateDecision = mdy(dateDecision),
           dateArgument = mdy(dateArgument),
           dateRearg = mdy(dateRearg)
           )
  return(data)
}

load_scdb_legacy <- function(centered = "justice") {
  url <- "http://scdb.wustl.edu/_brickFiles/"
  html <- read_html(url)
  table <- html %>% html_node("table") %>% html_table
  latest <- table %>%
    select(Name, `Last modified`) %>%
    filter(`Last modified` != "") %>%
    filter(str_detect(Name, "Legacy")) %>%
    mutate(mod = ymd_hm(`Last modified`)) %>%
    select(-`Last modified`) %>%
    slice_max(order_by = Name, n = 1)
  url2 <- paste0(url, 
                 latest$Name[1],
                 "SCDB_",
                 str_replace(latest$Name[1], "/", ""),
                 "_",
                 centered,
                 "Centered_",
                 "Citation",
                 ".csv.zip"
                 )
  temp <- tempfile()
  download.file(url2, temp)
  data <- read_csv(temp, col_types = "cccccfcccciffccccffffffffffffffffffffffffffffffcfffffffffffff")
  data <- data %>%
    mutate(dateDecision = mdy(dateDecision),
          dateArgument = mdy(dateArgument),
          dateRearg = mdy(dateRearg)
          )
  return(data)
}

load_scdb_integrated <- function(centered = "case") {
  df1 <- load_scdb(centered, "Citation")
  df2 <- load_scdb_legacy(centered)
  data <- bind_rows(df2, df1)
  return(data)
}