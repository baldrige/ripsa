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

case_lineup <- function(citation) {
  jus <- load_justices()
  df <- load_scdb_integrated("justice") |> 
    filter(usCite == citation) |> 
    select(justice, justiceName, caseName, dateDecision, vote, opinion, majority)
  caption <- df$caseName[1]  
  year <- year(df$dateDecision[1])
  df <- df |> left_join(jus, by = c("justice", "justiceName"))
  df |> select(-justice, -justiceName, -dateDecision, -Start, -End, -caseName) |>
    select(image, Name, majority, vote, opinion) |> 
    arrange(majority, vote, desc(opinion)) |>
    mutate(majority = case_match(majority, "1" ~ "Dissent", "2" ~ "Majority")) |>
    mutate(vote = case_match(vote, 
      "1" ~ "Voted with Majority or Plurality", 
      "2" ~ "Dissent", 
      "3" ~ "Regular Concurrence",
      "4" ~ "Special Concurence",
      "5" ~ "Judgment of the Court",
      "6" ~ "Dissental",
      "7" ~ "Jurisdictional Dissent",
      "8" ~ "Equally Divided Vote"
    )) |>
    mutate(opinion = case_match(opinion,
      "1" ~ "No opinion",
      "2" ~ "Authored opinion",
      "3" ~ "Co-authored Opinion"
    )) |> 
    group_by(majority) |> 
    gt(row_group_as_column = TRUE) |> 
    tab_header(title = caption, subtitle = paste0(citation, " (", year, ")")) |>
    fmt_image(columns = "image") |>
    cols_label(image = "", majority = "Vote", vote = "Vote Type", opinion = "Opinion?") |>
    data_color(columns = c(vote)) |>
    tab_style(style = cell_text(weight = "bold", v_align = "middle"), locations = cells_row_groups())
}