load_fjc <- function(type = "civil", period = "new") {
  library(tidyverse)
  branches <- tibble::tribble(
    ~category, ~old, ~new,
    "civil", "Civil%201970%20to%201987.zip", "cv88on.zip",
    "criminal", "cr70to95.zip", "cr96on.zip",
    "appeals", "ap71to07.zip", "ap08on_0.zip", 
    "bankruptcy", "cpbank08to17.zip", "cpbank18on_0.zip"
  )
  coltypes <- tibble::tribble(
    ~category, ~old, ~new,
    "civil", "dcdccdddddlldlccccdlddddddcd", "dcdcdccddcccdcddllccdcccdcdccdlldddddcccccdccd",
    "appeals", "fccclcfcddddcddccdcccccdddddddddccccccdllldddcddcdddddddd", "fccclcfcddcddcdcccddcccclcdddddddccccccdlclclcdddccddc"
  )
  # REMINDER: change to data type of dates from strings to date times
  trunk <- "https://www.fjc.gov/sites/default/files/idb/textfiles/"
  branch <- branches |> filter(category == type) |> select(period)
  url <- paste0(trunk, branch)
  temp <- tempfile()
  download.file(url, temp)
  cols <- coltypes |> filter(category == type) |> select(period) |> as.character()
  fjc <- readr::read_tsv(temp, col_names = TRUE, col_types = cols)
  return(fjc)
}