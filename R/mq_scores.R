mq_scores <- function() {
  url <- "http://mqscores.wustl.edu/media/2022/justices.csv"
  temp <- tempfile()
  download.file(url, temp)
  data <- read_csv(temp, col_types = "iicddddd")
  return(data)
}

judjis <- function() {
  judjis <- read_csv("data/judjis_judge_year.csv", 
                    col_names = c("id","first_last_court","first.name","last.name","first_last_court_eval.year","eval.year","court","pap","gender","asian","black","hispanic","white","judjis_score"),
                    col_types = "iccccicfllllld",
                    show_col_types = FALSE,
                    skip = 1) |> 
    select(-first_last_court, -first_last_court_eval.year)
}

jcs <- function() {
  url <- "https://epstein.wustl.edu/s/JCS2024.zip"
  temp <- tempfile()
  download.file(url, temp)
  data <- read_csv(unzip(temp, files = "jcs 2024/csv/coa_judges_2024.csv"), col_types = "icfd") |> rename(id = `...1`)
  data <- data |> separate_wider_regex(name, c(last = ".*", ", ", first = ".*", " ", middle = ".*", ", ", suffix = ".*", " "), too_few = "align_start")
  return(data)
}