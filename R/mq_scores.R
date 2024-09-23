mq_scores <- function() {
  url <- "http://mqscores.wustl.edu/media/2022/justices.csv"
  temp <- tempfile()
  download.file(url, temp)
  data <- read_csv(temp, col_types = "iicddddd")
  return(data)
}