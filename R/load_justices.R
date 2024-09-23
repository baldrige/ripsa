load_justices <- function() {
  justicebio <- read_csv("justicebio.csv", col_types = "iccDDc")
  return(justicebio)
}

load_presidents <- function() {
  presidents <- read_csv("presidents.csv", col_types = "si")
  return(presidents)
}