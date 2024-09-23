load_judges <- function() {
  url <- "https://www.fjc.gov/sites/default/files/history/judges.csv"
  temp <- tempfile()
  download.file(url, temp)
  data <- read_csv(temp, col_types = "iicccciiicciiiccccccccccccccDDDDcDccDDiiiiDcDccccccccccDDDDcDccDDiiiiDcDccccccccccDDDDcDccDDiiiiDcDccccccccccDDDDcDccDDiiiiDcDccccccccccDDDDcDccDDiiiiDcDccccccccccDDDDcDccDDiiiiDcDccccccicciccicciccicc")
  return(data)
}