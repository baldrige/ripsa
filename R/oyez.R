oyez_lookup <- function(term, docket) {
  url <- "https://api.oyez.org/cases/"
  url2 <- paste0(url, term, "/", docket)
  req <- request(url2)
  resp <- req_perform(req)
  case <- resp |> resp_body_json()
  return(case)
}

oyez_get_oa_audio <- function(term, docket) {
  case <- oyez_lookup(term, docket)
  href <- map(case$oral_argument_audio, "href")
  files <- map(href, \(x) pluck(resp_body_json(req_perform(request(x))), "media_file", 1, "href"))
  return(files)
}

oyez_lineup <- function(term, docket) {

}

oyez_database <- function() {
  url <- "https://raw.githubusercontent.com/walkerdb/supreme_court_transcripts/refs/heads/master/oyez/case_summaries.json"
  case_list <- fromJSON(url) |> unnest_wider(c(citation, timeline), names_sep = "_")
}