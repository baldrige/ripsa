oyez_helper <- function(filter = "", docket_number = "", per_page = 450, ...) {
  params <- list(
    ...,
    filter = filter,
    per_page = per_page
  )
  request("https://api.oyez.org/cases") |>
    req_url_query(!!!params) |>
    req_perform() |>
    resp_body_json()
}

oyez_case_query <- function(term = 2023, docket_number = "") {
  request("https://api.oyez.org/cases") |>
    req_url_path_append(term) |>
    req_url_path_append(docket_number) |>
    req_perform() |>
    resp_body_json()
}

oyez_argument_query <- function(id) {
  request("https://api.oyez.org/case_media/oral_argument_audio") |>
    req_url_path_append(id) |>
    req_perform() |>
    resp_body_json()
}

tibble(
  name = map_chr(diaz, "name"), 
  term = map_chr(diaz, "term"), 
  docket = map_chr(diaz, "docket_number"), 
  citation = map(diaz, "citation"), 
  facts = map_chr(diaz, "facts_of_the_case"), 
  question = map_chr(diaz, "question"), 
  advocates = map(diaz, "advocates"), 
  decisions = map(diaz, "decisions")
)