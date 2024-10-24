get_case_data <- function(case_id, api_key) {
  # Build the URL for the specific case
  url <- paste0("https://www.courtlistener.com/api/rest/v4/opinions/", case_id, "/")
  
  # Make the request with httr2
  response <- request(url) %>%
    req_headers(Authorization = paste("Token", api_key)) %>%
    req_perform()
  
  # Check for any errors in the response
  if (resp_status(response) != 200) {
    stop("Error: ", resp_status(response), " - ", resp_body_string(response))
  }
  
  # Parse and return the JSON response
  case_data <- resp_body_json(response, simplifyVector = TRUE)
  return(case_data)
}

get_cases_by_filter <- function(query_params = list(), api_key) {
  response <- request("https://www.courtlistener.com/api/rest/v4/opinions/") %>%
    req_headers(Authorization = paste("Token", api_key)) %>%
    req_url_query(!!!query_params) %>%
    req_perform()
  
  # Check for errors
  if (resp_status(response) != 200) {
    stop("Error: ", resp_status(response), " - ", resp_body_string(response))
  }
  
  # Parse and return the response
  cases_data <- resp_body_json(response, simplifyVector = TRUE)
  return(cases_data)
}
