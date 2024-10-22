library(httr2)

congress_helper <- function(key = "UAmUTgvrj24sW5P4kB8Zt2CxwIjNNRJubL4Vgsxe", ..., format = "json", resource = "member", limit = 250, offset = 0) {
  params <- list(
    ...,
    limit = limit,
    offset = offset,
    format = format,
    resource = resource,
    api_key = key
  )
  request("https://api.congress.gov/v3/") |>
    req_url_path_append(resource) |>
    req_url_query(!!!params) |> 
    req_user_agent("ripsa") |> 
    req_perform() |>
    resp_body_json()
}

congress_members <- function() {
  members <- congress_helper(format = "json", resource = "member", limit = 250)
  max <- ceiling(members$pagination$count/250)
  df <- tibble::tibble()
  for(i in 1:max) {
    json <- congress_helper(format = "json", resource = "member", limit = 250, offset = ((i-1)*250))
    df2 <- tibble::tibble(
      name = map_chr(json$members, "name"), 
      bioguideId = map_chr(json$members, "bioguideId"), 
      party = map_chr(json$members, "partyName"), 
      state = map_chr(json$members, "state"), 
      depiction = map(json$members, "depiction"),
      terms = map(json$members, terms)) |>
      unnest_wider(col = depiction) |> hoist(terms, "item")
    df <- df |> bind_rows(df2)
  }
  df <- df |> unnest_wider(col = starts_with("item"), names_sep = "_")
  return(df)
}

congress_bills <- function() {
  bills <- congress_helper(resource = "bill")
  max <- ceiling(bills$pagination$count/250)
  df <- tibble::tibble()
  for(i in 1:max) {
    json <- congress_helper(resource = "bill", offset = ((i-1)*250))
    df2 <- tibble::tibble(
      congress = map_chr(json$bills, "congress"),
      number = map_chr(json$bills, "number"),
      originChamber = map_chr(json$bills, "originChamber"),
      title = map_chr(json$bills, "title"),
      type = map_chr(json$bills, "type")
    )
    df <- df |> bind_rows(df2)
  }
  return(df)
}