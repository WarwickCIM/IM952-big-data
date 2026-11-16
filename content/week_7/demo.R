# ------------------------------------------------------------------
# Fetch the chronology of an OpenStreetMap tag key from taginfo
# API docs: https://taginfo.openstreetmap.org/taginfo/apidoc#api_4_key_chronology
#
# Endpoint: GET /api/4/key/chronology (json only)
# Parameters: key (required) — no paging, filter or sort.
# Response:
#   total      INT     — total number of results
#   url        STRING  — URL of the request
#   data_until STRING  — all source changes until this date are reflected
#   data       ARRAY OF HASHES:
#     date      TEXT  — YYYY-MM-DD
#     nodes     INT   — DIFFERENCE of node count with this key vs. previous entry
#     ways      INT   — DIFFERENCE of way count with this key vs. previous entry
#     relations INT   — DIFFERENCE of relation count with this key vs. previous entry
#
# Requires: jsonlite  ->  install.packages("jsonlite")
# ------------------------------------------------------------------

library(jsonlite)

fetch_key_chronology <- function(key,
                                 base_url = "https://taginfo.openstreetmap.org/api/4/key/chronology") {

  # Build the request URL (single required parameter: key)
  url <- paste0(base_url, "?key=", utils::URLencode(key, reserved = TRUE))

  # GET the JSON (taginfo is read-only, GET requests only)
  response <- tryCatch(
    fromJSON(url, simplifyDataFrame = TRUE),
    error = function(e) {
      stop("Request failed for key '", key, "': ", conditionMessage(e),
           "\nCheck that the key exists and your connection is working.", call. = FALSE)
    }
  )

  # The chronology rows live in the 'data' member
  if (is.null(response$data)) {
    stop("Unexpected response: no 'data' element found.", call. = FALSE)
  }

  df <- as.data.frame(response$data)

  # Parse the date column (YYYY-MM-DD) and sort chronologically
  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
    df <- df[order(df$date), ]
  }
  rownames(df) <- NULL

  # Attach the metadata the API returns alongside the data
  attr(df, "key")        <- key
  attr(df, "total")      <- response$total
  attr(df, "url")        <- response$url
  attr(df, "data_until") <- response$data_until

  df
}

# ------------------------------------------------------------------
# Example usage
# ------------------------------------------------------------------

# Chronology of the 'highway' key (as in the API docs example)
highway_chronology <- fetch_key_chronology("highway")

str(highway_chronology)
head(highway_chronology)

# Attributes (metadata returned by the API):
attributes(highway_chronology)

# NOTE on interpretation: nodes/ways/relations hold *differences* vs. the
# previous chronology entry. Cumulative usage over time:
#   cumsum(highway_chronology$ways)

# Plot cumulative way usage with base R:
plot(highway_chronology$date,
     cumsum(highway_chronology$ways),
     type = "l",
     main  = "Cumulative 'highway' key usage on ways",
     xlab  = "Date", ylab = "Ways (cumulative)")

# Works for any key, e.g.:
# amenity_chronology <- fetch_key_chronology("amenity")
# building_chronology <- fetch_key_chronology("building")


# Claude -----------------------------------------------------------------

# install.packages(c("httr2", "jsonlite"))
library(httr2)
library(jsonlite)

get_key_overview <- function(key) {
  resp <- request("https://taginfo.openstreetmap.org/api/4/key/overview") |>
    req_url_query(key = key) |>
    req_user_agent("R taginfo client (your_email@example.com)") |>
    req_retry(max_tries = 3) |>
    req_perform()

  resp_body_json(resp, simplifyVector = FALSE)
}

res <- get_key_overview("highway")

# Top-level metadata
res$url
res$data_until

df <- as.data.frame(res$data)

# Inspect the structure of the returned data
str(res$data, max.level = 2)
key <- "highway"
url <- paste0("https://taginfo.openstreetmap.org/api/4/key/overview?key=", URLencode(key, reserved = TRUE))
res <- fromJSON(url)
str(res$data)

df <- as.data.frame(res$data)


# other ------------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)

get_key_overview <- function(key) {
  resp <- request("https://taginfo.openstreetmap.org/api/4/key/overview") |>
    req_url_query(key = key) |>
    req_user_agent("R taginfo client (your_email@example.com)") |>
    req_retry(max_tries = 3) |>
    req_perform()

  # simplifyVector = TRUE turns arrays of records into data frames automatically
  resp_body_string(resp) |> 
    fromJSON(simplifyVector = TRUE)
}

selected_key <- "highway"

res <- get_key_overview(selected_key)

# 1. See what you actually got
names(res$data)
str(res$data, max.level = 1)

# 2. Split into sub-tables (data frames) and everything else
is_tbl <- vapply(res$data, is.data.frame, logical(1))

tables  <- res$data[is_tbl]    # named list of data frames
scalars <- res$data[!is_tbl]   # scalars / nested non-tabular items

# 3. Access individual tables by name, e.g.
names(tables)
head(tables[[1]])


# Te response contains multiple dataframes
str(res$data, max.level = 1)

# Convert each dataframe:
key_counts <- res$data$counts 
 

