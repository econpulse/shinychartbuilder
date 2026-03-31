# DB Query Layer fuer echte Tickerdaten mit einfachem In-Memory-Cache

new_data_cache <- function() {
  e <- new.env(parent = emptyenv())
  list(
    get = function(key) if (exists(key, envir = e, inherits = FALSE)) get(key, envir = e) else NULL,
    set = function(key, value) assign(key, value, envir = e),
    clear = function() rm(list = ls(envir = e), envir = e)
  )
}

build_cache_key <- function(ticker, date_from, date_to) {
  paste(ticker, as.character(date_from), as.character(date_to), sep = "|")
}

list_available_tickers <- function(con) {
  # Erwartet Tabelle ticker_meta(ticker TEXT PRIMARY KEY, frequency TEXT, description TEXT)
  DBI::dbGetQuery(con, "SELECT ticker, frequency, description FROM ticker_meta ORDER BY ticker")
}

fetch_ticker_data <- function(con, ticker, date_from = NULL, date_to = NULL, cache = NULL) {
  date_from <- date_from %||% as.Date("1900-01-01")
  date_to <- date_to %||% Sys.Date()

  key <- build_cache_key(ticker, date_from, date_to)
  if (!is.null(cache)) {
    cached <- cache$get(key)
    if (!is.null(cached)) return(cached)
  }

  sql <- "
    SELECT date, value
    FROM ticker_data
    WHERE ticker = ? AND date BETWEEN ? AND ?
    ORDER BY date
  "

  df <- DBI::dbGetQuery(con, sql, params = list(ticker, as.character(date_from), as.character(date_to)))
  if (nrow(df) > 0) df$date <- as.Date(df$date)

  if (!is.null(cache)) cache$set(key, df)
  df
}

load_raw_series_data <- function(con, raw_series_cfg, date_from = NULL, date_to = NULL, cache = NULL) {
  out <- list()
  for (rs in raw_series_cfg) {
    out[[rs$id]] <- fetch_ticker_data(
      con = con,
      ticker = rs$ticker,
      date_from = date_from,
      date_to = date_to,
      cache = cache
    )
  }
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
