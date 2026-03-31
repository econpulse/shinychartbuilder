# SQLite Storage Layer (Hybrid: relationale Metadaten + JSON Config)

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_json_parse <- function(txt) {
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) {
    list(.parse_error = TRUE, message = conditionMessage(e))
  })
}

init_storage <- function(db_path = "data/charts.sqlite") {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS charts (
      chart_id INTEGER PRIMARY KEY AUTOINCREMENT,
      chart_name TEXT NOT NULL,
      created_by TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      is_deleted INTEGER NOT NULL DEFAULT 0
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chart_configs (
      chart_config_id INTEGER PRIMARY KEY AUTOINCREMENT,
      chart_id INTEGER NOT NULL,
      revision INTEGER NOT NULL,
      config_json TEXT NOT NULL,
      config_hash TEXT,
      saved_at TEXT NOT NULL,
      FOREIGN KEY(chart_id) REFERENCES charts(chart_id),
      UNIQUE(chart_id, revision)
    )
  ")

  con
}

save_chart_config <- function(con, chart_config, chart_id = NULL, save_as_new = FALSE) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cfg_json <- jsonlite::toJSON(chart_config, auto_unbox = TRUE, null = "null")

  if (is.null(chart_id) || isTRUE(save_as_new)) {
    DBI::dbExecute(con,
      "INSERT INTO charts (chart_name, created_by, created_at, updated_at, is_deleted) VALUES (?, ?, ?, ?, 0)",
      params = list(chart_config$chart_meta$chart_name %||% "Unbenannt", chart_config$chart_meta$created_by %||% "system", now, now)
    )
    chart_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]
    revision <- 1L
  } else {
    DBI::dbExecute(con,
      "UPDATE charts SET chart_name = ?, updated_at = ? WHERE chart_id = ?",
      params = list(chart_config$chart_meta$chart_name %||% "Unbenannt", now, chart_id)
    )
    revision <- DBI::dbGetQuery(con,
      "SELECT COALESCE(MAX(revision), 0) + 1 AS rev FROM chart_configs WHERE chart_id = ?",
      params = list(chart_id)
    )$rev[[1]]
  }

  DBI::dbExecute(con,
    "INSERT INTO chart_configs (chart_id, revision, config_json, config_hash, saved_at) VALUES (?, ?, ?, ?, ?)",
    params = list(chart_id, revision, cfg_json, NA_character_, now)
  )

  list(chart_id = chart_id, revision = revision)
}

load_chart_config <- function(con, chart_id, revision = NULL) {
  if (is.null(revision)) {
    q <- DBI::dbGetQuery(con,
      "SELECT config_json, revision FROM chart_configs WHERE chart_id = ? ORDER BY revision DESC LIMIT 1",
      params = list(chart_id)
    )
  } else {
    q <- DBI::dbGetQuery(con,
      "SELECT config_json, revision FROM chart_configs WHERE chart_id = ? AND revision = ?",
      params = list(chart_id, revision)
    )
  }

  if (nrow(q) == 0) return(NULL)
  safe_json_parse(q$config_json[[1]])
}

list_chart_configs <- function(con, include_deleted = FALSE) {
  where <- if (isTRUE(include_deleted)) "" else "WHERE c.is_deleted = 0"
  sql <- sprintf(
    "SELECT c.chart_id, c.chart_name, c.updated_at, c.is_deleted, MAX(cc.revision) AS latest_revision
     FROM charts c
     LEFT JOIN chart_configs cc ON c.chart_id = cc.chart_id
     %s
     GROUP BY c.chart_id, c.chart_name, c.updated_at, c.is_deleted
     ORDER BY c.updated_at DESC",
    where
  )
  DBI::dbGetQuery(con, sql)
}

delete_chart_config <- function(con, chart_id, soft_delete = TRUE) {
  if (isTRUE(soft_delete)) {
    DBI::dbExecute(con,
      "UPDATE charts SET is_deleted = 1, updated_at = ? WHERE chart_id = ?",
      params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), chart_id)
    )
  } else {
    DBI::dbExecute(con, "DELETE FROM chart_configs WHERE chart_id = ?", params = list(chart_id))
    DBI::dbExecute(con, "DELETE FROM charts WHERE chart_id = ?", params = list(chart_id))
  }
  invisible(TRUE)
}
