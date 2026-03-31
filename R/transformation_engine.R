# Deklarative Transformations-Engine mit DAG-Auswertung

`%||%` <- function(x, y) if (is.null(x)) y else x

as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x)
}

transformation_registry <- function() {
  list(
    identity = op_identity,
    ratio = op_ratio,
    difference = op_difference,
    sma = op_sma,
    ema = op_ema,
    yoy_pct = op_yoy_pct
  )
}

validate_transformations_structure <- function(transformations) {
  errors <- list()

  if (!is.list(transformations)) {
    return(list(ok = FALSE, errors = list(list(code = "invalid_type", message = "transformations muss eine Liste sein."))))
  }

  required <- c("id", "type", "inputs", "params", "output_label")
  ids <- character(0)

  for (i in seq_along(transformations)) {
    tr <- transformations[[i]]
    miss <- setdiff(required, names(tr))
    if (length(miss) > 0) {
      errors[[length(errors) + 1]] <- list(
        code = "missing_fields",
        message = sprintf("Transformation %s: fehlende Felder: %s", tr$id %||% i, paste(miss, collapse = ", ")),
        transformation_id = tr$id %||% NA_character_
      )
    }
    ids <- c(ids, tr$id %||% NA_character_)
  }

  if (anyNA(ids) || anyDuplicated(ids) > 0) {
    errors[[length(errors) + 1]] <- list(
      code = "duplicate_or_missing_id",
      message = "Transformation IDs fehlen oder sind nicht eindeutig.",
      transformation_id = NA_character_
    )
  }

  list(ok = length(errors) == 0, errors = errors)
}

build_transformation_graph <- function(transformations) {
  nodes <- vapply(transformations, `[[`, character(1), "id")
  tr_map <- setNames(transformations, nodes)

  edges <- lapply(nodes, function(id) {
    tr <- tr_map[[id]]
    deps <- tr$inputs
    deps[deps %in% nodes] # nur Abhaengigkeit auf andere Transformationen
  })
  names(edges) <- nodes

  list(nodes = nodes, edges = edges, tr_map = tr_map)
}

check_references <- function(raw_data, transformations) {
  graph <- build_transformation_graph(transformations)
  available_raw <- names(raw_data)
  available_tr <- graph$nodes
  errors <- list()

  for (tr in transformations) {
    for (inp in tr$inputs) {
      known <- inp %in% available_raw || inp %in% available_tr
      if (!known) {
        errors[[length(errors) + 1]] <- list(
          code = "unknown_input_reference",
          message = sprintf("Transformation %s referenziert unbekannten Input %s", tr$id, inp),
          transformation_id = tr$id
        )
      }
    }
  }

  list(ok = length(errors) == 0, errors = errors)
}

check_circular_dependencies <- function(transformations) {
  graph <- build_transformation_graph(transformations)
  nodes <- graph$nodes
  edges <- graph$edges

  indeg <- setNames(integer(length(nodes)), nodes)
  for (n in nodes) {
    deps <- edges[[n]]
    if (length(deps) > 0) {
      indeg[n] <- length(deps)
    }
  }

  queue <- nodes[indeg == 0]
  processed <- character(0)

  while (length(queue) > 0) {
    n <- queue[[1]]
    queue <- queue[-1]
    processed <- c(processed, n)

    for (m in nodes) {
      if (n %in% edges[[m]]) {
        indeg[m] <- indeg[m] - 1L
        if (indeg[m] == 0L) queue <- c(queue, m)
      }
    }
  }

  if (length(processed) != length(nodes)) {
    cycle_nodes <- setdiff(nodes, processed)
    return(list(
      ok = FALSE,
      errors = list(list(
        code = "circular_dependency",
        message = sprintf("Zirkulaere Abhaengigkeit erkannt: %s", paste(cycle_nodes, collapse = ", ")),
        transformation_id = NA_character_
      ))
    ))
  }

  list(ok = TRUE, errors = list())
}

topo_sort_transformations <- function(transformations) {
  graph <- build_transformation_graph(transformations)
  nodes <- graph$nodes
  edges <- graph$edges

  indeg <- setNames(integer(length(nodes)), nodes)
  for (n in nodes) indeg[n] <- length(edges[[n]])

  queue <- nodes[indeg == 0]
  order <- character(0)

  while (length(queue) > 0) {
    n <- queue[[1]]
    queue <- queue[-1]
    order <- c(order, n)

    for (m in nodes) {
      if (n %in% edges[[m]]) {
        indeg[m] <- indeg[m] - 1L
        if (indeg[m] == 0L) queue <- c(queue, m)
      }
    }
  }

  if (length(order) != length(nodes)) {
    stop("Topologische Sortierung fehlgeschlagen (moeglicher Zyklus).", call. = FALSE)
  }

  order
}

# Frequenz-Harmonisierung ----------------------------------------------------

infer_frequency <- function(dates) {
  dates <- sort(unique(as_date(dates)))
  if (length(dates) < 3) return("unknown")
  d <- median(as.numeric(diff(dates)), na.rm = TRUE)
  if (d <= 1.5) return("daily")
  if (d <= 8) return("weekly")
  if (d <= 35) return("monthly")
  "unknown"
}

resample_to_frequency <- function(df, target_freq, method = "last") {
  stopifnot(all(c("date", "value") %in% names(df)))
  df$date <- as_date(df$date)
  df <- df[order(df$date), , drop = FALSE]

  bucket <- switch(
    target_freq,
    daily = df$date,
    weekly = as.Date(cut(df$date, "week")),
    monthly = as.Date(cut(df$date, "month")),
    stop("Ungueltige target_freq.", call. = FALSE)
  )

  split_df <- split(df$value, bucket)
  values <- vapply(split_df, function(x) {
    if (method == "last") tail(x, 1)
    else if (method == "mean") mean(x, na.rm = TRUE)
    else if (method == "sum") sum(x, na.rm = TRUE)
    else stop("Ungueltige Aggregationsmethode.", call. = FALSE)
  }, numeric(1))

  data.frame(date = as.Date(names(values)), value = as.numeric(values))
}

harmonise_raw_data <- function(raw_data, target_freq = "daily", aggregation_method = "last") {
  lapply(raw_data, function(df) {
    src_freq <- infer_frequency(df$date)
    if (identical(src_freq, target_freq) || identical(src_freq, "unknown")) {
      df$date <- as_date(df$date)
      df[order(df$date), c("date", "value"), drop = FALSE]
    } else {
      # Klare MVP-Regel: nur Aggregation von hoeherer auf tiefere Frequenz
      # daily -> weekly/monthly, weekly -> monthly erlaubt
      allowed <- (src_freq == "daily" && target_freq %in% c("weekly", "monthly")) ||
        (src_freq == "weekly" && target_freq == "monthly")
      if (!allowed) {
        stop(sprintf("Nicht erlaubte Frequenzkonvertierung: %s -> %s", src_freq, target_freq), call. = FALSE)
      }
      resample_to_frequency(df, target_freq = target_freq, method = aggregation_method)
    }
  })
}

# Engine --------------------------------------------------------------------

apply_transformations <- function(raw_data, transformations, freq_policy = list(target_freq = "daily", aggregation_method = "last")) {
  raw_data_h <- tryCatch(
    harmonise_raw_data(
      raw_data,
      target_freq = freq_policy$target_freq %||% "daily",
      aggregation_method = freq_policy$aggregation_method %||% "last"
    ),
    error = function(e) e
  )

  if (inherits(raw_data_h, "error")) {
    return(list(ok = FALSE, errors = list(list(code = "frequency_harmonisation_failed", message = conditionMessage(raw_data_h))), data = NULL))
  }

  s_val <- validate_transformations_structure(transformations)
  if (!s_val$ok) return(list(ok = FALSE, errors = s_val$errors, data = NULL))

  r_val <- check_references(raw_data_h, transformations)
  if (!r_val$ok) return(list(ok = FALSE, errors = r_val$errors, data = NULL))

  c_val <- check_circular_dependencies(transformations)
  if (!c_val$ok) return(list(ok = FALSE, errors = c_val$errors, data = NULL))

  order <- topo_sort_transformations(transformations)
  env <- list2env(raw_data_h, parent = emptyenv())
  errors <- list()

  for (tr_id in order) {
    tr <- transformations[[which(vapply(transformations, function(x) identical(x$id, tr_id), logical(1)))]]
    fn <- transformation_registry()[[tr$type]]

    if (is.null(fn)) {
      errors[[length(errors) + 1]] <- list(code = "unknown_operation", message = sprintf("Unbekannte Operation: %s", tr$type), transformation_id = tr$id)
      next
    }

    res <- tryCatch(fn(tr, env), error = function(e) e)
    if (inherits(res, "error")) {
      errors[[length(errors) + 1]] <- list(code = "execution_failed", message = conditionMessage(res), transformation_id = tr$id)
      next
    }

    assign(tr$id, res, envir = env)
  }

  if (length(errors) > 0) return(list(ok = FALSE, errors = errors, data = as.list(env)))

  list(ok = TRUE, errors = list(), data = as.list(env))
}

get_input_series <- function(input_ids, env) {
  lapply(input_ids, function(id) {
    if (!exists(id, envir = env, inherits = FALSE)) {
      stop(sprintf("Input-Serie nicht gefunden: %s", id), call. = FALSE)
    }
    get(id, envir = env, inherits = FALSE)
  })
}

merge_two_inputs <- function(a, b) {
  names(a) <- c("date", "a")
  names(b) <- c("date", "b")
  out <- merge(a, b, by = "date", all = FALSE)
  out[order(out$date), , drop = FALSE]
}

op_identity <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)
  inp[[1]]
}

op_ratio <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)
  merged <- merge_two_inputs(inp[[1]], inp[[2]])
  merged$value <- ifelse(merged$b == 0, NA_real_, merged$a / merged$b)
  merged[c("date", "value")]
}

op_difference <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)
  merged <- merge_two_inputs(inp[[1]], inp[[2]])
  merged$value <- merged$a - merged$b
  merged[c("date", "value")]
}

op_sma <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)[[1]]
  window <- as.integer(tr$params$window %||% 20L)
  if (window < 1) stop("sma: window muss >= 1 sein", call. = FALSE)
  inp <- inp[order(inp$date), , drop = FALSE]
  inp$value <- as.numeric(stats::filter(inp$value, rep(1 / window, window), sides = 1))
  inp
}

op_ema <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)[[1]]
  window <- as.integer(tr$params$window %||% 20L)
  if (window < 1) stop("ema: window muss >= 1 sein", call. = FALSE)
  alpha <- 2 / (window + 1)
  x <- inp$value
  ema <- numeric(length(x))
  if (length(x) > 0) {
    ema[1] <- x[1]
    if (length(x) > 1) {
      for (i in 2:length(x)) ema[i] <- alpha * x[i] + (1 - alpha) * ema[i - 1]
    }
  }
  inp$value <- ema
  inp
}

op_yoy_pct <- function(tr, env) {
  inp <- get_input_series(tr$inputs, env)[[1]]
  lag_n <- as.integer(tr$params$lag_n %||% 12L)
  if (lag_n < 1) stop("yoy_pct: lag_n muss >= 1 sein", call. = FALSE)
  inp <- inp[order(inp$date), , drop = FALSE]
  base <- dplyr::lag(inp$value, lag_n)
  inp$value <- (inp$value / base - 1) * 100
  inp
}
