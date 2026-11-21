#' Convert local device times to UTC
#'
#' @description
#' Cleaning entry point for device logs whose timestamps are recorded as local
#' wall-clock time. Each device is associated with a known IANA time zone, and
#' the records are ordered by actual collection order.
#' `local_time_to_utc()` appends a `UTC` column with the corresponding
#' [POSIXct] instants while leaving the original columns and row order
#' untouched.
#'
#' Two kinds of wall-clock times need special treatment around daylight saving
#' transitions:
#'
#' * A wall-clock time that does not exist (the "gap" created by a clock jump
#'   forward, e.g. `02:30` on the day clocks move from `02:00` to `03:00`)
#'   cannot be mapped to any instant.
#' * A wall-clock time that occurs twice (the "fold" created by a clock jump
#'   back, e.g. `02:30` on the day clocks return from `03:00` to `02:00`)
#'   maps to two different instants.
#'
#' Folds are resolved with the help of the complete record sequence of the
#' device. For every pair of adjacent records, the actual interval between the
#' chosen instants must lie within the closed interval
#' `[min_interval, max_interval]` (in seconds). A record later in the sequence
#' can therefore remove ambiguity introduced earlier. The sequences of
#' instants are not enumerated: candidate counts are propagated with a
#' bounded-count dynamic program (the count is capped at two, "one" versus
#' "more than one") in time and memory linear in the number of records.
#'
#' A device's batch fails, and the whole call errors, if any of its records
#' falls in a gap, if no sequence satisfies the interval constraints, or if
#' more than one sequence remains. The error identifies the device and the
#' involved source rows.
#'
#' @param .data A data frame containing device log records, in actual
#'   collection order within each device.
#' @param device_col,tz_col,local_col Names (bare name or string) of the
#'   device identifier, IANA time zone (see [OlsonNames()]), and local
#'   wall-clock time columns. `local_col` may be a character column (parsed
#'   with the usual `YYYY-MM-DD HH:MM:SS` formats) or a [POSIXct] column,
#'   whose displayed wall-clock time in its own time zone is interpreted.
#' @param min_interval,max_interval Single numeric values, the inclusive lower
#'   and upper bound in seconds for the actual interval between adjacent
#'   records of one device. `0 <= min_interval <= max_interval`.
#' @param .name Name of the appended column, defaults to `"UTC"`. If a column
#'   of that name already exists it is replaced.
#'
#' @return A [tibble()] with the same rows, in the original order, and all
#'   original columns unchanged, plus a `POSIXct` column (time zone `"UTC"`)
#'   holding the resolved instants.
#' @export
#' @examples
#' logs <- tibble::tibble(
#'   device = "dev-1",
#'   tz = "Europe/Berlin",
#'   local = c("2021-10-31 02:30:00", "2021-10-31 04:00:00")
#' )
#'
#' # The 02:30 fold is resolved by the constraint on the adjacent interval:
#' # with a 90-minute window, only the second occurrence (UTC 01:30) is
#' # 90 minutes before the 04:00 record (UTC 03:00).
#' tryCatch(
#'   local_time_to_utc(logs, device, tz, local,
#'     min_interval = 5400, max_interval = 5400
#'   ),
#'   error = function(e) message(e$message)
#' )
local_time_to_utc <- function(.data,
                              device_col,
                              tz_col,
                              local_col,
                              min_interval,
                              max_interval,
                              .name = "UTC") {
  if (!is.data.frame(.data)) {
    abort_local_time_not_data_frame(.data)
  }
  device_col <- as_name(ensym(device_col))
  tz_col <- as_name(ensym(tz_col))
  local_col <- as_name(ensym(local_col))
  check_col_exists(.data, device_col, "device_col")
  check_col_exists(.data, tz_col, "tz_col")
  check_col_exists(.data, local_col, "local_col")
  check_interval(min_interval, "min_interval")
  check_interval(max_interval, "max_interval")
  if (min_interval > max_interval) {
    abort_local_time_interval_order(min_interval, max_interval)
  }
  if (!is_string(.name) || !nzchar(.name)) {
    abort_local_time_bad_name(.name)
  }

  wall <- parse_wall_column(.data[[local_col]])
  devices <- .data[[device_col]]
  zones <- .data[[tz_col]]
  n <- nrow(.data)

  issues <- list()
  # Numeric UTC instants while processing; POSIXct is constructed once at the
  # end because piecewise assignment into a POSIXct vector drops its time zone.
  utc_num <- rep(NA_real_, n)
  if (n == 0L) {
    utc <- as.POSIXct(utc_num, origin = "1970-01-01", tz = "UTC")
    return(append_utc_column(.data, utc, .name))
  }

  # One cache per call is no longer needed: offset lookups are vectorized.
  split_idx <- split(seq_len(n), devices, drop = TRUE)

  for (dev in names(split_idx)) {
    idx <- split_idx[[dev]]
    dev_zones <- unique(zones[idx])
    if (length(dev_zones) != 1L || is.na(dev_zones) || !nzchar(dev_zones)) {
      issues <- c(
        issues,
        list(local_issue(
          device = dev, kind = "inconsistent_tz", rows = idx,
          detail = paste0(
            "time zone column must contain one known IANA zone per device, ",
            "got: ", paste(unique(format(dev_zones)), collapse = ", ")
          )
        ))
      )
      next
    }
    zone <- dev_zones
    if (!is_valid_tz(zone)) {
      issues <- c(
        issues,
        list(local_issue(
          device = dev, kind = "unknown_tz", rows = idx,
          detail = paste0("unknown IANA time zone: ", format(zone))
        ))
      )
      next
    }

    dev_issues <- local_input_issues(wall[idx], idx)
    if (length(dev_issues) > 0L) {
      issues <- c(issues, dev_issues)
      next
    }

    cand <- device_candidates(wall[idx], zone)
    gap_pos <- which(lengths(cand) == 0L)
    if (length(gap_pos) > 0L) {
      issues <- c(
        issues,
        list(local_issue(
          device = dev, kind = "gap", rows = idx[gap_pos],
          detail = "wall-clock time does not exist in this time zone (clock jump forward)"
        ))
      )
      next
    }

    solved <- solve_device(cand, min_interval, max_interval)
    if (!is.null(solved$issue)) {
      issues <- c(
        issues,
        list(local_issue(
          device = dev,
          kind = solved$issue$kind,
          rows = idx[solved$issue$positions],
          detail = solved$issue$detail
        ))
      )
    } else {
      utc_num[idx] <- solved$path
    }
  }

  if (length(issues) > 0L) {
    abort_local_time_conversion(issues)
  }

  utc <- as.POSIXct(utc_num, origin = "1970-01-01", tz = "UTC")
  append_utc_column(.data, utc, .name)
}

# Wall-clock parsing ---------------------------------------------------------

parse_wall_column <- function(x) {
  if (inherits(x, "POSIXct")) {
    # Interpret the displayed wall-clock time in the column's own time zone.
    return(as.numeric(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%OS"), tz = "UTC")))
  }
  if (is.character(x)) {
    return(parse_wall_character(x))
  }
  abort_local_time_unsupported(typeof(x))
}

parse_wall_character <- function(x) {
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%OS",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
  out <- rep(NA_real_, length(x))
  pending <- !is.na(x) & nzchar(x)
  for (fmt in formats) {
    if (!any(pending)) break
    parsed <- suppressWarnings(as.POSIXct(x[pending], tz = "UTC", format = fmt))
    ok <- !is.na(parsed)
    if (any(ok)) {
      out[which(pending)[ok]] <- as.numeric(parsed[ok])
      pending[which(pending)[ok]] <- FALSE
    }
  }
  out
}

local_input_issues <- function(wall, idx) {
  bad <- which(is.na(wall))
  if (length(bad) == 0L) {
    return(list())
  }
  list(local_issue(
    device = NULL, kind = "unparseable", rows = idx[bad],
    detail = "missing or unparseable local wall-clock time"
  ))
}

# Local wall time -> candidate UTC instants ---------------------------------

# Offsets (seconds east of UTC) in force at the given UTC instants within a
# zone. `as.POSIXlt()` resolves the zone rules (including historical LMT and
# fractional-hour zones) in one vectorized call; `gmtoff` is the offset in
# force at each instant.
tz_offsets <- function(inst, tz) {
  as.POSIXlt(.POSIXct(inst, tz = tz))$gmtoff
}

# For every wall-clock value in a device's record sequence, return the sorted
# UTC instants whose representation in `tz` is that wall time: zero (gap),
# one (unambiguous) or two (fold) candidates per record.
device_candidates <- function(wall, tz) {
  days <- as.integer(floor(wall / 86400))
  n <- length(wall)
  # One offset set per wall day. Sampling one full day on either side covers
  # every transition; at most two offsets occur near one transition.
  day_levels <- sort(unique(days))
  day_off <- lapply(day_levels, function(day) {
    samples <- day * 86400 + (-24L:24L) * 3600L
    sort(unique(tz_offsets(samples, tz)))
  })
  names(day_off) <- as.character(day_levels)

  # Build every tentative candidate (wall - offset) in one vector, then verify
  # all of them in a single vectorized zone lookup.
  row_id <- rep(seq_len(n), lengths(day_off[as.character(days)]))
  off <- unlist(day_off[as.character(days)], use.names = FALSE)
  tentative <- wall[row_id] - off
  good <- tz_offsets(tentative, tz) == off
  keep <- tentative[good]
  keep_row <- row_id[good]

  out <- vector("list", n)
  if (length(keep) > 0L) {
    ord <- order(keep_row, keep)
    split_keep <- split(keep[ord], keep_row[ord])
    out[as.integer(names(split_keep))] <- unname(split_keep)
  }
  out
}

is_valid_tz <- function(tz) {
  is_string(tz) && tz %in% OlsonNames()
}

# Bounded-count dynamic program ---------------------------------------------

# Given per-row candidate instants (each row 1 or 2 candidates), find all
# sequences whose adjacent deltas fall in [lo, hi]. Counts are capped at 2.
# Returns the unique path, or an issue describing why resolution failed.
solve_device <- function(cand, lo, hi) {
  n <- length(cand)
  m <- lengths(cand)
  fwd <- vector("list", n)
  bwd <- vector("list", n)
  pred <- vector("list", n)

  for (i in seq_len(n)) {
    k <- m[i]
    f <- integer(k)
    p <- integer(k)
    if (i == 1L) {
      f[] <- 1L
    } else {
      prev <- cand[[i - 1L]]
      pf <- fwd[[i - 1L]]
      for (j in seq_len(k)) {
        d <- cand[[i]][j] - prev
        ok <- d >= lo & d <= hi & pf > 0L
        if (any(ok)) {
          f[j] <- min(2L, sum(pmin(pf[ok], 2L)))
          p[j] <- which(ok)[1L]
        }
      }
    }
    fwd[[i]] <- f
    pred[[i]] <- p
  }

  for (i in n:1L) {
    k <- m[i]
    b <- integer(k)
    if (i == n) {
      b[] <- 1L
    } else {
      nxt <- cand[[i + 1L]]
      nb <- bwd[[i + 1L]]
      for (j in seq_len(k)) {
        d <- nxt - cand[[i]][j]
        ok <- d >= lo & d <= hi & nb > 0L
        if (any(ok)) {
          b[j] <- min(2L, sum(pmin(nb[ok], 2L)))
        }
      }
    }
    bwd[[i]] <- b
  }

  total <- if (n == 0L) 0L else min(2L, sum(pmin(fwd[[n]], 2L)))
  if (total == 0L) {
    return(list(issue = infeasibility_issue(cand, fwd, lo, hi)))
  }

  viable <- lapply(
    seq_len(n), function(i) which(fwd[[i]] > 0L & bwd[[i]] > 0L)
  )
  ambiguous <- which(lengths(viable) >= 2L)
  if (total >= 2L || length(ambiguous) > 0L) {
    return(list(issue = list(
      kind = "ambiguous",
      positions = ambiguous,
      detail = paste0(
        "multiple sequences satisfy the closed interval [", lo, ", ", hi,
        "] seconds; folds at these rows cannot be resolved uniquely"
      )
    )))
  }

  # Exactly one global explanation: backtrack through predecessor pointers.
  path <- numeric(n)
  j <- which(fwd[[n]] > 0L & bwd[[n]] > 0L)
  stopifnot(length(j) == 1L)
  path[n] <- cand[[n]][j]
  if (n >= 2L) {
    for (i in (n - 1L):1L) {
      j <- pred[[i + 1L]][j]
      path[i] <- cand[[i]][j]
    }
  }
  list(path = path)
}

# Locate where the forward pass dies and report the two adjacent rows.
infeasibility_issue <- function(cand, fwd, lo, hi) {
  n <- length(cand)
  dead <- which(vapply(fwd, function(f) all(f == 0L), logical(1)))[1L]
  if (is.na(dead)) dead <- n
  list(
    kind = "infeasible",
    positions = c(dead - 1L, dead),
    detail = paste0(
      "no sequence keeps every adjacent interval within the closed interval [",
      lo, ", ", hi, "] seconds; constraint first violated between these rows"
    )
  )
}

# Helpers --------------------------------------------------------------------

local_issue <- function(device, kind, rows, detail) {
  list(device = device, kind = kind, rows = as.integer(rows), detail = detail)
}

check_col_exists <- function(data, name, arg) {
  if (!name %in% names(data)) {
    abort_local_time_missing_column(name, arg)
  }
}

check_interval <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    abort_local_time_bad_interval(x, arg)
  }
}

append_utc_column <- function(data, utc, name) {
  out <- as.list(data)
  out[[name]] <- utc
  out <- new_data_frame(out, n = nrow(data))
  vectbl_restore(out, data)
}

# Errors ---------------------------------------------------------------------

abort_local_time_conversion <- function(issues) {
  bullets <- vapply(issues, function(issue) {
    rows <- issue$rows
    row_text <- if (length(rows) <= 6L) {
      paste(rows, collapse = ", ")
    } else {
      paste0(
        paste(rows[1:5], collapse = ", "), ", ... (",
        length(rows), " rows in total)"
      )
    }
    dev <- if (is.null(issue$device)) {
      ""
    } else {
      paste0(" device ", tick(issue$device), ",")
    }
    paste0(dev, " source row(s) ", row_text, ": ", issue$detail)
  }, character(1))

  tibble_abort(
    call = caller_env(),
    problems(
      paste0(
        "Local-to-UTC conversion failed for ",
        length(unique(map_chr(issues, function(i) {
          if (is.null(i$device)) "<unknown>" else i$device
        }))),
        " device(s):"
      ),
      bullets
    ),
    issues = issues
  )
}

abort_local_time_not_data_frame <- function(x, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`.data` must be a data frame, not ", obj_sum(x), ".")
  )
}

abort_local_time_missing_column <- function(name, arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Column ", tick(name), " (from `", arg, "`) can't be found in `.data`."
    )
  )
}

abort_local_time_bad_interval <- function(x, arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "`", arg, "` must be one finite, non-negative number of seconds, not ",
      obj_sum(x), "."
    )
  )
}

abort_local_time_interval_order <- function(lo, hi, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "`min_interval` (", lo, ") must not be greater than `max_interval` (",
      hi, ")."
    )
  )
}

abort_local_time_bad_name <- function(x, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`.name` must be a non-empty string, not ", obj_sum(x), ".")
  )
}

abort_local_time_unsupported <- function(type, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "The local time column must be character or POSIXct, not ", type, "."
    )
  )
}
