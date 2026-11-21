# local_time_to_utc --------------------------------------------------------

berlin <- "Europe/Berlin"

fold_tbl <- function(
  local = c(
    "2021-10-31 02:30:00",
    "2021-10-31 04:00:00"
  ),
  device = "dev-1",
  tz = berlin
) {
  tibble(device = device, tz = tz, local = local)
}

utc_chr <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

test_that("unambiguous times convert and input is preserved", {
  skip_if_not(berlin %in% OlsonNames())
  logs <- tibble(
    device = c("a", "a", "b"),
    tz = c(berlin, berlin, "Asia/Shanghai"),
    local = c(
      "2021-06-15 12:00:00",
      "2021-06-15 12:10:00",
      "2021-03-28 10:30:00"
    ),
    value = c(1.5, 2.5, 3.5)
  )
  out <- local_time_to_utc(logs, device, tz, local, 0, 3600)

  expect_s3_class(out, "tbl_df")
  expect_identical(out$local, logs$local)
  expect_identical(out$value, logs$value)
  expect_identical(nrow(out), nrow(logs))
  expect_s3_class(out$UTC, "POSIXct")
  expect_identical(attr(out$UTC, "tzone"), "UTC")
  expect_identical(utc_chr(out$UTC[1]), "2021-06-15 10:00:00")
  expect_identical(utc_chr(out$UTC[2]), "2021-06-15 10:10:00")
  expect_identical(utc_chr(out$UTC[3]), "2021-03-28 02:30:00")
})

test_that("fold is resolved by a later record", {
  skip_if_not(berlin %in% OlsonNames())
  # 02:30 fold instants are 00:30 / 01:30 UTC; 04:00 local is 03:00 UTC.
  # Deltas are 150 min vs 90 min. A 90-minute window forces the second fold.
  out <- local_time_to_utc(fold_tbl(), device, tz, local, 5400, 5400)
  expect_identical(utc_chr(out$UTC[1]), "2021-10-31 01:30:00")
  expect_identical(utc_chr(out$UTC[2]), "2021-10-31 03:00:00")

  # A 150-minute window forces the first fold instead.
  out2 <- local_time_to_utc(fold_tbl(), device, tz, local, 9000, 9000)
  expect_identical(utc_chr(out2$UTC[1]), "2021-10-31 00:30:00")
})

test_that("a fold not uniquely resolvable fails the whole batch", {
  skip_if_not(berlin %in% OlsonNames())
  cnd <- expect_error(
    local_time_to_utc(fold_tbl(), device, tz, local, 0, 100000),
    class = "tibble_error_local_time_conversion"
  )
  expect_s3_class(cnd, "tibble_error")
  expect_identical(cnd$issues[[1]]$kind, "ambiguous")
  expect_identical(cnd$issues[[1]]$rows, 1L)
  expect_identical(cnd$issues[[1]]$device, "dev-1")
  expect_match(conditionMessage(cnd), "dev-1")
  expect_match(conditionMessage(cnd), "source row")
})

test_that("a nonexistent (gap) wall time fails with device and rows", {
  skip_if_not(berlin %in% OlsonNames())
  logs <- tibble(
    device = "dev-2",
    tz = berlin,
    local = c("2021-03-28 02:30:00", "2021-03-28 04:00:00")
  )
  cnd <- expect_error(
    local_time_to_utc(logs, device, tz, local, 0, 99999),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "gap")
  expect_identical(cnd$issues[[1]]$rows, 1L)
  expect_identical(cnd$issues[[1]]$device, "dev-2")
})

test_that("no feasible sequence reports the rows where it breaks", {
  skip_if_not(berlin %in% OlsonNames())
  cnd <- expect_error(
    local_time_to_utc(fold_tbl(), device, tz, local, 1, 10),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "infeasible")
  expect_identical(cnd$issues[[1]]$rows, c(1L, 2L))
})

test_that("a single fold record with no neighbour is ambiguous", {
  skip_if_not(berlin %in% OlsonNames())
  one <- fold_tbl(local = "2021-10-31 02:30:00")
  cnd <- expect_error(
    local_time_to_utc(one, device, tz, local, 0, 10),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "ambiguous")

  # A single unambiguous record is fine.
  ok <- fold_tbl(local = "2021-06-15 12:00:00")
  out <- local_time_to_utc(ok, device, tz, local, 0, 10)
  expect_identical(utc_chr(out$UTC), "2021-06-15 10:00:00")
})

test_that("ambiguity introduced early is resolved far downstream", {
  skip_if_not(berlin %in% OlsonNames())
  # Exact 10-minute UTC cadence across the fold; the unique terminal chain
  # forces every earlier fold choice.
  inst <- as.POSIXct("2021-10-30 20:00:00", tz = "UTC") + (0:39) * 600
  local <- format(inst, "%Y-%m-%d %H:%M:%S", tz = berlin)
  logs <- fold_tbl(local = local)
  out <- local_time_to_utc(logs, device, tz, local, 600, 3600)

  expect_identical(as.numeric(sort(out$UTC)), as.numeric(inst))
  expect_true(all(diff(as.numeric(out$UTC[order(out$UTC)])) >= 600))
})

test_that("southern-hemisphere fold and gap are handled", {
  skip_if_not("Australia/Sydney" %in% OlsonNames())
  fold <- tibble(
    device = "sy",
    tz = "Australia/Sydney",
    local = c("2021-04-04 02:30:00", "2021-04-04 04:00:00")
  )
  out <- local_time_to_utc(fold, device, tz, local, 5400, 5400)
  expect_identical(utc_chr(out$UTC[1]), "2021-04-03 16:30:00")

  gap <- tibble(
    device = "sy2",
    tz = "Australia/Sydney",
    local = "2021-10-03 02:30:00"
  )
  cnd <- expect_error(
    local_time_to_utc(gap, device, tz, local, 0, 10),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "gap")
})

test_that("fractional seconds are preserved", {
  skip_if_not(berlin %in% OlsonNames())
  frac <- tibble(
    device = "f",
    tz = berlin,
    local = c("2021-06-15 12:00:00.25", "2021-06-15 12:00:01.75")
  )
  out <- local_time_to_utc(frac, device, tz, local, 0, 10)
  expect_equal(as.numeric(out$UTC[2]) - as.numeric(out$UTC[1]), 1.5)
})

test_that("POSIXct input uses its displayed wall time", {
  skip_if_not(berlin %in% OlsonNames())
  pc <- as.POSIXct(
    c("2021-06-15 12:00:00", "2021-06-15 12:05:00"),
    tz = berlin
  )
  ct <- tibble(device = "p", tz = berlin, local = pc)
  out <- local_time_to_utc(ct, device, tz, local, 0, 3600)
  expect_identical(utc_chr(out$UTC[1]), "2021-06-15 10:00:00")

  # String column names are accepted too.
  out2 <- local_time_to_utc(ct, "device", "tz", "local", 0, 3600)
  expect_identical(out2$UTC, out$UTC)
})

test_that("original row order is retained with interleaved devices", {
  inter <- tibble(
    device = c("a", "b", "a", "b"),
    tz = rep("UTC", 4),
    local = c(
      "2021-01-01 00:00:00",
      "2021-02-01 00:00:00",
      "2021-01-01 00:10:00",
      "2021-02-01 00:10:00"
    )
  )
  out <- local_time_to_utc(inter, device, tz, local, 0, 3600)
  expect_identical(out$local, inter$local)
  expect_identical(utc_chr(out$UTC[1]), "2021-01-01 00:00:00")
  expect_identical(utc_chr(out$UTC[2]), "2021-02-01 00:00:00")
})

test_that("failures across several devices are aggregated", {
  mix <- tibble(
    device = c("good", "good", "bad-gap", "bad-fold"),
    tz = c("UTC", "UTC", berlin, berlin),
    local = c(
      "2021-01-01 00:00:00",
      "2021-01-01 00:10:00",
      "2021-03-28 02:30:00",
      "2021-10-31 02:30:00"
    )
  )
  cnd <- expect_error(
    local_time_to_utc(mix, device, tz, local, 0, 3600),
    class = "tibble_error_local_time_conversion"
  )
  devs <- vapply(cnd$issues, function(i) i$device, character(1))
  expect_setequal(devs, c("bad-gap", "bad-fold"))
  expect_match(conditionMessage(cnd), "2 device")
})

test_that("unparseable and missing times are reported", {
  bad <- tibble(
    device = "u",
    tz = "UTC",
    local = c("2021-01-01 00:00:00", "nonsense", NA)
  )
  cnd <- expect_error(
    local_time_to_utc(bad, device, tz, local, 0, 3600),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "unparseable")
  expect_identical(cnd$issues[[1]]$rows, c(2L, 3L))
})

test_that("unknown and inconsistent time zones are rejected", {
  unknown <- tibble(
    device = "x",
    tz = "Not/AZone",
    local = "2021-01-01 00:00:00"
  )
  cnd <- expect_error(
    local_time_to_utc(unknown, device, tz, local, 0, 10),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd$issues[[1]]$kind, "unknown_tz")

  inconsistent <- tibble(
    device = "x",
    tz = c("UTC", berlin),
    local = c("2021-01-01 00:00:00", "2021-01-01 00:10:00")
  )
  cnd2 <- expect_error(
    local_time_to_utc(inconsistent, device, tz, local, 0, 3600),
    class = "tibble_error_local_time_conversion"
  )
  expect_identical(cnd2$issues[[1]]$kind, "inconsistent_tz")
})

test_that("argument validation errors", {
  logs <- fold_tbl()
  expect_error(
    local_time_to_utc(logs, nope, tz, local, 0, 3600),
    class = "tibble_error"
  )
  expect_error(
    local_time_to_utc(logs, device, tz, local, -1, 10),
    class = "tibble_error"
  )
  expect_error(
    local_time_to_utc(logs, device, tz, local, 10, 1),
    class = "tibble_error"
  )
  expect_error(
    local_time_to_utc("not a df", device, tz, local, 0, 10),
    class = "tibble_error"
  )
})

test_that("empty input returns an empty tibble with a UTC column", {
  empty <- tibble(device = character(), tz = character(), local = character())
  out <- local_time_to_utc(empty, device, tz, local, 0, 10)
  expect_identical(nrow(out), 0L)
  expect_s3_class(out$UTC, "POSIXct")
  expect_identical(attr(out$UTC, "tzone"), "UTC")
})

test_that("the output column can be renamed and replaced", {
  skip_if_not(berlin %in% OlsonNames())
  logs <- fold_tbl(local = "2021-06-15 12:00:00")
  out <- local_time_to_utc(logs, device, tz, local, 0, 10, .name = "when")
  expect_true("when" %in% names(out))
  expect_false("UTC" %in% names(out))

  logs2 <- tibble(
    device = "d",
    tz = berlin,
    local = "2021-06-15 12:00:00",
    UTC = 1
  )
  out2 <- local_time_to_utc(logs2, device, tz, local, 0, 10)
  expect_s3_class(out2$UTC, "POSIXct")
})

test_that("resolution scales linearly without enumerating sequences", {
  skip_if_not(berlin %in% OlsonNames())
  # A long chain of fold-bearing records resolves to one path; a quadratic
  # enumeration of fold combinations would make many rows prohibitive.
  n <- 2000L
  inst <- as.POSIXct("2021-10-30 20:00:00", tz = "UTC") +
    (seq_len(n) - 1L) * 600
  logs <- fold_tbl(local = format(inst, "%Y-%m-%d %H:%M:%S", tz = berlin))
  out <- local_time_to_utc(logs, device, tz, local, 600, 3600)
  expect_identical(as.numeric(sort(out$UTC)), as.numeric(inst))
})
