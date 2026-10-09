##' @param secs 
##' @examples 
##' format_secs(45)          # "45s"
##' format_secs(125)         # "2m5s"
##' format_secs(3725)        # "1h2m5s"
##' format_secs(90125)       # "1d1h2m" (seconds dropped, max 3 units)
##' format_secs(90000)       # "1d1h"
##' @keywords internal

format_secs <- function(secs) {
  vapply(secs, format_secs_one, character(1))
}

format_secs_one <- function(secs) {
  if (is.na(secs)) return(NA_character_)

  secs <- round(secs)
  d <- secs %/% 86400
  h <- (secs %% 86400) %/% 3600
  m <- (secs %% 3600) %/% 60
  s <- secs %% 60

  units <- c(d = d, h = h, m = m, s = s)
  labels <- c("d", "h", "m", "s")

  # Drop seconds if >= 1 day
  if (d > 0) {
    units <- units[c("d", "h", "m")]
    labels <- c("d", "h", "m")
  }

  # Keep only units from the first non-zero one onward
  first_nonzero <- which(units > 0)[1]
  if (is.na(first_nonzero)) first_nonzero <- length(units)  # all zero -> show smallest unit (s or m)

  units  <- units[first_nonzero:length(units)]
  labels <- labels[first_nonzero:length(labels)]

  # limit to max 3 units
  keep <- seq_len(min(3, length(units)))
  units  <- units[keep]
  labels <- labels[keep]

  paste0(mapply(function(u, l) paste0(u, l), units, labels), collapse = "")
}

