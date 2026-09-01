#' Identify a pasted space character
#'
#' Takes one or more characters and reports key, name, code point, R escape
#' and HTML entity for each of them.
#'
#' @param x A single string, e.g. `checkspace(" ")`. Paste the character
#'   between the quotes. If omitted, the clipboard is read instead.
#' @return Invisibly, a data frame with one row per character.
#' @export
#'
#' @examples
#' checkspace("\u202f")
#' checkspace()          # reads from the clipboard
checkspace <- function(x) {
  
  if (missing(x)) {
    if (!requireNamespace("clipr", quietly = TRUE) || !clipr::clipr_available()) {
      stop("No input supplied and clipboard not available.", call. = FALSE)
    }
    x <- paste(clipr::read_clip(), collapse = "")
    message("Read from clipboard.")
  }
  if (!is.character(x) || length(x) != 1L) {
    stop("`x` must be a single character string.", call. = FALSE)
  }
  if (!nzchar(x)) {
    message("Empty string - nothing to check.")
    return(invisible(NULL))
  }
  
  cps <- utf8ToInt(enc2utf8(x))
  ref <- space_table()
  i   <- match(cps, ref$cp)
  
  out <- data.frame(
    pos     = seq_along(cps),
    key     = ifelse(is.na(i), "-", ref$key[i]),
    name    = ifelse(is.na(i), "not a listed space character", ref$name[i]),
    unicode = sprintf("U+%04X", cps),
    # \u only covers the BMP; anything above needs \U
    escape  = ifelse(cps > 0xFFFF, sprintf("\\U%08x", cps), sprintf("\\u%04x", cps)),
    entity  = sprintf("&#%d;", cps),
    stringsAsFactors = FALSE
  )
  
  cols  <- c("key", "name", "unicode", "escape", "entity")
  multi <- length(cps) > 1L
  pad   <- function(v, header) {
    formatC(c(header, v), width = max(nchar(c(header, v))), flag = "-")
  }
  block <- do.call(cbind, Map(pad, out[cols], c("key", "name", "unicode", "R", "entity")))
  if (multi) block <- cbind(pad(out$pos, "#"), block)
  
  cat("\n")
  cat(apply(block, 1L, paste, collapse = "  "), sep = "\n")
  if (any(is.na(i))) {
    cat("\nRun listspaces() to see all known space characters.\n")
  }
  cat("\n")
  
  invisible(out)
}