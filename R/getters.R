#' Get a canton label by key
#'
#' @param key key under `canton/`, e.g. "BE_d"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (usually character)
#' @export
glcant <- function(key, default = NULL, error = FALSE) {
  glget(paste0("canton/", key), default = default, error = error)
}

#' Get a country label by key
#'
#' @param key key under `country/`, e.g. "CHE"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (usually character)
#' @export
glcnt <- function(key, default = NULL, error = FALSE) {
  glget(paste0("country/", key), default = default, error = error)
}

#' Get a domain label by key
#'
#' @param key key under `domain/`, e.g. "math"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (usually character)
#' @export
gldom <- function(key, default = NULL, error = FALSE) {
  glget(paste0("domain/", key), default = default, error = error)
}

#' Get a statistical term by key
#'
#' @param key key under `stat/`, e.g. "p_value"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (usually character)
#' @export
glstat <- function(key, default = NULL, error = FALSE) {
  glget(paste0("stat/", key), default = default, error = error)
}

#' Get a generic term by key
#'
#' @param key key under `term/`, e.g. "and"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (usually character)
#' @export
glterm <- function(key, default = NULL, error = FALSE) {
  glget(paste0("term/", key), default = default, error = error)
}

#' Get a feature entry by key
#'
#' @param key key under `feature/`, e.g. "homelang"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved value (can be list: e.g. var + level)
#' @export
glfeat <- function(key, default = NULL, error = FALSE) {
  glget(paste0("feature/", key), default = default, error = error)
}

#' Get a format string from the glossary
#'
#' @param key key under `format/`, e.g. "points"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return a character format string
#' @export
glformat <- function(key, default = NULL, error = FALSE) {
  glget(.gl_path("format", key), default = default, error = error)
}

#' Format values using a glossary format string
#'
#' @param key key under `format/`, e.g. "points"
#' @param ... values to insert; unnamed values are matched by placeholder order
#' @return formatted character string
#' @export
glfmt <- function(key, ...) {
  fmt <- glget(.gl_path("format", key), error = TRUE)
  
  if (!is.character(fmt) || length(fmt) != 1L) {
    stop("glfmt(): format must be a single character string.")
  }
  
  args <- list(...)
  nms <- names(args)
  
  # extract placeholder names in order of appearance
  ph_raw <- regmatches(fmt, gregexpr("%<([^>]+)>", fmt, perl = TRUE))[[1]]
  ph <- sub("^%<|>$", "", ph_raw)
  
  # translate %<name> to % for sprintf
  fmt2 <- gsub("%<[^>]+>", "%", fmt)
  
  if (length(ph) == 0L) {
    return(do.call(sprintf, c(list(fmt2), args)))
  }
  
  # assign names to unnamed args by placeholder order
  if (is.null(nms)) nms <- rep("", length(args))
  unnamed <- which(nms == "")
  
  if (length(unnamed) > length(ph)) {
    stop("glfmt(): too many unnamed values for format placeholders.")
  }
  
  nms[unnamed] <- ph[seq_along(unnamed)]
  names(args) <- nms
  
  do.call(sprintf, c(list(fmt2), args))
}




