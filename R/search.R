#' Search keys and values in the active glossary
#'
#' @param query character(1). Regular expression (default) or plain substring.
#' @param where optional top-level key to restrict search, e.g. "label", "format", "write".
#' @param scope search in "path", "value", or "both".
#' @param in search in "path", "value", or "both".
#' @param regex logical. If FALSE, treat query as fixed substring.
#' @param ignore_case logical. If TRUE, match case-insensitively.
#' @param n maximum number of results to return.
#' @return data.frame with columns: path, value, type
#' @export
glsearch <- function(
    query,
    where = NULL,
    scope = c("both", "path", "value"),
    regex = TRUE,
    ignore_case = TRUE,
    n = 50L
) {
  if (missing(query) || !is.character(query) || length(query) != 1L) {
    stop("glsearch(): 'query' must be a single character string.")
  }
  scope <- match.arg(scope)
  
  idx <- .gl_index_get()
  
  if (!is.null(where)) {
    if (!is.character(where) || length(where) != 1L || !nzchar(where)) {
      stop("glsearch(): 'where' must be a single non-empty string or NULL.")
    }
    idx <- idx[idx$type == where, , drop = FALSE]
  }
  
  if (!nrow(idx)) return(idx)
  
  hay_path <- idx$path
  hay_val  <- idx$value
  
  match_one <- function(hay) {
    if (isTRUE(regex)) {
      grepl(query, hay, perl = TRUE, ignore.case = isTRUE(ignore_case))
    } else {
      grepl(query, hay, fixed = TRUE, ignore.case = isTRUE(ignore_case))
    }
  }
  
  keep <- switch(
    scope,
    path  = match_one(hay_path),
    value = match_one(hay_val),
    both  = match_one(hay_path) | match_one(hay_val)
  )
  
  
  res <- idx[keep, , drop = FALSE]
  
  # shorten long previews
  if (nrow(res)) {
    res$value <- ifelse(nchar(res$value) > 120, paste0(substr(res$value, 1, 117), "..."), res$value)
  }
  
  if (!is.numeric(n) || length(n) != 1L || n < 1L) n <- 50L
  utils::head(res, n = as.integer(n))
}

#' Complete a glossary path prefix
#'
#' @param prefix character(1) path prefix, e.g. "write/cantons/ab"
#' @param where optional top-level key restriction
#' @param n maximum number of results to return
#' @return character vector of matching paths
#' @export
glcomplete <- function(prefix, where = NULL, n = 50L) {
  if (missing(prefix) || !is.character(prefix) || length(prefix) != 1L) {
    stop("glcomplete(): 'prefix' must be a single character string.")
  }
  
  idx <- .gl_index_get()
  
  if (!is.null(where)) {
    if (!is.character(where) || length(where) != 1L || !nzchar(where)) {
      stop("glcomplete(): 'where' must be a single non-empty string or NULL.")
    }
    idx <- idx[idx$type == where, , drop = FALSE]
  }
  
  if (!nrow(idx)) return(character(0))
  
  keep <- startsWith(idx$path, prefix)
  paths <- idx$path[keep]
  
  if (!is.numeric(n) || length(n) != 1L || n < 1L) n <- 50L
  utils::head(paths, n = as.integer(n))
}
