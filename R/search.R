#' Search keys and values in the active glossary
#'
#' @param query character(1). Regular expression (default) or plain substring.
#' @param where optional top-level key to restrict search, e.g. "label", "format", "write".
#' @param scope search in "path", "value", or "both".
#' @param regex logical. If FALSE, treat query as fixed substring.
#' @param ignore_case logical. If TRUE, match case-insensitively.
#' @param n maximum number of results to return.
#' @return data.frame with columns: path, value, type
#' @export
glsearch <- function(query, where = NULL, scope = c("value", "both", "path"), regex = TRUE, ignore_case = TRUE, n = 50L) {
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

#' Interactively pick a glossary entry and return a ready-to-paste call
#'
#' The function searches the active glossary using `glsearch()`, shows a
#' numbered menu, and returns a paste-ready function call string.
#'
#' @param query search query passed to glsearch()
#' @param where optional top-level restriction, e.g. "label", "format", "write"
#' @param scope search in "path", "value", or "both"
#' @param regex logical, passed to glsearch()
#' @param ignore_case logical, passed to glsearch()
#' @param n maximum number of candidates shown
#' @param clip logical; if TRUE, copy result to clipboard when possible
#' @return invisibly, a character(1) call string; NULL if cancelled
#' @export
glpick <- function(
    query,
    where = NULL,
    scope = c("value", "both", "path"),
    regex = TRUE,
    ignore_case = TRUE,
    n = 20L,
    clip = FALSE
) {
  scope <- match.arg(scope)
  
  hits <- glsearch(
    query,
    where = where,
    scope = scope,
    regex = regex,
    ignore_case = ignore_case,
    n = n
  )
  
  if (!nrow(hits)) {
    message("glpick(): no matches.")
    return(NULL)
  }
  
  choices <- paste0(hits$path, "  |  ", hits$value)
  sel <- utils::menu(choices, title = "Select a glossary entry (0 = cancel)")
  if (sel <= 0L) return(NULL)
  
  path <- hits$path[[sel]]
  
  # build paste-ready call based on top-level key
  if (startsWith(path, "label/")) {
    key <- sub("^label/", "", path)
    call <- paste0('gllabel("', key, '")')
  } else if (startsWith(path, "format/")) {
    key <- sub("^format/", "", path)
    call <- paste0('glformat("', key, '", )')
  } else {
    call <- paste0('glget("', path, '")')
  }
  
  # optionally copy to clipboard
  if (isTRUE(clip)) {
    copied <- FALSE
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(call)
      copied <- TRUE
    } else if (.Platform$OS.type == "windows") {
      utils::writeClipboard(call)
      copied <- TRUE
    }
    if (copied) message("glpick(): copied to clipboard.")
  }
  
  # print without escaping for easy copy/paste
  cat(call, "\n")
  
  invisible(call)
}

#' Pick a glossary entry and return a Quarto inline snippet
#'
#' Wrapper around glpick() that formats the result for Quarto inline use.
#'
#' @param ... arguments passed to glpick()
#' @param clip logical; if TRUE, copy the result to clipboard when possible
#' @return invisibly, a character(1) Quarto inline snippet; NULL if cancelled
#' @export
qtpick <- function(..., clip = FALSE) {
  call <- glpick(..., clip = FALSE)
  if (is.null(call)) return(NULL)
  
  qt <- paste0("`r {{", call, "}}`")
  
  if (isTRUE(clip)) {
    copied <- FALSE
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(qt)
      copied <- TRUE
    } else if (.Platform$OS.type == "windows") {
      utils::writeClipboard(qt)
      copied <- TRUE
    }
    if (copied) message("qtpick(): copied to clipboard.")
  }
  
  cat(qt, "\n")
  invisible(qt)
}


