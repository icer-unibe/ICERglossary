#' Search in the active glossary
#'
#' @param query search string
#' @param where search in "key", "value", or both (default)
#' @param top optional top-level key to restrict search, e.g. "stat"
#' @param ignore_case logical; case-insensitive search (default TRUE)
#' @param fixed logical; if TRUE, treat query as fixed string, else regex (default FALSE)
#' @param n optional maximum number of rows to return (default NULL = no limit)
#' @return data.frame with columns path, value, type, match_in
#' @export
glsearch <- function(
    query,
    where = c("key", "value"),
    top = NULL,
    ignore_case = TRUE,
    fixed = FALSE,
    n = NULL
) {
  if (missing(query) || !is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("glsearch(): 'query' must be a non-empty character scalar.")
  }
  
  where <- match.arg(where, several.ok = TRUE)
  
  .gl_init_state()
  if (is.null(.gl_env$json)) {
    glreset(lang = .gl_env$lang %||% "DE", base_lang = .gl_env$base_lang %||% "DE")
  }
  
  json <- .gl_env$json
  
  if (!is.null(top)) {
    if (!is.character(top) || length(top) != 1L || !nzchar(top)) {
      stop("glsearch(): 'top' must be a single non-empty string or NULL.")
    }
    json <- json[[top]]
    if (is.null(json) || !is.list(json)) {
      return(data.frame(path = character(), value = character(), type = character(), match_in = character()))
    }
  }
  
  flat <- .gl_flatten(json, prefix = if (is.null(top)) "" else paste0(top, "/"))
  
  # build matcher
  flags <- if (isTRUE(ignore_case)) "(?i)" else ""
  pat <- if (isTRUE(fixed)) paste0(flags, "\\Q", query, "\\E") else paste0(flags, query)
  
  in_key <- grepl(pat, flat$path, perl = TRUE)
  in_val <- grepl(pat, flat$value, perl = TRUE)
  
  keep <- rep(FALSE, nrow(flat))
  match_in <- rep("", nrow(flat))
  
  if ("key" %in% where) {
    keep <- keep | in_key
    match_in[in_key] <- ifelse(match_in[in_key] == "", "key", paste0(match_in[in_key], "+key"))
  }
  if ("value" %in% where) {
    keep <- keep | in_val
    match_in[in_val] <- ifelse(match_in[in_val] == "", "value", paste0(match_in[in_val], "+value"))
  }
  
  out <- flat[keep, , drop = FALSE]
  out$match_in <- match_in[keep]
  rownames(out) <- NULL
  
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) {
      stop("glsearch(): 'n' must be a single positive number or NULL.")
    }
    out <- utils::head(out, n = as.integer(n))
  }
  
  out
}

#' Interactively pick a glossary entry and return a ready-to-paste call
#'
#' @param query search query passed to glsearch()
#' @param top optional top-level restriction, e.g. "stat", "canton", "country"
#' @param where search in "key", "value", or both (default)
#' @param fixed logical; if TRUE, treat query as fixed string, else regex (default FALSE)
#' @param ignore_case logical; case-insensitive search (default TRUE)
#' @param n maximum number of candidates shown
#' @param clip logical; if TRUE, copy result to clipboard when possible
#' @return invisibly, a character(1) call string; NULL if cancelled
#' @export
glpick <- function(
    query,
    top = NULL,
    where = c("key", "value"),
    fixed = FALSE,
    ignore_case = TRUE,
    n = 20L,
    clip = FALSE
) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) n <- 20L
  
  hits <- glsearch(
    query = query,
    where = where,
    top = top,
    ignore_case = ignore_case,
    fixed = fixed,
    n = as.integer(n)
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
  if (startsWith(path, "canton/")) {
    key <- sub("^canton/", "", path)
    call <- paste0('glcant("', key, '")')
  } else if (startsWith(path, "country/")) {
    key <- sub("^country/", "", path)
    call <- paste0('glcnt("', key, '")')
  } else if (startsWith(path, "domain/")) {
    key <- sub("^domain/", "", path)
    call <- paste0('gldom("', key, '")')
  } else if (startsWith(path, "stat/")) {
    key <- sub("^stat/", "", path)
    call <- paste0('glstat("', key, '")')
  } else if (startsWith(path, "term/")) {
    key <- sub("^term/", "", path)
    call <- paste0('glterm("', key, '")')
  } else if (startsWith(path, "feature/")) {
    key <- sub("^feature/", "", path)
    call <- paste0('glfeat("', key, '")')
  } else if (startsWith(path, "format/")) {
    key <- sub("^format/", "", path)
    # template: users can add the value argument
    call <- paste0('glfmt("', key, '", )')
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

# internal: flatten a nested list into path/value rows
.gl_flatten <- function(x, prefix = "") {
  rows <- list()
  
  walk <- function(node, path) {
    if (is.list(node)) {
      if (length(node) == 0L) return()
      nms <- names(node)
      if (is.null(nms)) {
        for (i in seq_along(node)) walk(node[[i]], paste0(path, "/", i))
      } else {
        for (k in nms) walk(node[[k]], paste0(path, if (nzchar(path)) "/" else "", k))
      }
    } else {
      val <- if (is.null(node)) "" else node
      if (length(val) != 1L) {
        # collapse vectors to a single string for searching
        val <- paste(as.character(val), collapse = " | ")
      } else {
        val <- as.character(val)
      }
      rows[[length(rows) + 1L]] <<- list(path = path, value = val)
    }
  }
  
  base <- sub("/$", "", prefix)
  
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) {
      for (i in seq_along(x)) walk(x[[i]], paste0(base, if (nzchar(base)) "/" else "", i))
    } else {
      for (k in nms) walk(x[[k]], paste0(base, if (nzchar(base)) "/" else "", k))
    }
  }
  
  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  if (is.null(df)) {
    df <- data.frame(path = character(), value = character(), stringsAsFactors = FALSE)
  }
  
  df$type <- sub("/.*$", "", df$path)
  df
}
