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

#' Search across DE/FR/IT and return a paste-ready glget() call
#'
#' Searches in keys and values across all entries (excluding top-level keys).
#' Prints a numbered list with columns: de | fr | it.
#' The selected item returns (and prints) a ready-to-paste call like glget("term/foo").
#'
#' @param query search string
#' @param ignore_case logical; case-insensitive search (default TRUE)
#' @param fixed logical; if TRUE, treat query as fixed substring, else regex (default TRUE)
#' @param n maximum number of candidates shown (default 30)
#' @param base_lang fallback language used when building non-DE json (default: current base_lang or "DE")
#' @param clip logical; if TRUE, copy result to clipboard when possible
#' @return invisibly, a character(1) call string; NULL if cancelled or no matches
#' @export
gls <- function(
    query,
    ignore_case = TRUE,
    fixed = TRUE,
    n = 30L,
    base_lang = NULL,
    clip = FALSE
) {
  if (missing(query) || !is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("gls(): 'query' must be a non-empty character scalar.")
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) n <- 30L
  n <- as.integer(n)
  
  .gl_init_state()
  
  # use current base_lang if available
  if (is.null(base_lang)) base_lang <- .gl_env$base_lang %||% "DE"
  if (!is.character(base_lang) || length(base_lang) != 1L || !nzchar(base_lang)) {
    base_lang <- "DE"
  }
  
  # build three language jsons without touching the global active json
  json_de <- .gl_build_json(lang = "DE", base_lang = "DE", json_dir = NULL)
  json_fr <- .gl_build_json(lang = "FR", base_lang = base_lang, json_dir = NULL)
  json_it <- .gl_build_json(lang = "IT", base_lang = base_lang, json_dir = NULL)
  
  # flatten leaves
  flat_de <- .gl_flatten(json_de, prefix = "")
  flat_fr <- .gl_flatten(json_fr, prefix = "")
  flat_it <- .gl_flatten(json_it, prefix = "")
  
  # align by path (union)
  paths <- sort(unique(c(flat_de$path, flat_fr$path, flat_it$path)))
  
  get_val <- function(flat, p) {
    i <- match(p, flat$path)
    if (is.na(i)) return("")
    flat$value[[i]]
  }
  
  out <- data.frame(
    path = paths,
    de = vapply(paths, function(p) get_val(flat_de, p), character(1)),
    fr = vapply(paths, function(p) get_val(flat_fr, p), character(1)),
    it = vapply(paths, function(p) get_val(flat_it, p), character(1)),
    stringsAsFactors = FALSE
  )
  
  # search in key without top-level + in any value
  key_no_top <- sub("^[^/]+/", "", out$path)
  
  # build matcher
  flags <- if (isTRUE(ignore_case)) "(?i)" else ""
  pat <- if (isTRUE(fixed)) paste0(flags, "\\Q", query, "\\E") else paste0(flags, query)
  
  in_key <- grepl(pat, key_no_top, perl = TRUE)
  in_val <- grepl(pat, out$de, perl = TRUE) | grepl(pat, out$fr, perl = TRUE) | grepl(pat, out$it, perl = TRUE)
  
  hits <- out[in_key | in_val, , drop = FALSE]
  if (!nrow(hits)) {
    message("gls(): no matches.")
    return(NULL)
  }
  
  # limit rows
  if (nrow(hits) > n) hits <- hits[seq_len(n), , drop = FALSE]
  
  # print 3-column list (de | fr | it)
  # note: keep output simple and stable in the console
  one_line <- function(x) {
    # note: keep console output single-line
    x <- gsub("[\r\n]+", " ", x)
    x <- gsub("\\s{2,}", " ", x)
    trimws(x)
  }
  
  lines <- vapply(
    seq_len(nrow(hits)),
    function(i) paste0(one_line(hits$de[[i]]), " | ", one_line(hits$fr[[i]]), " | ", one_line(hits$it[[i]])),
    character(1)
  )
  
  sel <- utils::menu(lines, title = "Select entry (0 = cancel)")
  if (sel <= 0L) return(NULL)
  
  path <- hits$path[[sel]]
  call <- paste0('glget("', path, '")')
  
  if (isTRUE(clip)) {
    copied <- FALSE
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(call)
      copied <- TRUE
    } else if (.Platform$OS.type == "windows") {
      utils::writeClipboard(call)
      copied <- TRUE
    }
    if (copied) message("gls(): copied to clipboard.")
  }
  
  cat(call, "\n")
  invisible(call)
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

#' Quarto inline wrapper for gls()
#'
#' Runs gls() and wraps the selected glget() call for Quarto inline R:
#' `r {{glget("...")}}`
#'
#' @param query search string forwarded to gls()
#' @param ignore_case logical; forwarded to gls()
#' @param fixed logical; forwarded to gls()
#' @param n integer; forwarded to gls()
#' @param base_lang character(1); forwarded to gls()
#' @param clip logical; if TRUE, copy wrapped string to clipboard when possible
#' @return invisibly, a character(1) inline string; NULL if cancelled/no match
#' @export
glsq <- function(
    query,
    ignore_case = TRUE,
    fixed = TRUE,
    n = 30L,
    base_lang = NULL,
    clip = TRUE
) {
  call <- gls(
    query = query,
    ignore_case = ignore_case,
    fixed = fixed,
    n = n,
    base_lang = base_lang,
    clip = FALSE
  )
  
  if (is.null(call)) return(NULL)
  
  inline <- paste0("`r {{", call, "}}`")
  
  if (isTRUE(clip)) {
    copied <- FALSE
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(inline)
      copied <- TRUE
    } else if (.Platform$OS.type == "windows") {
      utils::writeClipboard(inline)
      copied <- TRUE
    }
    if (copied) message("glsq(): copied to clipboard.")
  }
  
  cat(inline, "\n")
  invisible(inline)
}
