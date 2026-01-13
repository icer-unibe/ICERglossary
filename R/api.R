# internal: default verbosity (interactive by default)
.gl_verbose_default <- function(verbose) {
  if (!missing(verbose) && !is.null(verbose)) return(isTRUE(verbose))
  isTRUE(getOption("ICERglossary.verbose", interactive()))
}

# internal: compute added/changed leaves between two json objects
.gl_diff_leaves <- function(old_json, new_json) {
  old_idx <- .gl_index_build(old_json)
  new_idx <- .gl_index_build(new_json)
  
  if (!nrow(old_idx) && !nrow(new_idx)) {
    return(list(added = character(0), changed = character(0)))
  }
  
  old_map <- stats::setNames(old_idx$value, old_idx$path)
  new_map <- stats::setNames(new_idx$value, new_idx$path)
  
  added_paths <- setdiff(names(new_map), names(old_map))
  
  common <- intersect(names(new_map), names(old_map))
  changed_paths <- common[new_map[common] != old_map[common]]
  
  list(added = sort(added_paths), changed = sort(changed_paths))
}

#' Reset glossary to package defaults
#'
#' Builds the active glossary from JSON files shipped with the package.
#'
#' @param lang language code, e.g. "DE", "FR", "IT"
#' @param base_lang fallback language, default "DE"
#' @param json_dir optional directory to load JSON from (defaults to package inst/glossary)
#' @param verbose logical; if TRUE, print status messages
#' @return invisibly, the rebuilt JSON list
#' @export
glreset <- function(lang = "DE", base_lang = "DE", json_dir = NULL, verbose = NULL) {
  if (!is.character(lang) || length(lang) != 1L) stop("glreset(): 'lang' must be a single character string.")
  if (!is.character(base_lang) || length(base_lang) != 1L) stop("glreset(): 'base_lang' must be a single character string.")
  
  verbose <- .gl_verbose_default(verbose)
  
  .gl_init_state()
  
  json <- .gl_build_json(lang = lang, base_lang = base_lang, json_dir = json_dir)
  .gl_env$lang <- lang
  .gl_env$base_lang <- base_lang
  
  .gl_set_JSON(json)
  
  # invalidate search index cache
  .gl_env$index <- NULL
  .gl_env$index_id <- NULL
  
  if (isTRUE(verbose)) {
    msg_dir <- if (is.null(json_dir)) "<package default>" else normalizePath(json_dir, winslash = "/", mustWork = FALSE)
    message(sprintf("glreset(): active language='%s' (base='%s'); json_dir=%s", lang, base_lang, msg_dir))
  }
  
  invisible(json)
}

#' Switch active glossary language
#'
#' @param lang language code, e.g. "DE", "FR", "IT"
#' @param base_lang fallback language, default "DE"
#' @param verbose logical; if TRUE, print status messages
#' @return invisibly, the rebuilt JSON list
#' @export
gllang <- function(lang, base_lang = "DE", verbose = NULL) {
  if (missing(lang)) stop("gllang(): please provide 'lang', e.g. 'DE'.")
  glreset(lang = lang, base_lang = base_lang, verbose = verbose)
}

#' Overlay JSON from external file or directory
#'
#' @param path path to a .json file or a directory containing .json files
#' @param verbose logical; if TRUE, print status messages and changes
#' @param show maximum number of changed/added paths to print per category
#' @return invisibly, the updated JSON list
#' @export
glread <- function(path, verbose = NULL, show = 25L) {
  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("glread(): 'path' must be a single character string pointing to a file or directory.")
  }
  
  verbose <- .gl_verbose_default(verbose)
  
  .gl_init_state()
  if (is.null(.gl_env$json)) {
    glreset(lang = .gl_env$lang %||% "DE", base_lang = .gl_env$base_lang %||% "DE", verbose = FALSE)
  }
  
  files <- character(0)
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "\\.json$", full.names = TRUE, ignore.case = FALSE)
  } else if (file.exists(path)) {
    files <- path
  } else {
    stop("glread(): path not found: ", path)
  }
  if (!length(files)) stop("glread(): no .json files found at: ", path)
  
  old_json <- .gl_env$json
  
  overlay <- list()
  for (p in files) {
    overlay <- .gl_merge_override(overlay, .gl_read_json(p))
  }
  
  merged <- .gl_merge_override(old_json, overlay)
  merged <- .gl_resolve_refs(merged, merged)
  
  .gl_set_JSON(merged)
  
  # invalidate search index cache
  .gl_env$index <- NULL
  .gl_env$index_id <- NULL
  
  if (isTRUE(verbose)) {
    origin <- normalizePath(path, winslash = "/", mustWork = FALSE)
    message(sprintf("glread(): overlay applied from '%s' (%d file(s))", origin, length(files)))
    
    diff <- .gl_diff_leaves(old_json, merged)
    
    # sanitize show
    if (!is.numeric(show) || length(show) != 1L || show < 1L) show <- 25L
    show <- as.integer(show)
    
    if (length(diff$added) == 0L && length(diff$changed) == 0L) {
      message("glread(): no added/changed leaf values detected.")
    } else {
      if (length(diff$added)) {
        message(sprintf("glread(): added (%d):", length(diff$added)))
        to_print <- utils::head(diff$added, show)
        for (p in to_print) message("  + ", p, " = ", glget(p, default = "<missing>"))
        if (length(diff$added) > show) message("  ... (more omitted)")
      }
      
      if (length(diff$changed)) {
        message(sprintf("glread(): changed (%d):", length(diff$changed)))
        to_print <- utils::head(diff$changed, show)
        for (p in to_print) message("  * ", p, " = ", glget(p, default = "<missing>"))
        if (length(diff$changed) > show) message("  ... (more omitted)")
      }
    }
  }
  
  invisible(merged)
}

# internal helper: ensure glossary is initialised
.gl_ensure <- function() {
  .gl_init_state()
  if (is.null(.gl_env$json)) {
    glreset(lang = .gl_env$lang %||% "DE", base_lang = .gl_env$base_lang %||% "DE")
  }
  invisible(TRUE)
}

# internal helper: build a full path from top-level key + subpath
.gl_path <- function(top, key) {
  if (!is.character(top) || length(top) != 1L || !nzchar(top)) stop(".gl_path(): 'top' must be a non-empty string.")
  if (!is.character(key) || length(key) != 1L || !nzchar(key)) stop(".gl_path(): 'key' must be a non-empty string.")
  paste0(top, "/", key)
}

#' Get a value from the active glossary by path
#'
#' @param path slash-separated path, e.g. "label/BE_d"
#' @param default value returned when path is missing (default NULL)
#' @param error if TRUE, throw an error when path is missing
#' @return the resolved value (can be scalar, vector, or list)
#' @export
glget <- function(path, default = NULL, error = FALSE) {
  if (!is.character(path) || length(path) != 1L) stop("glget(): 'path' must be a single character string.")
  
  .gl_ensure()
  
  val <- .gl_resolve_path(path, .gl_env$json)
  if (is.null(val)) {
    if (isTRUE(error)) stop("glget(): path not found: ", path)
    return(default)
  }
  val
}

#' Return the active glossary JSON
#'
#' @return the active JSON list
#' @export
gljson <- function() {
  .gl_ensure()
  .gl_env$json
}

#' Get a label from the glossary
#'
#' Convenience wrapper for `glget("label/<key>")`.
#'
#' @param key label key or subpath, e.g. "BE_d" or "homelang/var"
#' @param default value returned when key is missing (default NULL)
#' @param error if TRUE, throw an error when key is missing
#' @return the resolved label value
#' @export
gllabel <- function(key, default = NULL, error = FALSE) {
  glget(.gl_path("label", key), default = default, error = error)
}

#' Format values using a glossary format string
#'
#' Convenience wrapper for formats stored under `format/<key>`.
#' Supports positional values (unnamed) and named values. Placeholder names in
#' the format string must use the pattern `%<name>`, which will be translated
#' to valid `sprintf()` syntax.
#'
#' @param key format key or subpath, e.g. "points"
#' @param ... values to insert; positional or named
#' @return formatted character string
#' @export
glformat <- function(key, ...) {
  fmt <- glget(.gl_path("format", key), error = TRUE)
  
  if (!is.character(fmt) || length(fmt) != 1L) {
    stop("glformat(): format must be a single character string.")
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
    stop("glformat(): too many unnamed values for format placeholders.")
  }
  
  nms[unnamed] <- ph[seq_along(unnamed)]
  names(args) <- nms
  
  do.call(sprintf, c(list(fmt2), args))
}

#' Format values using a glossary format string (compatibility alias)
#'
#' Prefer `glformat()` for new code.
#'
#' @param path format key or full path; if it does not start with "format/",
#'   it is treated as a format key under "format/<path>".
#' @param ... values to insert; positional or named
#' @return formatted character string
#' @export
glfmt <- function(path, ...) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("glfmt(): 'path' must be a non-empty string.")
  }
  
  if (grepl("^format/", path)) {
    key <- sub("^format/", "", path)
    return(glformat(key, ...))
  }
  
  glformat(path, ...)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
