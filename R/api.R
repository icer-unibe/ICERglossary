#' Reset glossary to package defaults
#'
#' Builds the active glossary from JSON files shipped with the package.
#'
#' @param lang language code, e.g. "DE", "FR", "IT"
#' @param base_lang fallback language, default "DE"
#' @param json_dir optional directory to load JSON from (defaults to package inst/glossary)
#' @return invisibly, the rebuilt JSON list
#' @export
glreset <- function(lang = "DE", base_lang = "DE", json_dir = NULL) {
  if (!is.character(lang) || length(lang) != 1L) stop("glreset(): 'lang' must be a single character string.")
  if (!is.character(base_lang) || length(base_lang) != 1L) stop("glreset(): 'base_lang' must be a single character string.")
  
  .gl_init_state()
  
  json <- .gl_build_json(lang = lang, base_lang = base_lang, json_dir = json_dir)
  .gl_env$lang <- lang
  .gl_env$base_lang <- base_lang
  
  .gl_set_JSON(json)
  .gl_env$index <- NULL
  .gl_env$index_id <- NULL
  invisible(json)
}

#' Switch active glossary language
#'
#' @param lang language code, e.g. "DE", "FR", "IT"
#' @param base_lang fallback language, default "DE"
#' @return invisibly, the rebuilt JSON list
#' @export
gllang <- function(lang, base_lang = "DE") {
  if (missing(lang)) stop("gllang(): please provide 'lang', e.g. 'DE'.")
  glreset(lang = lang, base_lang = base_lang)
}

#' Overlay JSON from external file or directory
#'
#' @param path path to a .json file or a directory containing .json files
#' @return invisibly, the updated JSON list
#' @export
glread <- function(path) {
  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("glread(): 'path' must be a single string pointing to a file or directory.")
  }
  
  .gl_init_state()
  if (is.null(.gl_env$json)) {
    glreset(lang = .gl_env$lang %||% "DE", base_lang = .gl_env$base_lang %||% "DE")
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
  
  overlay <- list()
  for (p in files) {
    overlay <- .gl_merge_override(overlay, .gl_read_json(p))
  }
  
  merged <- .gl_merge_override(.gl_env$json, overlay)
  merged <- .gl_resolve_refs(merged, merged)
  
  .gl_set_JSON(merged)
  .gl_env$index <- NULL
  .gl_env$index_id <- NULL
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
