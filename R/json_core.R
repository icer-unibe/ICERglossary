.gl_json_dir <- function(json_dir = NULL) {
  if (!is.null(json_dir)) return(json_dir)
  system.file("glossary", package = "ICERglossary")
}

.gl_find_lang_files <- function(dir, lang) {
  pattern <- paste0("^", lang, ".*\\.json$")
  list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = FALSE)
}

.gl_read_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = TRUE)
}

.gl_merge_override <- function(current, source) {
  out <- current
  for (key in names(source)) {
    if (!key %in% names(out)) {
      out[[key]] <- source[[key]]
    } else if (is.list(out[[key]]) && is.list(source[[key]])) {
      out[[key]] <- .gl_merge_override(out[[key]], source[[key]])
    } else {
      out[[key]] <- source[[key]]
    }
  }
  out
}

.gl_resolve_path <- function(path, data) {
  keys <- strsplit(path, "/", fixed = TRUE)[[1]]
  for (k in keys) {
    if (!is.list(data) || is.null(data[[k]])) return(NULL)
    data <- data[[k]]
  }
  data
}

.gl_resolve_string_refs <- function(x, root, max_iter = 50L) {
  # resolves {{path}} references in strings; iter guard only for repeated expansions
  if (!is.character(x) || length(x) != 1L) return(x)
  
  iter <- 0L
  repeat {
    iter <- iter + 1L
    if (iter > max_iter) {
      stop("reference resolution exceeded max_iter (possible cycle): ", x)
    }
    
    # full-string deep reference: {{path/to/value}}
    if (grepl("^\\{\\{.*\\}\\}$", x)) {
      path <- sub("^\\{\\{(.*)\\}\\}$", "\\1", x)
      val <- .gl_resolve_path(path, root)
      if (is.null(val)) return(x)
      
      # if resolved is not a scalar string, return as-is (can be list/number/vector)
      if (!is.character(val) || length(val) != 1L) return(val)
      
      # continue in case resolved string contains further refs
      x <- val
      next
    }
    
    # inline references inside a string
    m <- gregexpr("\\{\\{(.*?)\\}\\}", x, perl = TRUE)
    refs <- regmatches(x, m)[[1]]
    if (!length(refs)) return(x)
    
    paths <- sub("^\\{\\{(.*?)\\}\\}$", "\\1", refs)
    replacements <- vapply(paths, function(p) {
      val <- .gl_resolve_path(p, root)
      if (is.null(val)) return(paste0("{{", p, "}}"))
      # inline replacement must be a string
      if (!is.atomic(val) || length(val) != 1L) return(as.character(val)[1])
      as.character(val)
    }, FUN.VALUE = character(1))
    
    x_new <- x
    for (i in seq_along(refs)) {
      x_new <- sub(refs[i], replacements[i], x_new, fixed = TRUE)
    }
    
    # if no change, stop to avoid infinite loop
    if (identical(x_new, x)) return(x)
    x <- x_new
  }
}

.gl_resolve_refs <- function(node, root, max_iter = 50L) {
  # recursively resolve refs in a json-like object
  if (is.list(node)) {
    keys <- if (is.null(names(node))) seq_along(node) else names(node)
    for (k in keys) {
      node[[k]] <- .gl_resolve_refs(node[[k]], root, max_iter = max_iter)
    }
    return(node)
  }
  
  if (is.character(node)) {
    out <- vector("list", length(node))
    for (i in seq_along(node)) {
      resolved <- .gl_resolve_string_refs(node[[i]], root, max_iter = max_iter)
      
      # if a deep ref resolves to a list, resolve refs inside that list too
      if (is.list(resolved)) {
        out[[i]] <- .gl_resolve_refs(resolved, root, max_iter = max_iter)
      } else {
        out[[i]] <- resolved
      }
    }
    
    # keep as character vector when possible
    if (all(vapply(out, is.character, logical(1))) &&
        all(vapply(out, function(z) length(z) == 1L, logical(1)))) {
      return(unlist(out, use.names = FALSE))
    }
    return(out)
  }
  
  node
}


.gl_build_json <- function(lang, base_lang, json_dir = NULL) {
  dir <- .gl_json_dir(json_dir)
  if (!nzchar(dir) || !dir.exists(dir)) stop("JSON directory not found: ", dir)
  
  # load order (overlay wins):
  # base_lang files first (if different), then lang files
  paths <- character(0)
  
  if (!identical(toupper(lang), toupper(base_lang))) {
    paths <- c(paths, .gl_find_lang_files(dir, base_lang))
  }
  paths <- c(paths, .gl_find_lang_files(dir, lang))
  
  if (!length(paths)) {
    stop("No JSON files found for lang='", lang, "' (base='", base_lang, "') in ", dir)
  }
  
  json <- list()
  for (p in paths) {
    json <- .gl_merge_override(json, .gl_read_json(p))
  }
  
  # resolve refs after full merge
  json <- .gl_resolve_refs(json, json)
  json
}
