# internal: build a flat index of the active glossary
# returns a data.frame with columns: path, value, type
.gl_index_build <- function(json) {
  if (is.null(json) || !is.list(json)) {
    return(data.frame(path = character(0), value = character(0), type = character(0), stringsAsFactors = FALSE))
  }
  
  out_path <- character(0)
  out_val  <- character(0)
  out_type <- character(0)
  
  add_leaf <- function(path, value, type) {
    out_path <<- c(out_path, path)
    out_val  <<- c(out_val, value)
    out_type <<- c(out_type, type)
    invisible(NULL)
  }
  
  walk <- function(node, prefix, type) {
    if (is.list(node)) {
      nms <- names(node)
      if (is.null(nms)) {
        # unnamed lists are treated as leaves (collapsed)
        add_leaf(prefix, "<list>", type)
        return(invisible(NULL))
      }
      
      for (k in nms) {
        next_prefix <- if (nzchar(prefix)) paste0(prefix, "/", k) else k
        walk(node[[k]], next_prefix, type)
      }
      return(invisible(NULL))
    }
    
    if (is.atomic(node)) {
      # keep only scalar-ish values for preview; collapse vectors
      if (length(node) == 0L) {
        add_leaf(prefix, "", type)
      } else if (length(node) == 1L) {
        add_leaf(prefix, as.character(node), type)
      } else {
        add_leaf(prefix, paste(as.character(node), collapse = ", "), type)
      }
      return(invisible(NULL))
    }
    
    # fallback for unknown types
    add_leaf(prefix, "<value>", type)
    invisible(NULL)
  }
  
  top_keys <- names(json)
  if (is.null(top_keys)) {
    return(data.frame(path = character(0), value = character(0), type = character(0), stringsAsFactors = FALSE))
  }
  
  for (tk in top_keys) {
    node <- json[[tk]]
    walk(node, tk, tk)
  }
  
  data.frame(
    path = out_path,
    value = out_val,
    type = out_type,
    stringsAsFactors = FALSE
  )
}

# internal: get cached index, rebuild if missing or stale
.gl_index_get <- function() {
  .gl_ensure()
  
  # cache key based on identity of the current json object
  json <- .gl_env$json
  obj_id <- paste0("id_", sprintf("%x", as.integer(utils::object.size(json))))
  
  if (!is.null(.gl_env$index) && identical(.gl_env$index_id, obj_id)) {
    return(.gl_env$index)
  }
  
  idx <- .gl_index_build(json)
  .gl_env$index <- idx
  .gl_env$index_id <- obj_id
  idx
}
