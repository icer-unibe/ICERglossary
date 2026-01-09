.gl_env <- new.env(parent = emptyenv())

.gl_init_state <- function() {
  if (is.null(.gl_env$lang)) .gl_env$lang <- "DE"
  if (is.null(.gl_env$base_lang)) .gl_env$base_lang <- "DE"
  if (is.null(.gl_env$json)) .gl_env$json <- NULL
}

.gl_set_JSON <- function(json) {
  ns <- asNamespace("ICERglossary")
  
  if (bindingIsLocked("JSON", ns)) {
    unlockBinding("JSON", ns)
    on.exit(lockBinding("JSON", ns), add = TRUE)
  }
  
  assign("JSON", json, envir = ns)
  .gl_env$json <- json
  invisible(json)
}
