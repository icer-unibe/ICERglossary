.onLoad <- function(libname, pkgname) {
  .gl_init_state()
  try(glreset(lang = "DE", base_lang = "DE"), silent = TRUE)
}
