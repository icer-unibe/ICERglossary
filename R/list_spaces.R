#' List Unicode space characters
#'
#' Prints a table of space characters with name, code point, R escape, HTML
#' entity and the literal character between quotes, ready to be copied into
#' .qmd files.
#'
#' @param copy Optional key (e.g. "nnbsp"); copies that character to the
#'   clipboard instead of printing the table.
#' @return Invisibly, the data frame (or the selected character).
#' @export
#'
#' @examples
#' listspaces()
#' listspaces(copy = "nnbsp")
listspaces <- function(copy = NULL) {
  
  tbl <- space_table()
  
  if (!is.null(copy)) {
    i <- match(copy, tbl$key)
    if (is.na(i)) {
      stop("Unknown key \"", copy, "\". Use one of: ",
           paste(tbl$key, collapse = ", "), call. = FALSE)
    }
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      clipr::write_clip(tbl$char[i], allow_non_interactive = TRUE)
      message(tbl$name[i], " (", tbl$code[i], ") copied to clipboard.")
    } else {
      message("Package 'clipr' not available - character returned instead.")
    }
    return(invisible(tbl$char[i]))
  }
  
  w <- max(nchar(tbl$name))
  # cat() writes raw bytes; print() would escape the characters
  cat(sprintf("%-9s  %-*s  %-8s  %-8s  %-9s  %s\n",
              "key", w, "name", "unicode", "R", "entity", "char"))
  for (i in seq_len(nrow(tbl))) {
    cat(sprintf('%-9s  %-*s  %-8s  %-8s  %-9s  "%s"\n',
                tbl$key[i], w, tbl$name[i], tbl$code[i],
                tbl$escape[i], tbl$entity[i], tbl$char[i]))
  }
  
  # Footer: copying invisible characters by hand is error-prone, so point the
  # user to the clipboard helper and the entity alternative
  cat("\n")
  cat('Copy between the quotes, or use the clipboard helper:\n')
  cat('  listspaces(copy = "nnbsp")   ->   pastes ', tbl$code[3],
      ' wherever you need it\n', sep = "")
  cat('Alternatively paste the entity into the .qmd source, e.g. 1',
      tbl$entity[3], '000 (stays visible in the source after saving)\n', sep = "")
  cat('  checkspace("<paste>")        ->   identifies a character you already have\n')
  
  invisible(tbl)
}