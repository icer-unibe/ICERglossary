# Internal: reference table of space characters
space_table <- function() {
  tbl <- data.frame(
    key  = c("space", "nbsp", "nnbsp", "thinsp", "hairsp", "ensp", "emsp",
             "figuresp", "punctsp", "mmsp", "zwsp"),
    name = c("Space", "No-break space", "Narrow no-break space", "Thin space",
             "Hair space", "En space", "Em space", "Figure space",
             "Punctuation space", "Medium mathematical space",
             "Zero width space"),
    char = c("\u0020", "\u00a0", "\u202f", "\u2009", "\u200a", "\u2002",
             "\u2003", "\u2007", "\u2008", "\u205f", "\u200b"),
    stringsAsFactors = FALSE
  )
  tbl$cp     <- vapply(tbl$char, utf8ToInt, integer(1), USE.NAMES = FALSE)
  tbl$code   <- sprintf("U+%04X", tbl$cp)
  tbl$escape <- sprintf("\\u%04x", tbl$cp)
  tbl$entity <- sprintf("&#%d;", tbl$cp)
  tbl
}