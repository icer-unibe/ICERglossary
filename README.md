
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICERglossary

<!-- badges: start -->

<!-- badges: end -->

`ICERglossary` provides a centralized, JSON-based glossary for
multilingual (DE, FR, IT) Large Scale Assessment (LSA) reporting
workflows.  
It is designed as a shared backend for Quarto-based reports and R
packages such as `ICERreport` and `ICERplot`, but can also be used
standalone as a lookup and formatting tool. The glossary is intended to
be continuously extended and reused across different large-scale
assessments (LSA), allowing new studies and reporting contexts to build
on a shared, evolving set of definitions and conventions.

## Installation

``` r
devtools::install_git(
  "https://gitlab.switch.ch/unibe-icer/icer-r-packages/icerglossary.git"
)
```

## Key ideas

- **Package-based structure and versioning** Maintaining the glossary as
  an R package enforces a clear structure, proper documentation, and
  explicit versioning, supporting stable and reproducible reporting
  workflows.

- **Single source of truth**  
  All central terms, labels, symbols, units, and formatting rules are
  defined once in JSON and reused consistently across reports and
  packages, ensuring uniform wording and reducing the risk of errors.

- **Language-aware formatting**  
  Output automatically adapts to DE / FR / IT conventions (wording,
  spacing, symbols).

- **Collaborative development** Project contributors can propose new
  definitions, labels, or conventions via merge requests, enabling
  shared ownership and gradual refinement of the glossary.

- **Quarto-first, but not Quarto-only**  
  Designed for Quarto inline usage, but fully usable interactively or
  from other R packages.

- **Override-friendly**  
  Project-specific JSON files can temporarily override built-in
  definitions without modifying the package.

## JSON structure (conceptual)

The glossary is currently organized based on top-level keys such as the
following (A more detailed breakdown should be reviewed):

- `label/*` – human-readable names (regions, domains, variables, …)
- `format/*` – format strings for numeric output
- `symbol/*` – language-specific units and typographic elements
- `write/*` – longer text templates for narrative reporting

At load time, all relevant JSON files are merged into a single active
glossary object.

## Quick start

### Retrieve labels and switch language

Use `glget()` and enter the path to retrieve value:

``` r
options(ICERglossary.verbose = TRUE)
glget("label/BE_d")
#> [1] "Bern (deutschsprachiger Teil)"

glget("label/byMigrationStatus")
#> [1] "nach Migrationshintergrund"

gllang("FR")
#> glreset(): active language='FR' (base='DE'); json_dir=<package default>

glget("label/BE_d")
#> [1] "Berne (partie germanophone)"

gllang("IT")
#> glreset(): active language='IT' (base='DE'); json_dir=<package default>

glget("label/byMigrationStatus")
#> [1] "secondo lo statuto migratorio"

glreset()
#> glreset(): active language='DE' (base='DE'); json_dir=<package default>
```

`base_lang` defaults to `"DE"` and acts as a fallback if a key is
missing in the selected language.

### Format numeric values

``` r
glformat("points", 125)
#> [1] "125 Punkte"

glformat("percentage", 62)
#> [1] "62 Prozent"

gllang("FR")
#> glreset(): active language='FR' (base='DE'); json_dir=<package default>

glformat("percentage", 62)
#> [1] "62%"

glreset()
#> glreset(): active language='DE' (base='DE'); json_dir=<package default>
```

## Searching and discovery

### Search glossary entries

``` r
glsearch("bern")
#>         path                             value  type
#> 2 label/BE_d     Bern (deutschsprachiger Teil) label
#> 3 label/BE_f Bern (französischsprachiger Teil) label

glsearch("points", where = "format")
#>              path               value   type
#> 170 format/points %<points>.0f Punkte format
```

### Complete paths by prefix

``` r
glcomplete("label/homelang")
#>  [1] "label/homelang/var"           "label/homelang/level/1"      
#>  [3] "label/homelang/level/2"       "label/homelang/level/3"      
#>  [5] "label/homelang2/var"          "label/homelang2/level/1"     
#>  [7] "label/homelang2/level/2"      "label/homelang2_long/var"    
#>  [9] "label/homelang2_long/level/1" "label/homelang2_long/level/2"

glget("label/homelang2_long/level/2")
#> [1] "keine Schulsprache zu Hause"
```

### Interactive picker

``` r
glpick("bern", where = "label")
```

The picker shows a numbered list of matches and prints a ready-to-paste
function call.

### Quarto picker helper

``` r
qtpick("bern", where = "label")
```

Returns function call that can be copied and pasted into Quarto:

``` r
`r {{gllabel("BE_d")}}`
```

## Overriding glossary content

Project-specific adjustments can be applied via external JSON files.

### Apply an overlay

``` r
"glread("path/to/custom/json")"
```

All `.json` files in the directory are read and merged; existing keys
are overwritten, new keys are added.

### Reset to package defaults

``` r
glreset()
```

### Verbose mode

Messages can be enabled or disabled globally:

``` r
options(ICERglossary.verbose = TRUE)
```

or per call:

``` r
glread("path/to/custom/json", verbose = TRUE)
```

## Typical usage patterns

- Interactive exploration of labels and formats in the R console
- Inline text generation in Quarto documents
- Shared glossary backend for reporting and plotting packages
- Project-specific overrides without forking or editing the package

## File layout

- `R/` – package code (API, search helpers, state management)
- `inst/extdata/json/` – built-in glossary JSON files
  - `DE.json`, `FR.json`, `IT.json`
  - `*_tech.json` (formats, symbols)
- `man/` – documentation

## To do

- [x] Core glossary loading and language switching
- [x] Format and symbol resolution
- [x] Interactive search and picker helpers
- [ ] Review and stabilize top-level key conventions
- [ ] Align downstream packages (`ICERreport`, `ICERplot`)
- [ ] Optional pkgdown site

``` r
packageVersion("ICERglossary")
#> [1] '0.0.0.9000'
```

## License

Internal use only (University of Bern / ICER).
