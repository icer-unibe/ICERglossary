
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICERglossary - TEST

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
following (the corresponding functions are explained in the next
section):

- `canton/*` – Swiss cantons (keys represent the abbreviations used so
  far)
- `country/*` – PISA countries (keys represent country codes)
- `domain/*` – Various terms used in connection with tested domains
- `feature/*` – Various terms, value labels etc. used in connection with
  student characteristics
- `stats/*` – Statistical terms
- `term/*` – General terms
- `tech/*` – Mainly formatting and grammar
- `old/*` – Old definitions with top-level keys *write* and *draft*

At load time, all relevant JSON files are merged into a single active
glossary object.

## Quick start

### Retrieve labels and switch language

``` r
options(ICERglossary.verbose = TRUE)

# get canton BE
glcant("BE_d")
#> [1] "Bern (deutschsprachiger Teil)"

# get country France
glcnt("FRA")
#> [1] "Frankreich"

# switch language to French
gllang("FR")
#> glreset(): active language='FR' (base='DE'); json_dir=<package default>

# get domain reading (school language)
gldom("sl")
#> [1] "Compréhension écrite dans la langue de scolarisation"

# get feature expression by migration status
glfeat("byMigrationStatus")
#> [1] "selon le statut migratoire"

# switch language to Italian
gllang("IT")
#> glreset(): active language='IT' (base='DE'); json_dir=<package default>

# get term for sample size
glstat("sample_size")
#> [1] "Dimensione del campione"

# get formatting code for points
glformat("points")
#> [1] "%<points>.0f punti"

glreset()
#> glreset(): active language='DE' (base='DE'); json_dir=<package default>
```

`base_lang` defaults to `"DE"` and acts as a fallback if a key is
missing in the selected language.

### Format numeric values

``` r
glfmt("points", 125)
#> [1] "125 Punkte"

glfmt("percentage", 62)
#> [1] "62 Prozent"

gllang("FR")
#> glreset(): active language='FR' (base='DE'); json_dir=<package default>

glfmt("percentage", 62)
#> [1] "62%"

glreset()
#> glreset(): active language='DE' (base='DE'); json_dir=<package default>
```

## Searching and discovery

### Search glossary entries

``` r
glsearch("sample")
#>                       path                             value type match_in
#> 1         pisa/pisa_sample                   PISA-Stichprobe pisa      key
#> 2              stat/sample                        Stichprobe stat      key
#> 3         stat/sample_size                 Stichprobengrösse stat      key
#> 4     stat/ugk_sample_size             ÜGK-Populationsumfang stat      key
#> 5  stat/comparable_samples         vergleichbare Stichproben stat      key
#> 6         stat/samples_alt                       Stichproben stat      key
#> 7         stat/rand_sample                 Zufallsstichprobe stat      key
#> 8  stat/cant_extra_samples       kantonale Zusatzstichproben stat      key
#> 9         stat/sample_prep      Vorbereitung der Stichproben stat      key
#> 10         stat/age_sample                  Altersstichprobe stat      key
#> 11    stat/rand_sch_sample zufällig gezogene Schulstichprobe stat      key
#> 12         term/sch_sample                   Schulstichprobe term      key
```

### Interactive picker

``` r
glpick("sample")
```

The picker shows a numbered list of matches and prints a ready-to-paste
function call.

### Quarto picker helper

``` r
qtpick("sample")
```

Returns function call that can be copied and pasted into Quarto.
Example: Pick number 2 returns a quarto snippet for sampling size.

``` r
`r {{glstat("sample_size")}}`
```

## Overriding glossary content

Project-specific adjustments can be applied via external JSON files.

### Apply an overlay

``` r
"glread("path/to/custom/json")"
```

All `.json` files in the directory are read and merged; existing keys
are overwritten, new keys are added.

### Save glossary

``` r
"glwrite("path/to/saved/json")"
```

Saves the currently active glossary as one JSON file. Make one export
per language!

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

## File layout

- `R/` – package code (API, search helpers, state management)
- `inst/extdata/json/` – built-in glossary JSON files
- `man/` – documentation

## To do

- [x] Core glossary loading and language switching
- [x] Format and symbol resolution
- [x] Interactive search and picker helpers
- [x] Review and stabilize top-level key conventions
- [ ] Align downstream packages (`ICERreport`, `ICERplot`)
- [x] Think about options to freeze glossary for finished projects

``` r
packageVersion("ICERglossary")
#> [1] '0.0.1'
```

## License

Internal use only (University of Bern / ICER).
