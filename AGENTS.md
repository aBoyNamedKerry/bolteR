# bolteR — Agent Instructions

## Project Overview

**bolteR** is an R package for Warhammer 40K (10th edition) combat probability simulation. It calculates expected hits, wounds, and outcomes given dice counts, strength/toughness stats, and combat modifiers.

## Build & Test Commands

Run these via the R console or terminal with `Rscript -e "..."`:

```r
devtools::load_all()    # Reload package after code changes
devtools::document()    # Regenerate roxygen2 docs → man/*.Rd and NAMESPACE
devtools::test()        # Run all tests in tests/testthat/
devtools::check()       # Full R CMD CHECK (docs, tests, CRAN compliance)
```

## Architecture

Functions form a clear hierarchy — lower-level functions are composed into higher-level ones:

| Layer | Functions | Returns |
|-------|-----------|---------|
| Atomic probability | `wound_probability()`, `dice_checkr()` | Scalar (numeric) |
| Simulation | `dice_distro()` | List (results + summary stats) |
| Composed | `wound_checker()` | Scalar (expected wound count) |
| Output table | `wound_table()` | `data.table` comparing 5 strength profiles |

See [R/](R/) for source. Each file maps 1:1 to a function.

## Conventions

### Code Style
- **Snake_case** for all functions and arguments
- Input validation at function entry via `stop()`: `if(!is.numeric(x)) stop("...")`
- Defaults provided for optional arguments (e.g., `rerolls = FALSE`, `rolls = 1000`)
- Use `purrr::map()` / `purrr::map2()` for vectorised operations over strength profiles
- Use `data.table` idioms (`:=`, `setDT()`, `setcolorder()`) in `wound_table()`

### Documentation (roxygen2 + Markdown)
All exported functions must have:
```r
#' One-line description
#'
#' @param arg_name description of the argument
#' @return description of what is returned
#' @export
#' @examples
#' function_name(args)
```
Run `devtools::document()` after any roxygen2 changes.

### Imports
Declare all imports in [R/bolteR-package.R](R/bolteR-package.R) using `@importFrom`. Do not use `::` inline unless for one-off calls. Current imports: `data.table` and `purrr`.

## Testing

- Framework: `testthat` (edition 3)
- Test files: `tests/testthat/test-<function_name>.R`
- Pattern: compute a result, then assert on `length()` and `typeof()`/`class()`

```r
result <- my_function(args)
test_that("output as expected", {
  expect_equal(1, length(result))
  expect_type(result, "double")
})
```

- Add a test file for every new exported function
- Run `devtools::test()` to verify before committing

## Key Domain Rules (40K wound probability)

`wound_probability(strength, toughness)` returns one of these fixed values:

| Condition | Probability |
|-----------|------------|
| strength ≥ 2× toughness | 0.833 |
| strength > toughness | 0.667 |
| strength == toughness | 0.500 |
| strength < toughness | 0.333 |
| strength ≤ ½ toughness | 0.167 |

These map directly to 40K core rules and should not be changed without updating the rules reference.
