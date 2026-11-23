# json5

Read and write [JSON5](https://spec.json5.org/) in R, keeping comments.

JSON5 extends JSON with features meant for hand-written files such as configuration files. It adds comments, trailing commas, unquoted keys, single-quoted and multi-line strings, hexadecimal numbers, and `Infinity`/`NaN`. Every valid JSON document is also valid JSON5.

`json5` is a lightweight, pure-R package with no compiled code:

- It validates text against the JSON5 1.0.0 specification. It passes the official [json5-tests](https://github.com/json5/json5-tests) conformance suite, and errors report the line and column.
- It converts the text to JSON and lets **jsonlite** map it to R objects, so you get the familiar `jsonlite::fromJSON()` results: vectors, data frames, and matrices.
- It keeps comments by attaching them to the R objects with R's native `comment()` attribute, and writes them back out.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/json5")
```

## Usage

```r
txt <- "// Server configuration
{
  host: 'localhost',
  port: 8080,          // default port
  timeout: .5,
  mask: 0xFF,
  retries: [1, 2, Infinity,],
}"

cfg <- json5::parse_json5(txt)   # or json5::read_json5("config.json5")
str(cfg)
#> List of 5
#>  $ host   : chr "localhost"
#>  $ port   : int 8080
#>   ..- attr(*, "comment")= chr "default port"
#>  $ timeout: num 0.5
#>  $ mask   : int 255
#>  $ retries: num [1:3] 1 2 Inf
#>  - attr(*, "comment")= chr "Server configuration"

cfg$port <- 9090L                # replacing a value also drops its comment
comment(cfg$host) <- "change in production"
json5::to_json5(cfg)             # or json5::write_json5(cfg, "config.json5")
#> // Server configuration
#> {
#>   // change in production
#>   host: "localhost",
#>   port: 9090,
#>   timeout: 0.5,
#>   mask: 255,
#>   retries: [1, 2, Infinity],
#> }
```

Other functions:

| Function | Purpose |
|---|---|
| `json5_to_json()` | Convert JSON5 text to strict JSON, for other tools |
| `validate_json5()` | `TRUE`/`FALSE`, with the error message in `attr(, "err")` |

Writer options include `pretty`, `indent`, `quote = "double" | "single" | "auto"`, `quote_keys`, `trailing_comma` and `comments`. Other arguments, such as `dataframe`, `matrix` and `digits`, are passed to `jsonlite::toJSON()`.

### How comments are kept

- A comment on its own line(s) belongs to the element that follows it. A comment after a value on the same line belongs to that value.
- Comments before or after the top-level value belong to the top-level object.
- Some JSON5 values have no R object that can hold an attribute: `null`, and the elements of arrays that jsonlite simplified into vectors, matrices or data frames. Their comments are stored on the enclosing object in a `"json5_comments"` attribute, keyed by JSON Pointer (for example `"/1"`). Use `simplifyVector = FALSE` to keep every element as its own R object.
- When writing, comments are always written before their element: `// text`, or `/* ... */` for text with line breaks. A trailing comment therefore moves to the line above.
- Like any R attribute, a comment is lost when you replace the whole value, subset a vector (`x[1:2]`), or use `c()`/`unlist()`. Changing elements in place (`x$v[2] <- 1`) keeps it.

### Limitations

- R strings cannot contain NUL, so `\0`, `\x00` and `\u0000` raise an error.
- Duplicate keys are all kept, as in jsonlite. The JSON5 spec leaves this to implementations.
- Numbers are written with 15 significant digits by default. Use `digits = I(17)` for an exact round trip of every double.
- The parser is written in R and aimed at configuration-sized files, not large data.

## Acknowledgements

### jsonlite

This package is built on top of the excellent
[**jsonlite**](https://cran.r-project.org/package=jsonlite) package by
**Jeroen Ooms** ([GitHub](https://github.com/jeroen/jsonlite)), released under the MIT license. Every conversion between JSON and R objects is done by jsonlite: data frames, matrices, simplification rules, and number handling. `json5` only adds the JSON5 syntax layer and comment handling on top. We are grateful for jsonlite's robust and carefully designed mapping, which made a small, pure-R JSON5 package possible. One small helper, `clean_names()`, is adapted from jsonlite so that comments map onto the same object keys that `jsonlite::toJSON()` produces (see `inst/COPYRIGHTS`).

If you use `json5` in published work, please also cite jsonlite:

> Ooms, J. (2014). *The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects.* arXiv:1403.2805 [stat.CO]. <https://arxiv.org/abs/1403.2805>

You can get the citation in R with `citation("jsonlite")`.

### JSON5

- The [JSON5 Data Interchange Format specification](https://spec.json5.org/) (v1.0.0) by Aseem Kishore, Jordan Tucker and contributors, under the MIT license.
- The [json5-tests](https://github.com/json5/json5-tests) conformance suite (MIT) by Aseem Kishore and contributors. It is included unmodified in `tests/testthat/json5-tests/`, with its license.

## License

MIT
