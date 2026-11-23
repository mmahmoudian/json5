#' json5: Read and Write JSON5 with Comments
#'
#' A pure R parser and generator for [JSON5](https://spec.json5.org/), built
#' on top of the 'jsonlite' package by Jeroen Ooms.
#'
#' * [parse_json5()] and [read_json5()] parse JSON5 text into R objects.
#' * [to_json5()] and [write_json5()] convert R objects to JSON5.
#' * [json5_to_json()] converts JSON5 text to strict JSON.
#' * [validate_json5()] checks whether text is valid JSON5.
#'
#' Comments are preserved with R's [comment()] attribute. See the
#' **Comments** sections of [parse_json5()] and [to_json5()].
#'
#' @keywords internal
"_PACKAGE"
