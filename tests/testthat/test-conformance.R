# Official JSON5 conformance suite (https://github.com/json5/json5-tests):
#   .json  valid JSON, must parse (and give the same result as jsonlite)
#   .json5 valid JSON5, must parse
#   .js    valid ES5 but invalid JSON5, must fail
#   .txt   invalid, must fail
# *.errorSpec files give the expected error position.

suite <- test_path("json5-tests")
cases <- list.files(suite, pattern = "\\.(json|json5|js|txt)$", recursive = TRUE)
read_case <- function(f) json5:::read_utf8(file.path(suite, f))

test_that("the conformance suite is present", {
  expect_gt(length(cases), 100)
})

for (f in cases[tools::file_ext(cases) %in% c("json", "json5")]) {
  test_that(paste("accepts", f), {
    expect_true(validate_json5(read_case(f)))
    expect_no_error(parse_json5(read_case(f)))
  })
}

for (f in cases[tools::file_ext(cases) %in% c("js", "txt")]) {
  test_that(paste("rejects", f), {
    expect_false(validate_json5(read_case(f)))
    expect_error(parse_json5(read_case(f)), class = "json5_parse_error")
  })
}

for (f in cases[tools::file_ext(cases) == "json"]) {
  test_that(paste("matches jsonlite for", f), {
    txt <- read_case(f)
    expect_identical(parse_json5(txt, comments = FALSE), jsonlite::fromJSON(txt))
  })
}

# Positions are 1-based columns of the offending character, or one past the
# last character at the end of the input. Two reference positions follow other
# conventions and are skipped: the reference reports an unescaped line break
# in a string as column 0 of the next line (we report the opening quote), and
# reports end of input after a final line comment one column further right
# than after a final block comment.
position_skips <- c("strings/unescaped-multi-line-string.errorSpec",
                    "comments/top-level-inline-comment.errorSpec")

for (s in list.files(suite, pattern = "\\.errorSpec$", recursive = TRUE)) {
  stem <- sub("\\.errorSpec$", "", s)
  src <- cases[sub("\\.[^.]+$", "", cases) == stem]
  if (length(src) != 1L || s %in% position_skips) next
  test_that(paste("reports the error position for", src), {
    spec <- parse_json5(read_case(s))
    err <- attr(validate_json5(read_case(src)), "err")
    expect_match(err, sprintf("line %d, column %d", spec$lineNumber, spec$columnNumber),
                 fixed = TRUE)
  })
}
