test_that("JSON5 syntax features parse to the expected values", {
  x <- parse_json5("{
    unquoted: 'single', \"double\": \"q\", $id_1: 1, été: 2, \\u0061b: 3,
    class: 'reserved words are fine', Infinity: 'key', NaN: 'key',
    hex: 0xFF, neghex: -0x10, lead: .5, trail: 5., plus: +1, exp: 1e3,
    inf: Infinity, ninf: -Infinity, pinf: +Infinity, nan: NaN,
    arr: [1, 2, 3,],
  }")
  expect_identical(x$unquoted, "single")
  expect_identical(x$double, "q")
  expect_identical(x$`$id_1`, 1L)
  expect_identical(x[["été"]], 2L)
  expect_identical(x$ab, 3L)
  expect_identical(x$class, "reserved words are fine")
  expect_identical(x$Infinity, "key")
  expect_identical(x$hex, 255L)
  expect_identical(x$neghex, -16L)
  expect_identical(x$lead, 0.5)
  expect_identical(x$trail, 5)
  expect_identical(x$plus, 1L)
  expect_identical(x$exp, 1000)
  expect_identical(c(x$inf, x$ninf, x$pinf), c(Inf, -Inf, Inf))
  expect_true(is.nan(x$nan))
  expect_identical(x$arr, 1:3)
})

test_that("string escapes and line continuations are decoded", {
  expect_identical(parse_json5("'a\\'b\"c'"), "a'b\"c")
  expect_identical(parse_json5("\"\\x41\\u0042\\v\\/\\A\\C\""), "AB\v/AC")
  expect_identical(parse_json5("'\\b\\f\\n\\r\\t\\\\'"), "\b\f\n\r\t\\")
  expect_identical(parse_json5("'line1 \\\nline2'"), "line1 line2")
  expect_identical(parse_json5("'a\\\r\nb'"), "ab")
  expect_identical(parse_json5("'a\\ b'"), "ab")
  expect_identical(parse_json5("'tab\there'"), "tab\there")
  expect_identical(parse_json5("'\\uD83C\\uDFBC'"), "\U0001F3BC")
  expect_identical(parse_json5("' '"), " ")
})

test_that("large hexadecimal integers are converted exactly", {
  expect_identical(parse_json5("0x7FFFFFFFFFFFFFFF", bigint_as_char = TRUE),
                   "9223372036854775807")
  expect_identical(json5_to_json("0xFFFFFFFFFFFFFFFFFFFF")[[1]], "1208925819614629174706175")
  expect_identical(json5:::hex_to_decimal("0"), "0")
  expect_identical(json5:::hex_to_decimal("00ff"), "255")
})

test_that("NaN is restored inside vectors, lists and data frames", {
  x <- parse_json5("[1, NaN, null]")
  expect_true(is.nan(x[2]) && is.na(x[3]) && !is.nan(x[3]))
  df <- parse_json5("[{a: NaN}, {a: 1}]")
  expect_true(is.nan(df$a[1]))
  l <- parse_json5("[NaN, 'x']", simplifyVector = FALSE)
  expect_true(is.nan(l[[1]]))
})

test_that("the NaN placeholder never collides with real numbers", {
  x <- parse_json5("[-1.2345678909876543e-300, NaN]")
  expect_identical(x[1], -1.2345678909876543e-300)
  expect_true(is.nan(x[2]))
})

test_that("white space and comments are allowed everywhere", {
  txt <- "﻿ /*a*/{ //b\n k　: /*c*/ 1 /*d*/,\v\f}// end"
  expect_identical(parse_json5(txt, comments = FALSE), list(k = 1L))
})

test_that("scalars at the top level", {
  expect_identical(parse_json5("42"), 42L)
  expect_identical(parse_json5("'str'"), "str")
  expect_identical(parse_json5("true"), TRUE)
  expect_null(parse_json5("null"))
})

test_that("simplification options are passed to jsonlite", {
  txt <- "[{a: 1}, {a: 2}]"
  expect_s3_class(parse_json5(txt), "data.frame")
  expect_type(parse_json5(txt, simplifyVector = FALSE), "list")
})

test_that("invalid input gives informative errors with positions", {
  err <- function(txt) attr(validate_json5(txt), "err")
  expect_match(err("{a: 012}"), "leading zeros")
  expect_match(err("0x"), "hexadecimal")
  expect_match(err("[1,,2]"), "line 1, column 4")
  expect_match(err("{a: 1,\n b: tru}"), "line 2, column 5.*identifier 'tru'")
  expect_match(err("'abc"), "unterminated string")
  expect_match(err("/* open"), "unterminated block comment")
  expect_match(err("'\\1'"), "octal")
  expect_match(err("'\\x4'"), "\\\\x escape")
  expect_match(err("'\\0'"), "NUL")
  expect_match(err("'\\u0000'"), "NUL")
  expect_match(err("{\\u0031a: 1}"), "identifier")
  expect_match(err("[1] 2"), "after the end")
  expect_match(err(""), "empty input")
  expect_match(err("// only a comment"), "empty input")
  expect_match(err("{a: 1"), "end of the input")
  expect_match(err("{a 1}"), "':'")
  expect_match(err("- 1"), "invalid character")
  expect_match(err("{,}"), "property name")
  expect_error(parse_json5("{a: 012}"), class = "json5_parse_error")
})

test_that("error columns count characters, not bytes", {
  err <- attr(validate_json5("{\n  \u00e9t\u00e9: [1, \u00a0 2], x: \"\u4e2d\u6587 \\1\"}"), "err")
  expect_match(err, "line 2, column 25", fixed = TRUE)
  expect_match(attr(validate_json5("{b\u20ac: 1}"), "err"), "invalid identifier")
})

test_that("parsing is fast enough for large files", {
  big <- paste0("[", paste(rep("{a: 'x\u00e9', b: [1, 2.5, 0x10]}", 5000), collapse = ",\n"), "]")
  expect_lt(system.time(parse_json5(big))[["elapsed"]], 20)
})

test_that("deep nesting is limited", {
  deep <- paste0(strrep("[", 1001), strrep("]", 1001))
  expect_match(attr(validate_json5(deep), "err"), "nesting depth")
  ok <- paste0(strrep("[", 200), strrep("]", 200))
  expect_true(validate_json5(ok))
})

test_that("json5_to_json() produces strict JSON", {
  out <- json5_to_json("{a: 'x', /* c */ b: [0x10, .5, +1,],}")
  expect_s3_class(out, "json")
  expect_identical(as.character(out), "{\"a\":\"x\",\"b\":[16,0.5,1]}")
  expect_true(jsonlite::validate(out))
  expect_warning(out <- json5_to_json("[Infinity, NaN]"), "no JSON representation")
  expect_identical(as.character(out), "[null,null]")
})

test_that("read_json5() reads files, including CRLF line endings", {
  f <- tempfile(fileext = ".json5")
  on.exit(unlink(f))
  writeBin(charToRaw("// c\r\n{a: 1, // trailing\r\n b: 'x'}\r\n"), f)
  x <- read_json5(f)
  expect_identical(x$b, "x")
  expect_identical(comment(x$a), "trailing")
  expect_identical(comment(x), "c")
})

test_that("input must be character", {
  expect_error(parse_json5(1), "character")
})
