test_that("to_json5() writes JSON5 syntax", {
  x <- list(name = "demo", "my key" = 1L, `$ok` = TRUE, inf = c(Inf, -Inf, NaN, NA))
  out <- to_json5(x, pretty = FALSE)
  expect_s3_class(out, "json5")
  expect_identical(unclass(out),
                   "{name:\"demo\",\"my key\":1,$ok:true,inf:[Infinity,-Infinity,NaN,null]}")
})

test_that("pretty output uses indentation and trailing commas", {
  out <- to_json5(list(a = 1L, b = list(c = "x")))
  expect_identical(unclass(out), "{\n  a: 1,\n  b: {\n    c: \"x\",\n  },\n}")
  out <- to_json5(list(a = 1L), trailing_comma = FALSE, indent = 4)
  expect_identical(unclass(out), "{\n    a: 1\n}")
})

test_that("short scalar arrays stay on one line", {
  expect_identical(unclass(to_json5(list(v = 1:3))), "{\n  v: [1, 2, 3],\n}")
  long <- to_json5(list(v = rep("abcdefghij", 10)))
  expect_gt(length(strsplit(long, "\n")[[1]]), 10)
})

test_that("quote styles and string escaping", {
  s <- "it's \"q\"\n \001"
  expect_identical(unclass(to_json5(s)), "\"it's \\\"q\\\"\\n\\u2028\\u0001\"")
  expect_identical(unclass(to_json5(s, quote = "single")), "'it\\'s \"q\"\\n\\u2028\\u0001'")
  expect_identical(unclass(to_json5("a\"b", quote = "auto")), "'a\"b'")
  expect_identical(unclass(to_json5("a'b", quote = "auto")), "\"a'b\"")
  expect_identical(unclass(to_json5(list(a = 1L), quote_keys = TRUE, pretty = FALSE)), "{\"a\":1}")
})

test_that("strings that look like special values stay strings", {
  x <- c("NA", "Inf", "-Inf", "NaN", "\u001ejson5\u001eNA", NA)
  out <- to_json5(x, pretty = FALSE)
  expect_identical(parse_json5(out), x)
  f <- factor(c("NaN", "b", NA))
  expect_identical(parse_json5(to_json5(f)), c("NaN", "b", NA))
})

test_that("comments are written before their element", {
  x <- list(a = 1L, b = list(2L, 3L))
  comment(x) <- "top"
  comment(x$a) <- "about a"
  comment(x$b[[2]]) <- "multi\nline"
  out <- unclass(to_json5(x))
  expect_identical(out, paste(
    "// top", "{", "  // about a", "  a: 1,", "  b: [", "    2,",
    "    /*", "     * multi", "     * line", "     */", "    3,", "  ],", "}",
    sep = "\n"))
  expect_identical(unclass(to_json5(x, pretty = FALSE)),
                   "/* top */{/* about a */a:1,b:[2,/* multi\nline */3]}")
  expect_false(grepl("//", to_json5(x, comments = FALSE)))
})

test_that("comment text cannot break out of a block comment", {
  x <- 1L
  comment(x) <- "evil */ 2"
  out <- to_json5(x, pretty = FALSE)
  expect_identical(parse_json5(out, comments = FALSE), 1L)
})

test_that("comments on unnamed and duplicated list names map like jsonlite", {
  x <- list(1L, a = 2L, a = 3L)
  comment(x[[1]]) <- "first"
  comment(x[[3]]) <- "second a"
  out <- to_json5(x, pretty = FALSE)
  expect_identical(unclass(out), "{/* first */\"1\":1,a:2,/* second a */\"a.1\":3}")
})

test_that("round trips preserve values", {
  x <- list(
    s = "text", i = 1L, d = pi, l = c(TRUE, FALSE), n = NULL,
    v = c(1.5, NA, Inf), nested = list(a = list(b = "c")),
    df = data.frame(x = 1:2, y = c("a", "b")),
    m = matrix(1:4, 2), e = list(), u = "é中\U0001F600"
  )
  y <- parse_json5(to_json5(x))
  expect_identical(y$s, x$s)
  expect_identical(y$i, x$i)
  expect_equal(y$d, x$d, tolerance = 1e-14)
  expect_identical(parse_json5(to_json5(pi, digits = I(17))), pi)
  expect_identical(y$l, x$l)
  expect_null(y$n)
  expect_identical(y$v, x$v)
  expect_identical(y$nested, x$nested)
  expect_identical(y$df, x$df)
  expect_identical(y$m, x$m)
  expect_identical(y$u, x$u)
})

test_that("the na argument is rejected", {
  expect_error(to_json5(NA, na = "string"), "not supported")
})

test_that("write_json5() writes UTF-8 files", {
  f <- tempfile(fileext = ".json5")
  on.exit(unlink(f))
  x <- list(k = "é")
  comment(x$k) <- "note"
  expect_identical(write_json5(x, f), f)
  y <- read_json5(f)
  expect_identical(y, x)
})

test_that("print method shows the text", {
  expect_output(print(to_json5(list(a = 1L))), "a: 1")
})
