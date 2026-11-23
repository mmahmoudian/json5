txt <- "
// Header comment
{
  // About name
  name: 'demo', // trailing on name
  /* block
   * comment */
  port: 8080,
  ports: [80, /* second */ 443],
  opt: null, // on null
  nested: {
    deep: true, // deep comment
    // dangling in nested
  },
  rows: [{x: 1}, {x: 2 /* cell */}],
  empty: { /* inside empty */ },
}
// Footer comment
"

test_that("comments attach to the element they describe", {
  x <- parse_json5(txt)
  expect_identical(comment(x), c("Header comment", "Footer comment"))
  expect_identical(comment(x$name), c("About name", "trailing on name"))
  expect_identical(comment(x$port), "block\ncomment")
  expect_identical(comment(x$nested$deep), "deep comment")
  expect_identical(comment(x$nested), "dangling in nested")
  expect_identical(comment(x$empty), "inside empty")
})

test_that("comments that cannot be attributes go to json5_comments", {
  x <- parse_json5(txt)
  expect_identical(attr(x, "json5_comments"), list("/opt" = "on null"))
  expect_identical(attr(x$ports, "json5_comments"), list("/1" = "second"))
  expect_identical(attr(x$rows, "json5_comments"), list("/1/x" = "cell"))
})

test_that("without simplification every element holds its own comment", {
  x <- parse_json5(txt, simplifyVector = FALSE)
  expect_identical(comment(x$ports[[2]]), "second")
  expect_identical(comment(x$rows[[2]]$x), "cell")
})

test_that("comments = FALSE drops comments", {
  x <- parse_json5(txt, comments = FALSE)
  expect_null(comment(x))
  expect_null(attr(x, "json5_comments"))
})

test_that("comments survive a write/read round trip", {
  x <- parse_json5(txt)
  y <- parse_json5(to_json5(x))
  expect_identical(y, x)
  expect_identical(parse_json5(to_json5(x, pretty = FALSE)), x)
  z <- parse_json5(txt, simplifyVector = FALSE)
  expect_identical(parse_json5(to_json5(z), simplifyVector = FALSE), z)
})

test_that("comment delimiters are stripped", {
  expect_identical(json5:::clean_comment(c("// a", "//b ", "/* c */", "/*\n * d\n * e\n */")),
                   c("a", "b", "c", "d\ne"))
})

test_that("a comment on a top-level null is dropped silently", {
  expect_null(parse_json5("// note\nnull"))
})
