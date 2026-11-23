# Reading JSON5: tokenize -> validate & transcode to strict JSON -> parse with
# jsonlite -> restore NaN -> attach comments.

#' Parse JSON5 text into R objects
#'
#' @description
#' `parse_json5()` parses a string of JSON5 text and `read_json5()` parses a
#' file. The text is validated against the JSON5 specification (v1.0.0) and
#' converted to strict JSON. The mapping to R objects is then done by
#' [jsonlite::parse_json()], so the result is the same as
#' [jsonlite::fromJSON()] would give for the equivalent JSON.
#'
#' @section Comments:
#' With `comments = TRUE`, comments are kept and attached to the R objects
#' they describe, using R's [comment()] attribute:
#'
#' * Comments on their own line(s) before an object member or array element
#'   belong to that member or element.
#' * A comment that follows a value on the same line (before or after the
#'   comma) belongs to that value.
#' * Comments before or after the top-level value, and comments directly
#'   inside an empty object/array or before its closing bracket, belong to
#'   the enclosing object/array (or to the top-level value).
#'
#' Comment delimiters are removed: `// note` and `/* note */` both become
#' `"note"`. When an element has several comments, `comment()` is a character
#' vector with one element per comment.
#'
#' Some JSON5 values have no R object that can hold an attribute: `null`
#' (`NULL` in R), the elements of an array that jsonlite simplified to an
#' atomic vector or matrix, and the rows and cells of a data frame. Their
#' comments are stored on the closest enclosing R object, in the attribute
#' `"json5_comments"`. It is a named list that maps a relative
#' [JSON Pointer](https://www.rfc-editor.org/info/rfc6901/) (such as `"/2"` or
#' `"/0/name"`) to a character vector of comments. [to_json5()] reads both
#' attributes, so comments survive a read/write round trip. Use
#' `simplifyVector = FALSE` to keep every element as its own R object.
#'
#' @section Numbers:
#' Hexadecimal literals are converted exactly to decimal before they reach
#' jsonlite. With `bigint_as_char = TRUE`, integers that do not fit in a
#' 32-bit integer but do fit in 64 bits (up to `0x7FFFFFFFFFFFFFFF`) are
#' returned as exact strings; larger ones become doubles. `Infinity`, `-Infinity` and `NaN` become `Inf`, `-Inf` and `NaN`.
#'
#' @section Limitations:
#' R strings cannot contain the NUL character, so strings that contain `\0`,
#' `\x00` or `\u0000` raise an error. Duplicate keys in an object are all kept,
#' as in jsonlite (the specification leaves this to implementations).
#'
#' @param txt A character vector of JSON5 text. Its elements are joined with
#'   newlines.
#' @param path Path to a JSON5 file (UTF-8 encoded).
#' @param simplifyVector,simplifyDataFrame,simplifyMatrix,flatten,bigint_as_char
#'   Passed to [jsonlite::parse_json()]. The defaults match
#'   [jsonlite::fromJSON()].
#' @param comments Keep comments as attributes? See section **Comments**.
#' @param ... Further arguments passed to [jsonlite::parse_json()].
#'
#' @return The parsed R object. Invalid input raises an error of class
#'   `json5_parse_error` whose message gives the line and column.
#' @seealso [to_json5()] for the reverse, [json5_to_json()] to convert text,
#'   and [validate_json5()].
#' @export
#' @examples
#' txt <- "
#' // Server configuration
#' {
#'   host: 'localhost',
#'   port: 8080,        // default port
#'   ratio: .5,
#'   mask: 0xFF,
#'   limits: [1, 2, Infinity,],
#' }"
#' cfg <- json5::parse_json5(txt)
#' str(cfg)
#' comment(cfg)
#' comment(cfg$port)
#'
#' # Read a file
#' f <- tempfile(fileext = ".json5")
#' writeLines(txt, f)
#' identical(json5::read_json5(f), cfg)
parse_json5 <- function(txt, simplifyVector = TRUE,
                        simplifyDataFrame = simplifyVector,
                        simplifyMatrix = simplifyVector, flatten = FALSE,
                        bigint_as_char = FALSE, comments = TRUE, ...) {
  res <- json5_transcode(as_json5_text(txt), keep_comments = comments)
  value <- jsonlite::parse_json(
    res$json,
    simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix, flatten = flatten,
    bigint_as_char = bigint_as_char, ...
  )
  if (res$has_nan) value <- restore_nan(value, res$nan_value)
  if (comments && length(res$comments)) {
    for (cm in res$comments) value <- add_comment_at(value, cm$path, cm$text)
  }
  value
}

#' @rdname parse_json5
#' @export
read_json5 <- function(path, ...) {
  parse_json5(read_utf8(path), ...)
}

#' Convert JSON5 text to strict JSON
#'
#' Validates JSON5 text and rewrites it as compact, standard JSON (RFC 8259)
#' that any JSON parser can read. Comments and trailing commas are removed,
#' keys and strings are double quoted, and hexadecimal and other JSON5-only
#' number forms become plain decimal numbers. JSON has no representation for
#' `Infinity`, `-Infinity` and `NaN`, so they become `null` (with a warning).
#'
#' @inheritParams parse_json5
#' @param pretty Indent the output? Uses [jsonlite::prettify()].
#' @return A string of class `json` (from jsonlite).
#' @export
#' @examples
#' json5::json5_to_json("{a: 1, b: 'two', /* note */ c: [0x10, .5,],}")
json5_to_json <- function(txt, pretty = FALSE) {
  res <- json5_transcode(as_json5_text(txt), keep_comments = FALSE,
                         nonfinite_as_null = TRUE)
  if (res$has_nonfinite) {
    warning("Infinity and NaN have no JSON representation and were converted to null",
            call. = FALSE)
  }
  out <- res$json
  if (pretty) out <- jsonlite::prettify(out) else class(out) <- "json"
  out
}

#' Check whether text is valid JSON5
#'
#' @inheritParams parse_json5
#' @return `TRUE` if `txt` is valid JSON5. Otherwise `FALSE`, with the parse
#'   error message in the attribute `"err"`.
#' @export
#' @examples
#' json5::validate_json5("{unquoted: 'ok', trailing: [1, 2,],}")
#' json5::validate_json5("{a: 012}")
validate_json5 <- function(txt) {
  tryCatch({
    json5_transcode(as_json5_text(txt), keep_comments = FALSE)
    TRUE
  }, json5_parse_error = function(e) {
    structure(FALSE, err = conditionMessage(e))
  })
}

# ---------------------------------------------------------------------------
# Input helpers

as_json5_text <- function(txt) {
  if (!is.character(txt)) stop("'txt' must be a character vector", call. = FALSE)
  if (length(txt) == 0L) txt <- ""
  txt <- enc2utf8(paste(txt, collapse = "\n"))
  if (!validUTF8(txt)) stop("'txt' is not valid UTF-8", call. = FALSE)
  txt
}

read_utf8 <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("'path' must be a single file path", call. = FALSE)
  }
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  txt
}

# ---------------------------------------------------------------------------
# Transcoding JSON5 to JSON

# Candidate placeholder numbers for NaN. JSON has no NaN, so NaN is written as
# a finite number that is extremely unlikely to occur in real data and
# replaced by NaN after parsing. A candidate that does occur in the document
# is skipped.
nan_candidates <- c(
  "-1.2345678909876543e-300", "-9.8765432123456789e-299",
  "-3.1415926535897931e-298", "-2.7182818284590451e-297"
)

json5_transcode <- function(text, keep_comments = TRUE, nonfinite_as_null = FALSE) {
  tok <- tokenize(text)
  type <- tok$type

  # Lexical errors: report the first offending character.
  bad <- which(type == "err")
  if (length(bad)) {
    b <- bad[1L]
    following <- if (b < length(type)) substr(tok$text[b + 1L], 1L, 1L) else ""
    json5_error(describe_bad_char(tok$text[b], following), tok$start[b], tok)
  }

  sig <- which(type %in% c("str", "num", "punct", "id"))
  n <- length(sig)
  if (n == 0L) json5_error("no value found (empty input)", nchar(text, type = "bytes") + 1L, tok)
  stype <- type[sig]
  stext <- tok$text[sig]
  sstart <- tok$start[sig]
  json <- stext
  check_malformed_numbers(stype, stext, sstart, tok$end[sig], tok)

  # Strings
  is_str <- stype == "str"
  if (any(is_str)) json[is_str] <- transcode_strings(stext[is_str], sstart[is_str], tok)

  # Numbers
  is_num <- stype == "num"
  has_nan <- FALSE
  has_nonfinite <- FALSE
  nan_value <- NA_real_
  if (any(is_num)) {
    nb <- number_body(stext[is_num])
    has_nonfinite <- any(nb$body %in% c("Infinity", "NaN"))
    if (nonfinite_as_null) {
      inf_text <- c("null", "null")
      nan_text <- "null"
    } else {
      inf_text <- c("1e999", "-1e999")
      has_nan <- any(nb$body == "NaN")
      nan_text <- nan_candidates[1L]
      if (has_nan) {
        values <- suppressWarnings(as.numeric(stext[is_num]))
        free <- !(as.numeric(nan_candidates) %in% values)
        if (!any(free)) json5_error("cannot represent NaN in this document")
        nan_text <- nan_candidates[free][1L]
        nan_value <- as.numeric(nan_text)
      }
    }
    json[is_num] <- transcode_numbers(nb, inf_text, nan_text)
  }

  ps <- parse_structure(stype, stext, json, sstart, tok, eof = nchar(text, type = "bytes") + 1L)

  comments <- list()
  if (keep_comments) {
    cidx <- which(type %in% c("lc", "bc"))
    if (length(cidx)) {
      comments <- assign_comments(
        ctext = tok$text[cidx], cstart = tok$start[cidx], cend = tok$end[cidx],
        tok = tok, stype = stype, stext = stext, sstart = sstart,
        send = tok$end[sig], ps = ps, json = ps$json
      )
    }
  }

  list(json = paste(ps$json[ps$keep], collapse = ""), comments = comments,
       has_nan = has_nan, nan_value = nan_value, has_nonfinite = has_nonfinite)
}

# A number immediately followed (without separation) by another number or an
# identifier means a malformed literal such as 012, 0x, 1e or 3px.
check_malformed_numbers <- function(stype, stext, sstart, send, tok) {
  n <- length(stype)
  if (n < 2L) return(invisible())
  glued <- which(stype[-n] == "num" & stype[-1L] %in% c("num", "id") &
                   sstart[-1L] == send[-n] + 1L)
  if (!length(glued)) return(invisible())
  k <- glued[1L]
  a <- stext[k]
  b <- stext[k + 1L]
  msg <- if (grepl("^[+-]?0$", a) && grepl("^[0-9]", b)) {
    "numbers cannot have leading zeros (octal literals are not allowed)"
  } else if (grepl("^[+-]?0$", a) && grepl("^[xX]", b)) {
    "invalid hexadecimal literal"
  } else {
    sprintf("invalid number %s", show_token(paste0(a, b)))
  }
  json5_error(msg, sstart[k], tok)
}

# Rewrite JSON5 string literals (with their quotes) as JSON string literals.
# `start` gives each literal's character offset, for error messages.
transcode_strings <- function(s, start, tok) {
  body <- substr(s, 2L, nchar(s) - 1L)
  pat <- paste0(
    "(*UTF)\\\\(?:x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|0[0-9]|\r\n|[\\s\\S])",
    "|[\"\\x{00}-\\x{1F}]"
  )
  m <- gregexpr(pat, body, perl = TRUE)
  pieces <- regmatches(body, m)
  flat <- unlist(pieces, use.names = FALSE)
  if (!length(flat)) return(paste0("\"", body, "\""))

  rep <- map_string_pieces(flat)
  bad <- which(!is.na(rep$error))
  if (length(bad)) {
    b <- bad[1L]
    owner <- rep.int(seq_along(pieces), lengths(pieces))[b]
    offset <- unlist(lapply(m, function(x) if (x[1L] == -1L) integer() else as.integer(x)))[b]
    # `offset` counts characters within the body; positions are in bytes.
    byte_offset <- nchar(substr(body[owner], 1L, offset - 1L), type = "bytes")
    json5_error(rep$error[b], start[owner] + 1L + byte_offset, tok)
  }
  grp <- factor(rep.int(seq_along(pieces), lengths(pieces)), levels = seq_along(pieces))
  regmatches(body, m) <- unname(split(rep$value, grp))
  paste0("\"", body, "\"")
}

# Map each escape sequence / special character found in a JSON5 string body
# to its JSON equivalent. Returns list(value, error).
map_string_pieces <- function(x) {
  value <- rep(NA_character_, length(x))
  error <- rep(NA_character_, length(x))
  is_esc <- startsWith(x, "\\")
  e <- substring(x, 2L)

  # Unescaped characters that JSON requires to be escaped.
  raw <- !is_esc
  value[raw & x == "\""] <- "\\\""
  ctrl <- raw & x != "\""
  value[ctrl] <- json_u_escape(x[ctrl])

  # Single-character escapes (spec table 1), line continuations (removed),
  # and escapes that are identical in JSON.
  simple <- c("'" = "'", "\"" = "\\\"", "\\" = "\\\\", "/" = "/",
              b = "\\b", f = "\\f", n = "\\n", r = "\\r", t = "\\t",
              v = "\\u000b",
              "\n" = "", "\r" = "", "\r\n" = "", "\u2028" = "", "\u2029" = "")
  hit <- is_esc & e %in% names(simple)
  value[hit] <- simple[e[hit]]

  hx <- is_esc & nchar(e) == 3L & startsWith(e, "x")
  value[hx] <- paste0("\\u00", substring(e[hx], 2L))
  hu <- is_esc & nchar(e) == 5L & startsWith(e, "u")
  value[hu] <- x[hu]

  nul <- (is_esc & e == "0") | (hx & substring(e, 2L) == "00") |
    (hu & substring(e, 2L) == "0000")
  error[nul] <- "NUL characters (\\0, \\x00, \\u0000) cannot be stored in R strings"

  bad_hex <- is_esc & e %in% c("x", "u")
  error[bad_hex] <- ifelse(e[bad_hex] == "x",
                           "invalid \\x escape (expected two hexadecimal digits)",
                           "invalid \\u escape (expected four hexadecimal digits)")
  octal <- is_esc & grepl("^[0-9]", e) & !nul
  error[octal] <- "octal escape sequences (\\1 to \\9, or \\0 followed by a digit) are not allowed"

  # Any other escaped character stands for itself.
  other <- is_esc & is.na(value) & is.na(error)
  value[other] <- json_escape_char(e[other])
  list(value = value, error = error)
}

json_u_escape <- function(ch) {
  if (!length(ch)) return(character())
  sprintf("\\u%04x", vapply(ch, utf8ToInt, integer(1L), USE.NAMES = FALSE))
}

# Escape a single character for inclusion in a JSON string, if needed.
json_escape_char <- function(ch) {
  out <- ch
  cp <- vapply(ch, function(c) utf8ToInt(c)[1L], integer(1L), USE.NAMES = FALSE)
  ctrl <- cp < 32L
  out[ctrl] <- sprintf("\\u%04x", cp[ctrl])
  out
}

# Split number tokens into sign and body.
number_body <- function(x) {
  first <- substr(x, 1L, 1L)
  signed <- first %in% c("+", "-")
  list(neg = first == "-", body = ifelse(signed, substring(x, 2L), x))
}

transcode_numbers <- function(nb, inf_text, nan_text) {
  body <- nb$body
  sign <- ifelse(nb$neg, "-", "")
  out <- character(length(body))

  inf <- body == "Infinity"
  out[inf] <- ifelse(nb$neg[inf], inf_text[2L], inf_text[1L])
  nan <- body == "NaN"
  out[nan] <- nan_text

  hex <- grepl("^0[xX]", body)
  if (any(hex)) out[hex] <- paste0(sign[hex], vapply(substring(body[hex], 3L), hex_to_decimal, ""))

  dec <- !(inf | nan | hex)
  d <- body[dec]
  d <- sub("^\\.", "0.", d)                      # .5  -> 0.5
  d <- sub("\\.(?![0-9])", ".0", d, perl = TRUE)  # 5.  -> 5.0, 5.e3 -> 5.0e3
  out[dec] <- paste0(sign[dec], d)
  out
}

# Exact conversion of a hexadecimal digit string to a decimal digit string.
hex_to_decimal <- function(h) {
  digits <- strtoi(strsplit(h, "", fixed = TRUE)[[1L]], 16L)
  dec <- 0L  # little-endian vector of decimal digits
  for (d in digits) {
    carry <- d
    for (k in seq_along(dec)) {
      v <- dec[k] * 16L + carry
      dec[k] <- v %% 10L
      carry <- v %/% 10L
    }
    while (carry > 0L) {
      dec <- c(dec, carry %% 10L)
      carry <- carry %/% 10L
    }
  }
  dec <- rev(dec)
  if (length(dec) > 1L) dec <- dec[cumsum(dec != 0L) > 0L | seq_along(dec) == length(dec)]
  paste(dec, collapse = "")
}

# ---------------------------------------------------------------------------
# Syntactic analysis

max_depth <- 1000L

# Recursive-descent parser over the significant tokens (JSON5 spec 9.2).
# Validates the structure, rewrites identifier keys as JSON strings, drops
# trailing commas, and records for every token the node (object member, array
# element, or the root) it belongs to. Nodes are stored as a parent table:
# node 1 is the root; other nodes have a parent and either a key token or an
# array index (0-based).
parse_structure <- function(type, text, json, start, tok, eof) {
  n <- length(type)
  keep <- rep(TRUE, n)
  owner <- integer(n)
  node_parent <- integer(n + 1L)
  node_key <- rep(NA_integer_, n + 1L)
  node_index <- rep(NA_integer_, n + 1L)
  nn <- 1L
  i <- 1L

  new_node <- function(parent, key = NA_integer_, index = NA_integer_) {
    nn <<- nn + 1L
    node_parent[nn] <<- parent
    node_key[nn] <<- key
    node_index[nn] <<- index
    nn
  }
  fail <- function(msg) {
    if (i > n) {
      json5_error(paste0(msg, ", but reached the end of the input"), eof, tok)
    }
    json5_error(paste0(msg, ", found ", show_token(text[i])), start[i], tok)
  }
  is_p <- function(ch) i <= n && type[i] == "punct" && text[i] == ch

  parse_value <- function(node, depth) {
    if (i > n) fail("expected a value")
    t <- type[i]
    if (t == "punct") {
      if (text[i] == "{") return(parse_object(node, depth + 1L))
      if (text[i] == "[") return(parse_array(node, depth + 1L))
      fail("expected a value")
    }
    if (t == "id" && !(text[i] %in% c("true", "false", "null"))) {
      json5_error(sprintf("unexpected identifier %s (string values must be quoted)",
                          show_token(text[i])), start[i], tok)
    }
    owner[i] <<- node
    i <<- i + 1L
    invisible()
  }

  open_container <- function(node, depth) {
    if (depth > max_depth) {
      json5_error(sprintf("maximum nesting depth (%d) exceeded", max_depth), start[i], tok)
    }
    owner[i] <<- node
    i <<- i + 1L
  }
  close_container <- function(node) {
    owner[i] <<- node
    i <<- i + 1L
  }

  parse_object <- function(node, depth) {
    open_container(node, depth)
    if (is_p("}")) return(close_container(node))
    repeat {
      if (i > n) fail("expected a property name or '}'")
      t <- type[i]
      if (t == "id") {
        json[i] <<- identifier_to_json(text[i], start[i], tok)
      } else if (t == "num" && text[i] %in% c("Infinity", "NaN")) {
        json[i] <<- paste0("\"", text[i], "\"")
      } else if (t != "str") {
        fail("expected a property name or '}'")
      }
      member <- new_node(node, key = i)
      owner[i] <<- member
      i <<- i + 1L
      if (!is_p(":")) fail("expected ':' after the property name")
      owner[i] <<- member
      i <<- i + 1L
      parse_value(member, depth)
      if (is_p(",")) {
        owner[i] <<- member
        comma <- i
        i <<- i + 1L
        if (is_p("}")) {
          keep[comma] <<- FALSE
          return(close_container(node))
        }
      } else if (is_p("}")) {
        return(close_container(node))
      } else {
        fail("expected ',' or '}'")
      }
    }
  }

  parse_array <- function(node, depth) {
    open_container(node, depth)
    if (is_p("]")) return(close_container(node))
    index <- 0L
    repeat {
      if (is_p(",")) fail("expected a value or ']'")
      element <- new_node(node, index = index)
      index <- index + 1L
      parse_value(element, depth)
      if (is_p(",")) {
        owner[i] <<- element
        comma <- i
        i <<- i + 1L
        if (is_p("]")) {
          keep[comma] <<- FALSE
          return(close_container(node))
        }
      } else if (is_p("]")) {
        return(close_container(node))
      } else {
        fail("expected ',' or ']'")
      }
    }
  }

  parse_value(1L, 0L)
  if (i <= n) {
    json5_error(sprintf("unexpected %s after the end of the value", show_token(text[i])),
                start[i], tok)
  }
  list(json = json, keep = keep, owner = owner,
       node_parent = node_parent[seq_len(nn)], node_key = node_key[seq_len(nn)],
       node_index = node_index[seq_len(nn)])
}

# Decode \uXXXX escapes in an identifier key, validate it against the exact
# (Unicode-aware) IdentifierName rules, and return it as a JSON string literal.
identifier_to_json <- function(x, pos, tok) {
  if (grepl("[^A-Za-z0-9$_]", x)) {
    m <- gregexpr("\\\\u[0-9a-fA-F]{4}", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(e) {
      vapply(strtoi(substring(e, 3L), 16L), intToUtf8, "")
    })
    if (!grepl(re_identifier_plain, x, perl = TRUE)) {
      json5_error(sprintf("invalid identifier %s", show_token(x)), pos, tok)
    }
  }
  paste0("\"", x, "\"")
}

# ---------------------------------------------------------------------------
# Comments

# Decide which node each comment belongs to, and return a list of
# list(path, text) in document order (grouped by node).
#
# A comment is "trailing" (belongs to the value before it) when it starts on
# the line where the previous token ends, and nothing but a comma or closing
# bracket follows it on that line: `a: 1, // note` or `[1 /* note */]`.
# Otherwise it is "leading" and belongs to whatever comes next:
# `[1, /* note */ 2]` describes 2. A leading comment before a closing bracket
# belongs to the enclosing object/array; one after the last token belongs to
# the root.
assign_comments <- function(ctext, cstart, cend, tok, stype, stext, sstart, send, ps, json) {
  n <- length(sstart)
  prev <- findInterval(cstart, sstart)  # index of the previous significant token
  nxt <- prev + 1L
  has_prev <- prev >= 1L
  has_next <- nxt <= n
  same_line_prev <- has_prev & tok$line_of(cstart) == tok$line_of(send[pmax(prev, 1L)])
  same_line_next <- has_next & tok$line_of(cend) == tok$line_of(sstart[pmin(nxt, n)])
  next_closes <- has_next & stype[pmin(nxt, n)] == "punct" &
    stext[pmin(nxt, n)] %in% c(",", "}", "]")
  trailing <- same_line_prev & (!same_line_next | next_closes)
  node <- ifelse(trailing, ps$owner[pmax(prev, 1L)],
                 ifelse(has_next, ps$owner[pmin(nxt, n)], 1L))
  text <- clean_comment(ctext)

  key_names <- list()
  node_path <- function(id) {
    path <- list()
    while (id != 1L) {
      k <- ps$node_key[id]
      comp <- if (is.na(k)) {
        ps$node_index[id]
      } else {
        kk <- as.character(k)
        if (is.null(key_names[[kk]])) key_names[[kk]] <<- jsonlite::parse_json(json[k])
        key_names[[kk]]
      }
      path <- c(list(comp), path)
      id <- ps$node_parent[id]
    }
    path
  }
  groups <- split(text, factor(node, levels = unique(node)))
  unname(Map(function(id, txt) list(path = node_path(as.integer(id)), text = unname(txt)),
             names(groups), groups))
}

# Remove comment delimiters and block-comment decoration.
clean_comment <- function(x) {
  vapply(x, function(c) {
    if (startsWith(c, "//")) return(trimws(sub("^// ?", "", c), "right"))
    inner <- substr(c, 3L, nchar(c) - 2L)
    lines <- strsplit(inner, "\r\n|[\n\r\u2028\u2029]", perl = TRUE)[[1L]]
    if (length(lines) <= 1L) return(trimws(inner))
    lines <- sub("^\\s*\\* ?", "", lines)
    lines <- trimws(lines, "right")
    nonblank <- which(nzchar(trimws(lines)))
    if (!length(nonblank)) return("")
    lines <- lines[min(nonblank):max(nonblank)]
    if (length(lines)) lines[1L] <- trimws(lines[1L], "left")
    paste(lines, collapse = "\n")
  }, "", USE.NAMES = FALSE)
}

# Attach comments `txt` to the element of `obj` at `path` (a list of keys and
# 0-based indices). Elements that cannot hold attributes get the comment in
# the "json5_comments" attribute of the closest enclosing object.
add_comment_at <- function(obj, path, txt) {
  if (!length(path)) {
    if (is.null(obj)) return(obj)  # a top-level null cannot hold a comment
    comment(obj) <- c(comment(obj), txt)
    return(obj)
  }
  comp <- path[[1L]]
  if (is.list(obj) && !is.data.frame(obj)) {
    nms <- names(obj)
    idx <- if (is.character(comp)) {
      match(comp, nms)
    } else if (is.null(nms)) {
      comp + 1L
    } else {
      NA_integer_
    }
    if (!is.na(idx) && idx <= length(obj) && !is.null(obj[[idx]])) {
      obj[[idx]] <- add_comment_at(obj[[idx]], path[-1L], txt)
      return(obj)
    }
  }
  if (is.null(obj)) return(obj)
  ptr <- path_to_pointer(path)
  fb <- attr(obj, "json5_comments", exact = TRUE)
  if (!is.list(fb)) fb <- list()
  fb[[ptr]] <- c(fb[[ptr]], txt)
  attr(obj, "json5_comments") <- fb
  obj
}

path_to_pointer <- function(path) {
  comps <- vapply(path, function(p) {
    if (is.character(p)) escape_pointer(p) else as.character(p)
  }, "")
  paste0("/", comps, collapse = "")
}

escape_pointer <- function(x) gsub("/", "~1", gsub("~", "~0", x, fixed = TRUE), fixed = TRUE)

# ---------------------------------------------------------------------------
# NaN

restore_nan <- function(x, nan_value) {
  if (is.double(x)) {
    hit <- !is.na(x) & x == nan_value
    if (any(hit)) x[hit] <- NaN
  } else if (is.list(x)) {
    for (k in seq_along(x)) {
      if (!is.null(x[[k]])) x[[k]] <- restore_nan(x[[k]], nan_value)
    }
  }
  x
}
