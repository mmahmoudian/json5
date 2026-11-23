# Lexical analysis of JSON5 text (JSON5 spec v1.0.0, section 9.1).
#
# The whole input is tokenized by a single vectorized call to gregexpr() with
# one alternation of named groups. Each match is classified by the group that
# participated. Order of alternatives matters: comments before everything
# that could start with "/", numbers before identifiers (so that "Infinity"
# and "NaN" are numbers), and a catch-all "err" group last so that every
# character of the input belongs to exactly one token.
#
# For speed, matching runs on the raw UTF-8 bytes (useBytes = TRUE, without
# PCRE's UTF mode, which is quadratic over long inputs). Non-ASCII characters
# are matched as UTF-8 byte sequences; the non-ASCII white space characters of
# the spec are listed explicitly, and identifiers containing other non-ASCII
# characters are validated afterwards with a Unicode-aware pattern (see
# identifier_to_json()). All offsets are therefore byte offsets.

# Non-ASCII white space as UTF-8: U+00A0, U+1680, U+2000-U+200A, U+2028,
# U+2029, U+202F, U+205F, U+3000 (category Zs plus the line/paragraph
# separators) and U+FEFF (byte order mark).
re_ws_multi <- paste0(
  r"-(\xC2\xA0|\xE1\x9A\x80|\xE2\x80[\x80-\x8A\xA8\xA9\xAF]|)-",
  r"-(\xE2\x81\x9F|\xE3\x80\x80|\xEF\xBB\xBF)-"
)
# One non-ASCII UTF-8 encoded character.
re_non_ascii <- r"-((?:[\xC2-\xDF][\x80-\xBF]|[\xE0-\xEF][\x80-\xBF]{2}|[\xF0-\xF4][\x80-\xBF]{3}))-"

re_ws <- paste0(r"-((?:[\t\n\x0B\f\r ]|)-", re_ws_multi, ")++")
re_line_comment <- r"-(//(?:[^\n\r\xE2]++|\xE2(?!\x80[\xA8\xA9]))*+)-"
re_block_comment <- r"-(/\*[\s\S]*?\*/)-"
re_string <- paste0(
  r"-("(?:[^"\\\n\r]++|\\(?:\r\n|[\s\S]))*+")-", "|",
  r"-('(?:[^'\\\n\r]++|\\(?:\r\n|[\s\S]))*+')-"
)
re_number <- paste0(
  r"-([+-]?(?:0[xX][0-9a-fA-F]++|Infinity|NaN|)-",
  r"-((?:0|[1-9][0-9]*+)(?:\.[0-9]*+)?(?:[eE][+-]?[0-9]++)?|)-",
  r"-(\.[0-9]++(?:[eE][+-]?[0-9]++)?))-"
)
re_punct <- r"-([{}\[\]:,])-"
# ES5.1 IdentifierName, approximated at the byte level: ASCII identifier
# characters, \uXXXX escapes, and any non-ASCII character that is not white
# space. Exact validation happens in identifier_to_json().
re_id_non_ascii <- paste0("(?:(?!", re_ws_multi, ")", re_non_ascii, ")")
re_identifier <- paste0(
  r"-((?:[A-Za-z$_]|\\u[0-9a-fA-F]{4}|)-", re_id_non_ascii, ")",
  r"-((?:[A-Za-z0-9$_]|\\u[0-9a-fA-F]{4}|)-", re_id_non_ascii, ")*+"
)

re_token <- paste0(
  "(?<ws>", re_ws, ")",
  "|(?<lc>", re_line_comment, ")",
  "|(?<bc>", re_block_comment, ")",
  "|(?<str>", re_string, ")",
  "|(?<num>", re_number, ")",
  "|(?<punct>", re_punct, ")",
  "|(?<id>", re_identifier, ")",
  "|(?<err>", re_non_ascii, "|[\\s\\S])"
)

re_line_terminator <- r"-(\r\n|[\n\r]|\xE2\x80[\xA8\xA9])-"

# Exact ES5.1 IdentifierName test (Unicode-aware, no escapes), used by the
# writer and to validate identifiers after decoding.
re_id_start <- r"-([\p{L}\p{Nl}$_])-"
re_id_part <- r"-([\p{L}\p{Nl}\p{Mn}\p{Mc}\p{Nd}\p{Pc}$_\x{200C}\x{200D}])-"
re_identifier_plain <- paste0("(*UTF)^", re_id_start, re_id_part, "*+$")

# Tokenize `text` (a single valid UTF-8 string).
#
# Returns a list with
#   type   : token types ("ws", "lc", "bc", "str", "num", "punct", "id", "err")
#   text   : token text (UTF-8)
#   start  : byte offset of the first byte (1-based)
#   end    : byte offset of the last byte
#   line_of: a function mapping byte offsets to line numbers
#   col_of : a function mapping byte offsets to (character) column numbers
tokenize <- function(text) {
  m <- gregexpr(re_token, text, perl = TRUE, useBytes = TRUE)[[1L]]
  lt <- gregexpr(re_line_terminator, text, perl = TRUE, useBytes = TRUE)[[1L]]
  lt_end <- if (lt[1L] == -1L) integer() else as.integer(lt) + attr(lt, "match.length") - 1L
  line_of <- function(pos) findInterval(pos - 1L, lt_end) + 1L
  col_of <- function(pos) {
    line_start <- c(0L, lt_end)[line_of(pos)] + 1L
    if (pos <= line_start) return(1L)
    prefix <- rawToChar(charToRaw(text)[line_start:(pos - 1L)])
    Encoding(prefix) <- "UTF-8"
    nchar(prefix, type = "chars", allowNA = TRUE) + 1L
  }
  if (m[1L] == -1L) {
    return(list(type = character(), text = character(), start = integer(),
                end = integer(), line_of = line_of, col_of = col_of))
  }
  start <- as.integer(m)
  len <- attr(m, "match.length")
  # Non-participating groups have capture.start 0; every token has length > 0.
  cs <- attr(m, "capture.start")
  types <- colnames(cs)[max.col(cs > 0L, ties.method = "first")]
  tokens <- regmatches(text, list(m))[[1L]]
  Encoding(tokens) <- "UTF-8"
  list(
    type = types,
    text = tokens,
    start = start,
    end = start + len - 1L,
    line_of = line_of,
    col_of = col_of
  )
}

# Signal a parse error located at byte offset `pos`.
json5_error <- function(msg, pos = NULL, tok = NULL) {
  where <- if (!is.null(pos) && !is.null(tok)) {
    sprintf(" at line %d, column %d", tok$line_of(pos), tok$col_of(pos))
  } else {
    ""
  }
  cond <- structure(
    class = c("json5_parse_error", "error", "condition"),
    list(message = paste0("JSON5 parse error", where, ": ", msg), call = NULL)
  )
  stop(cond)
}

# Describe an offending lexical "err" token for error messages.
describe_bad_char <- function(ch, following) {
  if (ch %in% c("\"", "'")) {
    "unterminated string literal (strings cannot contain unescaped line breaks)"
  } else if (ch == "/" && startsWith(following, "*")) {
    "unterminated block comment"
  } else if (ch == "\\") {
    "invalid escape sequence outside of a string"
  } else {
    cp <- utf8ToInt(ch)
    shown <- if (cp < 32L || cp == 127L) sprintf("U+%04X", cp) else sprintf("'%s'", ch)
    sprintf("invalid character %s", shown)
  }
}

# Make a token text readable in error messages.
show_token <- function(x) {
  if (nchar(x) > 30L) x <- paste0(substr(x, 1L, 27L), "...")
  sprintf("'%s'", x)
}
