# Writing JSON5: protect special strings -> jsonlite::toJSON() -> re-tokenize
# the strict JSON -> re-emit as JSON5 with comments.

#' Convert R objects to JSON5
#'
#' @description
#' `to_json5()` converts an R object to a JSON5 string and `write_json5()`
#' writes it to a file. The mapping from R objects to JSON values is done by
#' [jsonlite::toJSON()]. The result is then formatted as JSON5:
#'
#' * keys that are valid identifiers are left unquoted,
#' * `Inf`, `-Inf` and `NaN` are written as `Infinity`, `-Infinity` and `NaN`,
#'   and `NA` as `null`,
#' * pretty output uses trailing commas,
#' * comments stored with [comment()] (and in the `"json5_comments"`
#'   attribute, see [parse_json5()]) are written before the value they
#'   belong to.
#'
#' @section Comments:
#' A comment attached to an element is written on the line(s) before it: as
#' `// text` for single-line text, and as a `/* ... */` block for text with
#' line breaks. In compact output (`pretty = FALSE`) comments are written as
#' `/* text */` (text with line breaks keeps them). Comments on the columns of
#' a data frame are not written.
#'
#' @param x An R object.
#' @param path File to write to.
#' @param pretty Indent the output over several lines? Arrays of scalars that
#'   fit on one line are kept on one line.
#' @param indent Number of spaces per indentation level.
#' @param quote Quote character for strings: `"double"`, `"single"`, or
#'   `"auto"` (whichever needs fewer escapes, double quotes on a tie).
#' @param quote_keys Always quote object keys? By default keys are quoted
#'   only when they are not valid identifiers.
#' @param trailing_comma Add a comma after the last element of multi-line
#'   objects and arrays?
#' @param comments Write comments?
#' @param auto_unbox,digits,null Passed to [jsonlite::toJSON()]. Note that the
#'   defaults differ from jsonlite and suit configuration files: length-one
#'   vectors become scalars, numbers are written with up to 15 significant
#'   digits (`digits = NA`; use `digits = I(17)` to round-trip every double
#'   exactly, at the cost of output such as `0.10000000000000001`), and
#'   `NULL` becomes `null`.
#' @param ... Further arguments passed to [jsonlite::toJSON()], such as
#'   `dataframe`, `matrix`, `Date` or `POSIXt`. The `na` argument is not
#'   supported: `NA` is always written as `null`.
#'
#' @return `to_json5()` returns a string of class `json5`. `write_json5()`
#'   returns `path`, invisibly.
#' @seealso [parse_json5()]
#' @export
#' @examples
#' cfg <- list(name = "demo", port = 8080L, ratio = 0.5,
#'             tags = c("a", "b"), limits = c(1, Inf, NaN), opt = NULL)
#' comment(cfg) <- "Demo configuration"
#' comment(cfg$port) <- "default port"
#' json5::to_json5(cfg)
#' json5::to_json5(cfg, pretty = FALSE)
#' json5::to_json5(cfg, quote = "single", trailing_comma = FALSE)
#'
#' # Round trip
#' f <- tempfile(fileext = ".json5")
#' json5::write_json5(cfg, f)
#' comment(json5::read_json5(f)$port)
to_json5 <- function(x, pretty = TRUE, indent = 2L,
                     quote = c("double", "single", "auto"), quote_keys = FALSE,
                     trailing_comma = pretty, comments = TRUE,
                     auto_unbox = TRUE, digits = NA, null = "null", ...) {
  quote <- match.arg(quote)
  if ("na" %in% names(list(...))) {
    stop("the 'na' argument is not supported: NA is always written as null", call. = FALSE)
  }
  cmts <- if (comments) collect_comments(x) else new.env()
  json <- jsonlite::toJSON(protect_strings(x), pretty = FALSE,
                           auto_unbox = auto_unbox, digits = digits,
                           null = null, na = "string", ...)
  out <- emit_json5(as.character(json), cmts,
                    pretty = pretty, indent = indent, quote = quote,
                    quote_keys = quote_keys, trailing_comma = trailing_comma)
  structure(out, class = "json5")
}

#' @rdname to_json5
#' @export
write_json5 <- function(x, path, ...) {
  txt <- to_json5(x, ...)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(unclass(txt)), con, useBytes = TRUE)
  invisible(path)
}

#' @export
print.json5 <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Special strings
#
# With na = "string", jsonlite writes NA, Inf, -Inf and NaN as the strings
# "NA", "Inf", "-Inf" and "NaN". To tell those apart from genuine strings with
# the same text, genuine strings (and strings that already start with the
# marker) are prefixed with a marker before calling toJSON(); the prefix is
# removed again when the JSON is re-emitted.

special_strings <- c("NA", "Inf", "-Inf", "NaN")
protect_marker <- "\u001ejson5\u001e"

protect_strings <- function(x) {
  needs <- function(s) !is.na(s) & (s %in% special_strings | startsWith(s, protect_marker))
  if (is.factor(x)) {
    lv <- levels(x)
    hit <- needs(lv)
    if (any(hit)) levels(x)[hit] <- paste0(protect_marker, lv[hit])
  } else if (is.character(x)) {
    hit <- needs(x)
    if (any(hit)) x[hit] <- paste0(protect_marker, x[hit])
  } else if (is.list(x) && !inherits(x, c("POSIXlt", "json"))) {
    for (k in seq_along(x)) {
      if (!is.null(x[[k]])) x[[k]] <- protect_strings(x[[k]])
    }
  }
  x
}

# ---------------------------------------------------------------------------
# Comments

# Same rules jsonlite uses to turn list names into object keys
# (jsonlite:::cleannames, MIT, by Jeroen Ooms): missing names become the
# 1-based position and duplicates are made unique.
clean_names <- function(nms) {
  nms[nms == ""] <- NA_character_
  miss <- is.na(nms)
  nms[miss] <- as.character(seq_along(nms))[miss]
  make.unique(nms)
}

# Walk `x` and collect comments by JSON Pointer of the JSON value they will
# belong to. Returns an environment mapping "#<pointer>" to a character vector.
collect_comments <- function(x) {
  res <- new.env(parent = emptyenv())
  add <- function(ptr, txt) {
    key <- paste0("#", ptr)
    res[[key]] <- c(res[[key]], as.character(txt))
  }
  walk <- function(obj, ptr) {
    cm <- attr(obj, "comment", exact = TRUE)
    if (!is.null(cm)) add(ptr, cm)
    fb <- attr(obj, "json5_comments", exact = TRUE)
    if (is.list(fb)) for (nm in names(fb)) add(paste0(ptr, nm), fb[[nm]])
    if (is.list(obj) && !is.data.frame(obj) && !inherits(obj, c("POSIXlt", "json"))) {
      nms <- names(obj)
      keys <- if (is.null(nms)) {
        as.character(seq_along(obj) - 1L)
      } else {
        escape_pointer(clean_names(nms))
      }
      for (k in seq_along(obj)) walk(obj[[k]], paste0(ptr, "/", keys[k]))
    }
  }
  walk(x, "")
  res
}

format_comments <- function(txt, ind, pretty) {
  txt <- gsub("*/", "* /", txt, fixed = TRUE)
  if (!pretty) {
    return(paste0("/* ", txt, " */"))
  }
  unlist(lapply(txt, function(t) {
    lines <- strsplit(t, "\r\n|[\n\r\u2028\u2029]", perl = TRUE)[[1L]]
    if (length(lines) <= 1L) {
      paste0(ind, "//", if (nzchar(t)) " ", t)
    } else {
      c(paste0(ind, "/*"), paste0(ind, " *", ifelse(nzchar(lines), " ", ""), lines),
        paste0(ind, " */"))
    }
  }))
}

# ---------------------------------------------------------------------------
# Re-emitting JSON as JSON5

encode_string <- function(s, quote) {
  if (!length(s)) return(character())
  q <- switch(quote,
    double = rep("\"", length(s)),
    single = rep("'", length(s)),
    auto = ifelse(
      lengths(regmatches(s, gregexpr("\"", s, fixed = TRUE))) >
        lengths(regmatches(s, gregexpr("'", s, fixed = TRUE))),
      "'", "\""
    )
  )
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  dq <- q == "\""
  s[dq] <- gsub("\"", "\\\"", s[dq], fixed = TRUE)
  s[!dq] <- gsub("'", "\\'", s[!dq], fixed = TRUE)
  for (e in list(c("\b", "\\b"), c("\f", "\\f"), c("\n", "\\n"), c("\r", "\\r"), c("\t", "\\t"))) {
    s <- gsub(e[1L], e[2L], s, fixed = TRUE)
  }
  m <- gregexpr("(*UTF)[\\x{00}-\\x{1F}\\x{7F}\\x{2028}\\x{2029}]", s, perl = TRUE)
  if (any(vapply(m, function(v) v[1L] != -1L, logical(1L)))) {
    regmatches(s, m) <- lapply(regmatches(s, m), json_u_escape)
  }
  paste0(q, s, q)
}

is_identifier <- function(x) grepl(re_identifier_plain, x, perl = TRUE)

emit_json5 <- function(json, cmts, pretty, indent, quote, quote_keys, trailing_comma) {
  tok <- tokenize(enc2utf8(json))
  sig <- tok$type %in% c("str", "num", "punct", "id")
  type <- tok$type[sig]
  text <- tok$text[sig]
  n <- length(type)

  # Decode all strings at once with jsonlite, then render keys and values.
  is_str <- type == "str"
  is_key <- is_str & c(type[-1L] == "punct" & text[-1L] == ":", FALSE)
  value <- character(n)
  if (any(is_str)) {
    value[is_str] <- unlist(jsonlite::parse_json(
      paste0("[", paste(text[is_str], collapse = ","), "]")
    ))
  }
  out <- text
  if (any(is_key)) {
    k <- value[is_key]
    bare <- if (quote_keys) rep(FALSE, length(k)) else is_identifier(k)
    out[is_key][bare] <- k[bare]
    out[is_key][!bare] <- encode_string(k[!bare], quote)
  }
  is_val <- is_str & !is_key
  if (any(is_val)) {
    v <- value[is_val]
    rendered <- character(length(v))
    sp <- match(v, special_strings)
    rendered[!is.na(sp)] <- c("null", "Infinity", "-Infinity", "NaN")[sp[!is.na(sp)]]
    rest <- is.na(sp)
    vv <- v[rest]
    marked <- startsWith(vv, protect_marker)
    vv[marked] <- substring(vv[marked], nchar(protect_marker) + 1L)
    rendered[rest] <- encode_string(vv, quote)
    out[is_val] <- rendered
  }

  has_comments <- length(ls(cmts, all.names = TRUE)) > 0L
  get_comments <- function(ptr) {
    if (!has_comments) return(NULL)
    cmts[[paste0("#", ptr)]]
  }
  has_child_comments <- function(ptr) {
    has_comments && any(startsWith(ls(cmts, all.names = TRUE), paste0("#", ptr, "/")))
  }

  buf <- character(max(16L, 2L * n))
  nbuf <- 0L
  push <- function(s) {
    for (line in s) {
      nbuf <<- nbuf + 1L
      if (nbuf > length(buf)) length(buf) <<- 2L * length(buf)
      buf[nbuf] <<- line
    }
  }
  append_last <- function(s) buf[nbuf] <<- paste0(buf[nbuf], s)
  pad <- function(level) if (pretty) strrep(" ", indent * level) else ""
  colon <- if (pretty) ": " else ":"
  is_p <- function(k, ch) k <= n && type[k] == "punct" && text[k] == ch

  # Render the value starting at token k; returns the index after it.
  render <- function(k, ptr, level, lead) {
    cm <- get_comments(ptr)
    if (length(cm)) push(format_comments(cm, pad(level), pretty))
    t <- text[k]
    if (type[k] == "punct" && t %in% c("{", "[")) {
      close <- if (t == "{") "}" else "]"
      if (is_p(k + 1L, close)) {
        push(paste0(pad(level), lead, t, close))
        return(k + 2L)
      }
      if (t == "[") {
        flat <- flat_array_end(k)
        if (!is.na(flat) && !has_child_comments(ptr)) {
          items <- out[seq.int(k + 1L, flat - 1L)]
          items <- items[!(type[seq.int(k + 1L, flat - 1L)] == "punct")]
          line <- paste0(pad(level), lead, "[",
                         paste(items, collapse = if (pretty) ", " else ","), "]")
          if (!pretty || nchar(line) <= 80L) {
            push(line)
            return(flat + 1L)
          }
        }
      }
      push(paste0(pad(level), lead, t))
      j <- k + 1L
      index <- 0L
      repeat {
        if (t == "{") {
          key <- escape_pointer(value[j])
          j <- render(j + 2L, paste0(ptr, "/", key), level + 1L, paste0(out[j], colon))
        } else {
          j <- render(j, paste0(ptr, "/", index), level + 1L, "")
          index <- index + 1L
        }
        if (is_p(j, ",")) {
          append_last(",")
          j <- j + 1L
        } else {
          if (pretty && trailing_comma) append_last(",")
          break
        }
      }
      push(paste0(pad(level), close))
      return(j + 1L)
    }
    push(paste0(pad(level), lead, out[k]))
    k + 1L
  }

  # If the array opening at token k contains only scalars, return the index
  # of its closing bracket; otherwise NA.
  flat_array_end <- function(k) {
    j <- k + 1L
    while (j <= n) {
      if (type[j] == "punct") {
        if (text[j] == "]") return(j)
        if (text[j] %in% c("[", "{")) return(NA_integer_)
      }
      j <- j + 1L
    }
    NA_integer_
  }

  render(1L, "", 0L, "")
  lines <- buf[seq_len(nbuf)]
  if (pretty) paste(lines, collapse = "\n") else paste(lines, collapse = "")
}
