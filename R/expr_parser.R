# ---------------------------------------------------------------------------
# expr_parser.R -- lexer + Pratt parser for the C# subset used in Survey
# Solutions enabling conditions, validations, filters and calculated variables.
# Produces a small AST (nested lists with field `k`).
# ---------------------------------------------------------------------------

.cs_static_classes <- c("Math", "DateTime", "string", "String", "Convert", "int", "long",
                        "double", "decimal", "TimeSpan", "Int32", "Int64", "Double", "Decimal", "Quest")
.cs_types <- c("int", "long", "double", "decimal", "float", "string", "bool", "DateTime",
               "Int32", "Int64", "Double", "Decimal", "object")

cs_tokenize <- function(src) {
  s <- gsub("(?s)/\\*.*?\\*/", " ", src, perl = TRUE)
  s <- gsub("(^|[^:\"])//[^\n]*", "\\1 ", s, perl = TRUE)
  ch <- strsplit(s, "")[[1]]
  n <- length(ch); i <- 1L
  toks <- vector("list", n + 1); nt <- 0L
  push <- function(t) { nt <<- nt + 1L; toks[[nt]] <<- t }
  ops2 <- c("=>", "==", "!=", "<=", ">=", "&&", "||", "??", "?.")
  ops1 <- c("<", ">", "+", "-", "*", "/", "%", "!", "?", ":", "(", ")", "[", "]",
            "{", "}", ",", ".", "=", "&", "|", "^")
  isdig <- function(c) c >= "0" && c <= "9"
  isid0 <- function(c) grepl("[A-Za-z_@$]", c)
  isid  <- function(c) grepl("[A-Za-z0-9_]", c)
  while (i <= n) {
    c <- ch[i]
    if (grepl("\\s", c)) { i <- i + 1L; next }
    # numbers
    if (isdig(c) || (c == "." && i < n && isdig(ch[i + 1]))) {
      j <- i; seen_dot <- FALSE
      while (j <= n && (isdig(ch[j]) || (ch[j] == "." && !seen_dot && j < n && isdig(ch[j + 1])))) {
        if (ch[j] == ".") seen_dot <- TRUE
        j <- j + 1L
      }
      if (j <= n && ch[j] %in% c("e", "E") && j < n && grepl("[0-9+-]", ch[j + 1])) {
        j <- j + 2L; while (j <= n && isdig(ch[j])) j <- j + 1L; seen_dot <- TRUE
      }
      txt <- paste(ch[i:(j - 1)], collapse = "")
      is_int <- !seen_dot
      if (j <= n && ch[j] %in% c("m", "M", "d", "D", "f", "F")) { is_int <- FALSE; j <- j + 1L }
      else if (j <= n && ch[j] %in% c("l", "L", "u", "U")) j <- j + 1L
      push(list(t = "num", v = as.numeric(txt), int = is_int))
      i <- j; next
    }
    # strings (regular and verbatim)
    if (c == "\"" || (c == "@" && i < n && ch[i + 1] == "\"")) {
      verbatim <- c == "@"
      j <- i + if (verbatim) 2L else 1L
      buf <- character()
      while (j <= n) {
        if (!verbatim && ch[j] == "\\" && j < n) {
          nx <- ch[j + 1]
          buf <- c(buf, switch(nx, n = "\n", t = "\t", r = "\r", nx)); j <- j + 2L; next
        }
        if (ch[j] == "\"") {
          if (verbatim && j < n && ch[j + 1] == "\"") { buf <- c(buf, "\""); j <- j + 2L; next }
          break
        }
        buf <- c(buf, ch[j]); j <- j + 1L
      }
      if (j > n) stop("Unterminated string literal")
      push(list(t = "str", v = paste(buf, collapse = "")))
      i <- j + 1L; next
    }
    # char literal
    if (c == "'") {
      j <- i + 1L; buf <- character()
      while (j <= n && ch[j] != "'") {
        if (ch[j] == "\\" && j < n) { buf <- c(buf, ch[j + 1]); j <- j + 2L } else { buf <- c(buf, ch[j]); j <- j + 1L }
      }
      push(list(t = "str", v = paste(buf, collapse = "")))
      i <- j + 1L; next
    }
    # identifiers (incl. @rowcode, $macro)
    if (isid0(c)) {
      j <- i + 1L
      while (j <= n && isid(ch[j])) j <- j + 1L
      id <- paste(ch[i:(j - 1)], collapse = "")
      if (id %in% c("true", "false")) push(list(t = "bool", v = id == "true"))
      else if (id == "null") push(list(t = "null"))
      else push(list(t = "id", v = id))
      i <- j; next
    }
    if (i < n && paste0(c, ch[i + 1]) %in% ops2) {
      push(list(t = "op", v = paste0(c, ch[i + 1]))); i <- i + 2L; next
    }
    if (c %in% ops1) { push(list(t = "op", v = c)); i <- i + 1L; next }
    stop(sprintf("Unexpected character '%s'", c))
  }
  toks[seq_len(nt)]
}

.cs_prec <- c("??" = 1, "||" = 2, "&&" = 3, "|" = 4, "^" = 4, "&" = 5, "==" = 6, "!=" = 6,
              "<" = 7, ">" = 7, "<=" = 7, ">=" = 7, "+" = 8, "-" = 8, "*" = 9, "/" = 9, "%" = 9)

cs_parse <- function(src) {
  toks <- cs_tokenize(src)
  pos <- 1L
  EOF <- list(t = "eof", v = "<end>")
  peek <- function(k = 0L) if (pos + k <= length(toks)) toks[[pos + k]] else EOF
  is_op <- function(v, k = 0L) { tk <- peek(k); tk$t == "op" && tk$v %in% v }
  nxt <- function() { tk <- peek(); pos <<- pos + 1L; tk }
  expect <- function(v) {
    tk <- nxt()
    if (!(tk$t == "op" && tk$v == v)) stop(sprintf("Expected '%s' but found '%s'", v, tk$v %||% tk$t))
    tk
  }

  parse_expr <- function() {
    # lambda: x => body
    if (peek()$t == "id" && is_op("=>", 1L)) {
      p <- nxt()$v; nxt()
      return(list(k = "lambda", params = p, body = parse_expr()))
    }
    # lambda: (x, y) => body
    if (is_op("(")) {
      j <- 1L; ps <- character()
      while (peek(j)$t == "id") {
        ps <- c(ps, peek(j)$v); j <- j + 1L
        if (is_op(",", j)) j <- j + 1L else break
      }
      if (length(ps) && is_op(")", j) && is_op("=>", j + 1L)) {
        pos <<- pos + j + 2L
        return(list(k = "lambda", params = ps, body = parse_expr()))
      }
    }
    cond <- parse_binary(1)
    if (is_op("?")) {
      nxt(); yes <- parse_expr(); expect(":"); no <- parse_expr()
      return(list(k = "tern", cond = cond, yes = yes, no = no))
    }
    cond
  }

  parse_binary <- function(minp) {
    left <- parse_unary()
    repeat {
      tk <- peek()
      if (tk$t == "id" && tk$v == "is") stop("'is' operator not supported")
      if (tk$t != "op" || is.na(.cs_prec[tk$v]) || .cs_prec[tk$v] < minp) break
      p <- .cs_prec[[tk$v]]; nxt()
      right <- if (tk$v == "??") parse_binary(p) else parse_binary(p + 1)
      left <- list(k = "bin", op = tk$v, l = left, r = right)
    }
    left
  }

  try_cast <- function() {
    # (type) expr  /  (type?) expr  /  (type[]) expr
    if (!is_op("(") || peek(1L)$t != "id" || !(peek(1L)$v %in% .cs_types)) return(FALSE)
    j <- 2L
    if (is_op("?", j)) j <- j + 1L
    if (is_op("[", j) && is_op("]", j + 1L)) j <- j + 2L
    if (!is_op(")", j)) return(FALSE)
    nx <- peek(j + 1L)
    if (!(nx$t %in% c("id", "num", "str", "bool") || (nx$t == "op" && nx$v %in% c("(", "-", "!")))) return(FALSE)
    pos <<- pos + j + 1L
    TRUE
  }

  parse_unary <- function() {
    if (is_op("!")) { nxt(); return(list(k = "un", op = "!", x = parse_unary())) }
    if (is_op("-")) { nxt(); return(list(k = "un", op = "-", x = parse_unary())) }
    if (is_op("+")) { nxt(); return(parse_unary()) }
    if (try_cast()) return(parse_unary())
    parse_postfix(parse_primary())
  }

  parse_args <- function(close = ")") {
    args <- list()
    if (is_op(close)) { nxt(); return(args) }
    repeat {
      args[[length(args) + 1]] <- parse_expr()
      if (is_op(",")) { nxt(); next }
      expect(close); break
    }
    args
  }

  # Skip `<T, U>` only when it is really a generic argument list (followed by `(` or `{`)
  skip_generic <- function() {
    if (!is_op("<")) return(invisible())
    j <- 1L
    while (peek(j)$t == "id" || is_op(c(",", "?", "[", "]", "."), j)) j <- j + 1L
    if (is_op(">", j) && is_op(c("(", "{", "["), j + 1L)) pos <<- pos + j + 1L
  }

  parse_primary <- function() {
    tk <- nxt()
    switch(tk$t,
      num  = list(k = "num", v = tk$v, int = tk$int),
      str  = list(k = "str", v = tk$v),
      bool = list(k = "bool", v = tk$v),
      null = list(k = "null"),
      op = {
        if (tk$v == "(") { e <- parse_expr(); expect(")"); return(e) }
        stop(sprintf("Unexpected token '%s'", tk$v))
      },
      id = {
        if (tk$v == "new") {
          if (is_op("[")) { nxt(); expect("]"); expect("{"); return(list(k = "array", items = parse_args("}"))) }
          type <- nxt()$v
          while (is_op(".")) { nxt(); type <- paste0(type, ".", nxt()$v) }
          skip_generic()
          if (is_op("?")) nxt()                               # nullable element type: new decimal?[]{...}
          if (is_op("[")) { nxt(); expect("]"); expect("{"); return(list(k = "array", items = parse_args("}"))) }
          args <- list()
          if (is_op("(")) { nxt(); args <- parse_args(")") }
          if (is_op("{")) { nxt(); return(list(k = "array", items = parse_args("}"))) }
          return(list(k = "new", type = type, args = args))
        }
        if (tk$v %in% .cs_static_classes && is_op(".")) return(list(k = "static", name = tk$v))
        list(k = "id", name = tk$v)
      },
      stop(sprintf("Unexpected token '%s'", tk$v %||% tk$t)))
  }

  parse_postfix <- function(node) {
    force(node)
    repeat {
      if (is_op(c(".", "?."))) {
        nxt(); nm <- nxt()
        if (nm$t != "id") stop("Expected member name after '.'")
        skip_generic()
        if (is_op("(")) { nxt(); node <- list(k = "mcall", obj = node, name = nm$v, args = parse_args(")")) }
        else node <- list(k = "member", obj = node, name = nm$v)
      } else if (is_op("(")) {
        nxt(); node <- list(k = "call", fn = node, args = parse_args(")"))
      } else if (is_op("[")) {
        nxt(); idx <- parse_expr(); expect("]")
        node <- list(k = "index", obj = node, idx = idx)
      } else break
    }
    node
  }

  if (!length(toks)) stop("Empty expression")
  ast <- parse_expr()
  if (pos <= length(toks)) stop(sprintf("Unexpected trailing token '%s'", peek()$v %||% peek()$t))
  ast
}

# ---------------------------------------------------------------------------
# Compile AST -> R call. Runtime helpers are embedded as function objects so
# questionnaire variable names can never shadow them.
# ---------------------------------------------------------------------------
cs_compile <- function(a, params = character()) {
  rec <- function(x) cs_compile(x, params)
  fcall <- function(fn, ...) as.call(c(list(fn), list(...)))
  switch(a$k,
    num = if (isTRUE(a$int) && abs(a$v) < .Machine$integer.max) as.integer(a$v) else a$v,
    str = a$v,
    bool = a$v,
    null = NA,
    static = a$name,
    id = {
      nm <- a$name
      if (nm %in% params) as.name(paste0(".p_", nm))
      else if (nm == "self") fcall(rt_self)
      else if (startsWith(nm, "@")) fcall(rt_special, nm)
      else fcall(rt_var, nm)
    },
    member = {
      if (a$obj$k == "static") fcall(rt_static_member, a$obj$name, a$name)
      else fcall(rt_member, rec(a$obj), a$name)
    },
    mcall = {
      args <- lapply(a$args, rec)
      if (a$obj$k == "static") as.call(c(list(rt_static_call, a$obj$name, a$name), args))
      else as.call(c(list(rt_method, rec(a$obj), a$name), args))
    },
    call = {
      if (a$fn$k != "id") stop("Unsupported call target")
      as.call(c(list(rt_fun, a$fn$name), lapply(a$args, rec)))
    },
    index = fcall(rt_index, rec(a$obj), rec(a$idx)),
    bin = {
      if (a$op %in% c("&&", "&")) fcall(as.name("&&"), fcall(rt_truthy, rec(a$l)), fcall(rt_truthy, rec(a$r)))
      else if (a$op %in% c("||", "|")) fcall(as.name("||"), fcall(rt_truthy, rec(a$l)), fcall(rt_truthy, rec(a$r)))
      else if (a$op == "^") fcall(as.name("xor"), fcall(rt_truthy, rec(a$l)), fcall(rt_truthy, rec(a$r)))
      else fcall(rt_bin, a$op, rec(a$l), rec(a$r))
    },
    un = if (a$op == "!") fcall(as.name("!"), fcall(rt_truthy, rec(a$x))) else fcall(rt_neg, rec(a$x)),
    tern = as.call(list(as.name("if"), fcall(rt_truthy, rec(a$cond)), rec(a$yes), rec(a$no))),
    lambda = {
      p2 <- unique(c(params, a$params))
      fa <- eval(parse(text = sprintf("alist(%s)", paste0(".p_", a$params, "=", collapse = ","))))
      as.call(list(as.name("function"), as.pairlist(fa), cs_compile(a$body, p2)))
    },
    array = as.call(c(list(rt_array), lapply(a$items, rec))),
    new = as.call(c(list(rt_new, a$type), lapply(a$args, rec))),
    stop("Unknown AST node: ", a$k))
}

# ---------------------------------------------------------------------------
# Static analysis on the AST
# ---------------------------------------------------------------------------
ast_children <- function(a) {
  switch(a$k,
    member = list(a$obj), mcall = c(list(a$obj), a$args), call = a$args,
    index = list(a$obj, a$idx), bin = list(a$l, a$r), un = list(a$x),
    tern = list(a$cond, a$yes, a$no), array = a$items, new = a$args, list())
}

# Variables referenced (incl. members accessed on lambda parameters: x.age -> "age")
ast_refs <- function(ast) {
  out <- character()
  rec <- function(a, params) {
    if (!is.list(a) || is.null(a$k)) return(invisible())
    if (a$k == "id") {
      if (!(a$name %in% params) && a$name != "self" && !startsWith(a$name, "@")) out <<- c(out, a$name)
    } else if (a$k == "member" && a$obj$k == "id" && a$obj$name %in% params) {
      out <<- c(out, a$name)
    } else if (a$k == "lambda") {
      rec(a$body, c(params, a$params))
    } else for (ch in ast_children(a)) rec(ch, params)
  }
  rec(ast, character())
  unique(out)
}

.ast_var_of <- function(a, params, self_var) {
  if (a$k == "id") {
    if (a$name == "self") return(self_var)
    if (a$name %in% params || startsWith(a$name, "@")) return(NULL)
    return(a$name)
  }
  if (a$k == "member") {
    if (a$obj$k == "id" && a$obj$name %in% params) return(a$name)
    if (a$name %in% c("Value", "Yes", "No", "Length")) return(.ast_var_of(a$obj, params, self_var))
  }
  NULL
}
.ast_nums <- function(a) {
  if (a$k == "num") return(a$v)
  if (a$k == "un" && a$op == "-" && a$x$k == "num") return(-a$x$v)
  if (a$k == "array") return(unlist(lapply(a$items, .ast_nums)))
  NULL
}

# Numeric constants each variable is compared against (branch targets)
ast_targets <- function(ast, self_var = NULL) {
  out <- list(); nulls <- character()
  add <- function(v, nums) if (!is.null(v) && length(nums)) out[[v]] <<- unique(c(out[[v]], nums))
  rec <- function(a, params) {
    if (!is.list(a) || is.null(a$k)) return(invisible())
    if (a$k == "bin" && a$op %in% c("==", "!=", "<", ">", "<=", ">=")) {
      add(.ast_var_of(a$l, params, self_var), .ast_nums(a$r))
      add(.ast_var_of(a$r, params, self_var), .ast_nums(a$l))
      if (a$op %in% c("==", "!=")) {
        if (a$r$k == "null") nulls <<- c(nulls, .ast_var_of(a$l, params, self_var))
        if (a$l$k == "null") nulls <<- c(nulls, .ast_var_of(a$r, params, self_var))
      }
    }
    if (a$k == "call" && a$fn$k == "id" && a$fn$name == "IsAnswered" && length(a$args))
      nulls <<- c(nulls, .ast_var_of(a$args[[1]], params, self_var))
    if (a$k == "mcall" && a$name %in% c("InRange", "InList", "Contains", "ContainsAny",
                                         "ContainsAll", "ContainsOnly")) {
      add(.ast_var_of(a$obj, params, self_var), unlist(lapply(a$args, .ast_nums)))
    }
    if (a$k == "lambda") return(rec(a$body, c(params, a$params)))
    for (ch in ast_children(a)) rec(ch, params)
  }
  rec(ast, character())
  attr(out, "nulls") <- unique(nulls)
  out
}

# Bounds implied by a validation (top-level conjunction on self)
ast_bounds <- function(ast, self_var) {
  lo <- NA_real_; hi <- NA_real_
  conj <- function(a) if (a$k == "bin" && a$op %in% c("&&", "&")) c(conj(a$l), conj(a$r)) else list(a)
  is_self <- function(a) a$k == "id" && (a$name == "self" || identical(a$name, self_var))
  flip <- c("<" = ">", ">" = "<", "<=" = ">=", ">=" = "<=")
  for (cc in conj(ast)) {
    if (cc$k == "mcall" && cc$name == "InRange" && is_self(cc$obj) && length(cc$args) == 2) {
      l <- .ast_nums(cc$args[[1]]); h <- .ast_nums(cc$args[[2]])
      if (length(l)) lo <- suppressWarnings(max(lo, l, na.rm = TRUE))
      if (length(h)) hi <- suppressWarnings(min(hi, h, na.rm = TRUE))
    }
    if (cc$k == "bin" && cc$op %in% names(flip)) {
      op <- cc$op; num <- NULL
      if (is_self(cc$l)) num <- .ast_nums(cc$r)
      else if (is_self(cc$r)) { num <- .ast_nums(cc$l); op <- flip[[op]] }
      if (length(num) == 1) {
        if (op == ">=") lo <- suppressWarnings(max(lo, num, na.rm = TRUE))
        if (op == ">")  lo <- max(lo, num + 1e-9, na.rm = TRUE)
        if (op == "<=") hi <- suppressWarnings(min(hi, num, na.rm = TRUE))
        if (op == "<")  hi <- min(hi, num - 1e-9, na.rm = TRUE)
      }
    }
  }
  c(lo, hi)
}

#' Compile a Survey Solutions C# expression
#'
#' Exposed for debugging: shows how an expression is parsed and translated.
#' @param src Expression text.
#' @param macros Named character vector of macros (name -> content), without `$`.
#' @return List with `src`, `ast`, `call`, `refs`, `err`.
#' @export
suso_compile_expression <- function(src, macros = NULL) {
  if (is.null(src) || !nzchar(trimws(src))) return(NULL)
  s <- expand_macros(src, macros)
  out <- list(src = src, expanded = s, ast = NULL, call = NULL, refs = character(), err = NULL)
  res <- tryCatch({
    ast <- cs_parse(s)
    list(ast = ast, call = cs_compile(ast), refs = ast_refs(ast))
  }, error = function(e) list(err = conditionMessage(e)))
  utils::modifyList(out, res)
}

expand_macros <- function(src, macros) {
  if (!length(macros) || !grepl("$", src, fixed = TRUE)) return(src)
  for (k in 1:5) {
    m <- gregexpr("\\$[A-Za-z_][A-Za-z0-9_]*", src)[[1]]
    if (m[1] == -1) break
    nm <- substring(src, m + 1, m + attr(m, "match.length") - 1)
    hit <- nm[nm %in% names(macros)]
    if (!length(hit)) break
    for (h in unique(hit)) src <- gsub(paste0("\\$", h, "\\b"), paste0("(", macros[[h]], ")"), src, perl = TRUE)
  }
  src
}
