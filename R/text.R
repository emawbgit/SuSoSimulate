# ---------------------------------------------------------------------------
# text.R -- encoding safety and localisation helpers.
# Every string that enters the package (questionnaire JSON, category files,
# frames, settings, GeoJSON, user generators) goes through as_utf8(): valid
# UTF-8 is kept, Windows-1252 / Latin-1 bytes are converted, anything else is
# replaced, so regex, Stata and Tabular writers never see invalid strings.
# fold() maps accented Latin letters and typographic punctuation to ASCII
# (portable: iconv's //TRANSLIT differs by platform) for keyword heuristics,
# logins and file names. Non-ASCII literals are \u-escaped to keep the source ASCII.
# ---------------------------------------------------------------------------

#' @noRd
as_utf8 <- function(x) {
  if (is.factor(x)) { levels(x) <- as_utf8(levels(x)); return(x) }
  if (!is.character(x) || !length(x)) return(x)
  l1 <- Encoding(x) == "latin1"
  if (any(l1)) x[l1] <- enc2utf8(x[l1])                          # declared Latin-1 -> UTF-8
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) {
    y <- iconv(x[bad], "CP1252", "UTF-8")                       # Excel / Windows exports
    miss <- is.na(y)
    if (any(miss)) y[miss] <- iconv(x[bad][miss], "latin1", "UTF-8")
    miss <- is.na(y)
    if (any(miss)) y[miss] <- iconv(x[bad][miss], "UTF-8", "UTF-8", sub = "?")
    x[bad] <- y
  }
  x <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]", "", x, perl = TRUE)   # control characters
  ok <- !is.na(x)
  Encoding(x[ok]) <- "UTF-8"                                     # valid UTF-8 bytes, any locale
  x
}

# \u literals are not marked UTF-8 in a non-UTF-8 locale (e.g. C): mark them explicitly
.fold_old <- as_utf8("\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u0100\u0102\u0104\u00e7\u0107\u010d\u0109\u010b\u00c7\u0106\u010c\u0108\u010a\u010f\u0111\u00f0\u010e\u0110\u00d0\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119\u011b\u0115\u00c8\u00c9\u00ca\u00cb\u0112\u0116\u0118\u011a\u0114\u011f\u0123\u011d\u0121\u011e\u0122\u011c\u0120\u00ec\u00ed\u00ee\u00ef\u012b\u012f\u0131\u0129\u00cc\u00cd\u00ce\u00cf\u012a\u012e\u0130\u0128\u0142\u013e\u013a\u013c\u0141\u013d\u0139\u013b\u00f1\u0144\u0148\u0146\u00d1\u0143\u0147\u0145\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u014c\u0150\u0155\u0159\u0154\u0158\u015b\u0161\u015f\u0219\u015d\u015a\u0160\u015e\u0218\u015c\u0165\u0163\u021b\u0164\u0162\u021a\u00f9\u00fa\u00fb\u00fc\u016b\u016f\u0171\u0173\u0169\u00d9\u00da\u00db\u00dc\u016a\u016e\u0170\u0172\u0168\u00fd\u00ff\u00dd\u0178\u017a\u017c\u017e\u0179\u017b\u017d\u2018\u2019\u201a\u201b\u00b4`\u02bc\u201c\u201d\u201e\u00ab\u00bb\u2013\u2014\u2010\u2011\u2212\u00a0\u2007\u202f\u2009")
.fold_new <- "aaaaaaaaaAAAAAAAAAcccccCCCCCdddDDDeeeeeeeeeEEEEEEEEEggggGGGGiiiiiiiiIIIIIIIIllllLLLLnnnnNNNNooooooooOOOOOOOOrrRRsssssSSSSStttTTTuuuuuuuuuUUUUUUUUUyyYYzzzZZZ'''''''\"\"\"\"\"-----    "
.fold_multi_from <- as_utf8(c("\u00df", "\u00e6", "\u00c6", "\u0153", "\u0152", "\u00fe", "\u00de", "\u2026", "\u00bf", "\u00a1", "\u00b0", "\u00ba", "\u00aa", "\u20ac", "\u2116"))   # character vectors, not names: names are translated to the native encoding
.fold_multi_to <- c("ss", "ae", "AE", "oe", "OE", "th", "Th", "...", "", "", "o", "o", "a", "EUR", "No")


# a data.frame / list with every string (values, names, labels) made valid UTF-8
sanitize_df <- function(df) {
  if (!is.data.frame(df)) return(df)
  names(df) <- as_utf8(names(df))
  for (j in seq_along(df)) {
    x <- df[[j]]; at <- attributes(x)
    if (is.character(x) || is.factor(x)) x <- as_utf8(x)
    if (!is.null(at$label)) at$label <- as_utf8(at$label)
    if (!is.null(at$labels)) names(at$labels) <- as_utf8(names(at$labels))
    for (a in setdiff(names(at), c("names", "levels", "class"))) attr(x, a) <- at[[a]]
    df[[j]] <- x
  }
  df
}

sanitize_list <- function(x) {
  if (is.character(x)) return(as_utf8(x))
  if (is.list(x)) { if (!is.null(names(x))) names(x) <- as_utf8(names(x)); x[] <- lapply(x, sanitize_list) }
  x
}

# file -> one UTF-8 string (BOM removed; Windows-1252 / Latin-1 files converted)
read_text_utf8 <- function(path) {
  b <- readBin(path, "raw", file.size(path))
  if (length(b) >= 3 && identical(b[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) b <- b[-(1:3)]
  as_utf8(rawToChar(b))
}

read_csv_utf8 <- function(path, ...) {
  df <- utils::read.csv(text = read_text_utf8(path), stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8", ...)
  sanitize_df(df)
}

# accents and typographic punctuation -> ASCII; other non-ASCII removed.
# Code-point lookup (chartr/iconv are locale-dependent).
.fold_cp <- c(utf8ToInt(.fold_old), vapply(.fold_multi_from, utf8ToInt, 1L, USE.NAMES = FALSE))
.fold_to <- c(strsplit(.fold_new, "")[[1]], .fold_multi_to)
fold <- function(x, lower = TRUE) {
  x <- as_utf8(as.character(x))
  out <- vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    cp <- utf8ToInt(s)
    if (anyNA(cp) || all(cp < 128L)) return(if (anyNA(cp)) "" else s)
    r <- character(length(cp)); asc <- cp < 128L
    r[asc] <- intToUtf8(cp[asc], multiple = TRUE)
    k <- match(cp[!asc], .fold_cp)
    r[!asc] <- ifelse(is.na(k), "", .fold_to[k])
    paste(r, collapse = "")
  }, "", USE.NAMES = FALSE)
  if (lower) tolower(out) else out
}

# safe for file names on every OS (ASCII letters, digits, _ . -)
safe_file_name <- function(x, fallback = "questionnaire") {
  y <- gsub("[^A-Za-z0-9_.-]+", "_", fold(x, lower = FALSE))
  y <- gsub("^[._]+|_+$", "", y)
  ifelse(nzchar(y), substr(y, 1, 100), fallback)
}

# HQ logins are ASCII letters/digits
safe_login <- function(x) gsub("[^A-Za-z0-9_.-]", "", fold(strip_html(x), lower = FALSE))

# Language of the questionnaire texts, for realistic text answers
detect_language <- function(texts) {
  w <- strsplit(paste(" ", fold(paste(texts, collapse = " ")), " "), "[^a-z]+")[[1]]
  fr <- sum(w %in% c("le", "la", "les", "des", "du", "est", "pour", "avec", "menage", "vous", "dans", "nom", "combien", "quel", "quelle"))
  es <- sum(w %in% c("el", "los", "las", "del", "es", "para", "con", "hogar", "usted", "en", "nombre", "cuantos", "cuantas", "que", "cual"))
  en <- sum(w %in% c("the", "of", "is", "for", "with", "household", "you", "in", "name", "how", "many", "what", "which"))
  c("en", "fr", "es")[which.max(c(en + 0.5, fr, es))]
}

.text_pools <- sanitize_list(list(
  fr = list(first = c("A\u00efcha", "Fatoumata", "Mamadou", "Jean-Baptiste", "Chlo\u00e9", "Adjoua", "Koffi", "Aminata", "H\u00e9l\u00e8ne", "Fran\u00e7ois", "Moussa", "Mariam"),
            last = c("Kouassi", "Traor\u00e9", "Kon\u00e9", "Diallo", "Yao", "N\u2019Guessan", "Ouattara", "Coulibaly", "Bamba", "Tour\u00e9", "K\u00e9ita", "Ehouman"),
            words = c("champ", "march\u00e9", "eau", "\u00e9cole", "r\u00e9colte", "pluie", "b\u00e9tail", "village", "saison", "prix", "for\u00eat", "rivi\u00e8re", "cr\u00e9dit", "sant\u00e9", "boutique", "\u00e9t\u00e9"),
            dir = c("Nord", "Sud", "Centre", "Est", "Ouest")),
  es = list(first = c("Jos\u00e9", "Mar\u00eda", "Bego\u00f1a", "\u00c1ngel", "Luc\u00eda", "Jes\u00fas", "In\u00e9s", "Mart\u00edn", "Sof\u00eda", "Ra\u00fal", "Concepci\u00f3n", "I\u00f1aki"),
            last = c("Pe\u00f1a", "N\u00fa\u00f1ez", "Garc\u00eda", "L\u00f3pez", "Hern\u00e1ndez", "Mart\u00ednez", "G\u00f3mez", "P\u00e9rez", "Mu\u00f1oz", "Jim\u00e9nez", "Ord\u00f3\u00f1ez", "V\u00e1zquez"),
            words = c("campo", "mercado", "agua", "escuela", "cosecha", "lluvia", "ganado", "pueblo", "temporada", "precio", "r\u00edo", "cr\u00e9dito", "salud", "tienda", "a\u00f1o", "ni\u00f1o"),
            dir = c("Norte", "Sur", "Centro", "Este", "Oeste"))))
