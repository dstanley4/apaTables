#' Construct a richtext value
#'
#' A richtext value is either a length-1 character (plain) or a list of styled
#' runs created with [rt_run()]. It is the renderer-neutral styling layer.
#' @param x a length-1 character, or a list of runs.
#' @return an object of class `apa_richtext`.
#' @keywords internal
richtext <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    structure(x, class = "apa_richtext")
  } else if (is.list(x)) {
    if (length(x) > 0L) {
      ok <- vapply(x, function(el) is.list(el) && !is.null(el[["text"]]), logical(1))
      if (!all(ok)) {
        stop("richtext(): every list element must be a run created by rt_run() (a list with a 'text' field).")
      }
    }
    structure(x, class = "apa_richtext")
  } else {
    stop("richtext() needs a length-1 character or a list of runs.")
  }
}

#' One styled run within a richtext value
#' @keywords internal
rt_run <- function(text, italic = FALSE, bold = FALSE,
                   script = c("none", "super", "sub")) {
  script <- match.arg(script)
  list(text = text, italic = italic, bold = bold, script = script)
}

#' Flatten a richtext value to plain text
#' @param x an `apa_richtext`, `apa_richtext_lines`, character, or other object.
#' @param ... unused
#' @keywords internal
as_plaintext <- function(x, ...) UseMethod("as_plaintext")

#' @exportS3Method
#' @keywords internal
as_plaintext.default <- function(x, ...) {
  if (!is.null(attr(x, "plain"))) return(attr(x, "plain"))
  if (is.character(x)) return(paste0(x, collapse = ""))
  vapply(x, function(run) run$text, character(1)) |> paste0(collapse = "")
}

#' @exportS3Method
#' @keywords internal
as_plaintext.apa_richtext <- function(x, ...) {
  if (!is.null(attr(x, "plain"))) return(attr(x, "plain"))
  if (is.character(x)) return(paste0(x, collapse = ""))
  vapply(x, function(run) run$text, character(1)) |> paste0(collapse = "")
}

#' @exportS3Method
#' @keywords internal
as_plaintext.character <- function(x, ...) paste0(x, collapse = "")

#' Stack several richtext values as lines within one cell
#' @param ... richtext objects or plain character strings to stack as lines.
#' @keywords internal
richtext_lines <- function(...) {
  lines <- lapply(list(...), function(v) if (is.character(v)) richtext(v) else v)
  structure(lines, class = "apa_richtext_lines")
}

#' @exportS3Method
#' @keywords internal
as_plaintext.apa_richtext_lines <- function(x, ...) {
  vapply(x, as_plaintext, character(1)) |> paste(collapse = "\n")
}

#' Superscript significance stars
#' @keywords internal
apa_stars <- function(n) {
  if (n <= 0) return(richtext(""))
  richtext(list(rt_run(strrep("*", n), script = "super")))
}

#' Confidence-interval bracket richtext (reuses txt.ci.brackets)
#' @keywords internal
apa_ci <- function(ll, ul, strip_zero = TRUE) {
  richtext(txt.ci.brackets(ll, ul, strip_zero = strip_zero))
}

#' "N\% CI" column label
#' @keywords internal
apa_ci_label <- function(conf = .95) {
  richtext(sprintf("%g%% CI", conf * 100))
}

#' Richtext for a named APA statistical symbol
#' @keywords internal
apa_sym <- function(name) {
  it <- function(t) rt_run(t, italic = TRUE)
  defs <- list(
    N     = list(runs = list(it("N")),                                   plain = "N"),
    n     = list(runs = list(it("n")),                                   plain = "n"),
    M     = list(runs = list(it("M")),                                   plain = "M"),
    SD    = list(runs = list(it("SD")),                                  plain = "SD"),
    b     = list(runs = list(it("b")),                                   plain = "b"),
    beta  = list(runs = list(rt_run("\u03b2", italic = TRUE)),           plain = "beta"),
    sr2   = list(runs = list(it("sr"), rt_run("2", script = "super")),   plain = "sr2"),
    r     = list(runs = list(it("r")),                                   plain = "r"),
    df    = list(runs = list(it("df")),                                  plain = "df"),
    SS    = list(runs = list(it("SS")),                                  plain = "SS"),
    MS    = list(runs = list(it("MS")),                                  plain = "MS"),
    F     = list(runs = list(it("F")),                                   plain = "F"),
    p     = list(runs = list(it("p")),                                   plain = "p"),
    eta2p = list(runs = list(rt_run("\u03b7", italic = TRUE),
                             rt_run("2", script = "super"),
                             rt_run("p", script = "sub")),               plain = "eta2p"),
    R2    = list(runs = list(it("R"), rt_run("2", script = "super")),    plain = "R2"),
    dR2   = list(runs = list(rt_run("\u0394"), it("R"),
                             rt_run("2", script = "super")),             plain = "dR2"),
    d       = list(runs = list(it("d")),                                   plain = "d"),
    MSE     = list(runs = list(it("MSE")),                                 plain = "MSE"),
    eta2g   = list(runs = list(rt_run("\u03b7", italic = TRUE),
                               rt_run("2", script = "super"),
                               rt_run("g", script = "sub")),               plain = "eta2g"),
    epsilon = list(runs = list(rt_run("\u03b5", italic = TRUE)),           plain = "epsilon"),
    df_num  = list(runs = list(it("df"), rt_run("Num", script = "sub")),   plain = "df_num"),
    df_den  = list(runs = list(it("df"), rt_run("Den", script = "sub")),   plain = "df_den")
  )
  if (is.null(defs[[name]])) stop(sprintf("apa_sym: unknown symbol '%s'", name))
  rt <- richtext(defs[[name]]$runs)
  attr(rt, "plain") <- defs[[name]]$plain
  rt
}
