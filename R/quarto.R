#' Caption text for a table (for the apaquarto \code{tbl-cap} settings line)
#'
#' Returns the table title as a plain-text string, suitable for pasting into
#' the \code{tbl-cap} settings line of a table chunk (a small block of R
#' code) in an apaquarto document (a Quarto document using the apaquarto
#' APA-style extension). Returns \code{""} if the table has no title. Does
#' not include "Table N".
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @return A length-1 character string.
#' @seealso
#' \code{\link{apa.quarto.new}(path = "my-paper")} starts an APA manuscript with the
#' apaquarto extension already installed; \code{\link{apa.quarto.setup}()}
#' installs the extension into a project you already have. A step-by-step
#' guide to placing apaTables tables in Quarto documents also comes
#' installed with the package. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#' @examples
#' my_table <- apa.cor.table(data = attitude)
#' apa_caption(x = my_table)
#' @export
apa_caption <- function(x) {
  x <- as_spec(x)
  if (is.null(x$title)) "" else as_plaintext(x$title)
}

#' Note text for a table (for the apaquarto \code{apa-note} settings line)
#'
#' Returns the table note as a plain-text string, suitable for pasting into
#' the \code{apa-note} settings line of a table chunk (a small block of R
#' code) in an apaquarto document (a Quarto document using the apaquarto
#' APA-style extension). Returns \code{""} if the table has no notes. Does
#' not include "Note.".
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @return A length-1 character string.
#' @seealso
#' \code{\link{apa.quarto.new}(path = "my-paper")} starts an APA manuscript with the
#' apaquarto extension already installed; \code{\link{apa.quarto.setup}()}
#' installs the extension into a project you already have. A step-by-step
#' guide to placing apaTables tables in Quarto documents also comes
#' installed with the package. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#' @examples
#' my_table <- apa.cor.table(data = attitude)
#' apa_note_text(x = my_table)
#'
#' # Extend the generated wording rather than replace it
#' paste(apa_note_text(x = my_table), "Data collected in 2026.")
#' @export
apa_note_text <- function(x) {
  x <- as_spec(x)
  if (!length(x$notes)) return("")
  vapply(x$notes, as_plaintext, character(1)) |> paste(collapse = " ")
}
# The name the template gives an object when it has to invent one -- the case
# where the caller passed a build CALL rather than a variable. It matches what
# apa_quarto_label() already defaults to for that same case ("tbl-mytable"),
# so the chunk's label and its object agree instead of quietly differing.
APA_TEMPLATE_OBJECT <- "mytable"

# table.type -> the builder that produces it.
#
# This map is the one cost of naming the real builder in the template's
# commented build line. A table object records its `table.type` but NOT the
# function that made it, so without this the placeholder could only say
# "..." -- which teaches the shape but not which function to look up.
#
# IT IS A SECOND COPY OF A FACT THE BUILDERS ALREADY STATE, so it is guarded
# rather than trusted: test-quarto.R asserts that every exported apa.*.table()
# appears here AND that every table.type produced by actually RUNNING the
# builders has an entry. A builder added, renamed or re-typed without touching
# this map fails the suite instead of making the template print a confidently
# wrong function name.
#
# "twoway" and "twoway-ci" both come from apa.2way.table() -- the CI variant is
# the same builder with a different argument, not a different function.
#
# THE KEYS ARE THE SPEC'S `meta$table.type`, NOT THE OBJECT'S `$table.type`.
# The two now agree for every builder, but they are different fields and once
# disagreed (apa.aov.table()'s object said "aovstats", its spec "aov"). An
# earlier draft of this map keyed on the OBJECT field, its guard test read the
# OBJECT field too, the test passed, and `R CMD check` then died running the
# `apa.aov.table` example. **The test agreed with itself and not with the
# code.** Both this map and its guard read `as_spec(x)$meta$table.type`, which
# is what apa_builder_hint() below is handed.
APA_TYPE_BUILDER <- c(
  correlation  = "apa.cor.table",
  regression   = "apa.reg.table",
  oneway       = "apa.1way.table",
  twoway       = "apa.2way.table",
  "twoway-ci"  = "apa.2way.table",
  dvalue       = "apa.d.table",
  aov          = "apa.aov.table",
  afex         = "apa.afex.table")

# What to show on the commented build line: the builder that made this table,
# or a bare "..." when the type is absent or unrecognised.
#
# THE FALLBACK IS NOT DEFENSIVE PADDING -- it is reached by ordinary use. A
# spec assembled by hand through apa_spec_start()/apa_col()/apa_spec_finish()
# carries no `table.type` at all (measured), and those are exactly the specs
# the package's own tests and any custom caller build.
# The code apa.quarto.template() prints names every argument, as everything
# the package teaches does: apa.quarto.template(apa.cor.table(attitude))
# prints `apa.cor.table(data = attitude)`. Only calls to apaTables' own
# exported functions are rewritten; anything else is printed as it was
# typed. Arguments that go to `...` (apa.reg.table()'s models) stay unnamed.
apa_name_call_args <- function(expr) {
  if (!is.call(expr)) return(expr)
  fn <- expr[[1]]
  fname <- if (is.name(fn)) {
    as.character(fn)
  } else if (is.call(fn) && identical(fn[[1]], as.name("::")) &&
             identical(as.character(fn[[2]]), "apaTables")) {
    as.character(fn[[3]])
  } else {
    return(expr)
  }
  if (!fname %in% getNamespaceExports("apaTables")) return(expr)
  f <- get(fname, envir = asNamespace("apaTables"))
  if (!is.function(f)) return(expr)
  tryCatch(match.call(f, expr), error = function(e) expr)
}

apa_builder_hint <- function(spec) {
  ty <- spec$meta$table.type
  if (is.null(ty) || !length(ty) || is.na(ty[1]) || !nzchar(ty[1])) {
    return("...")
  }
  # match(), NOT `[[`. A named character vector THROWS "subscript out of
  # bounds" on a missing name rather than returning NULL, so the `is.null()`
  # guard an earlier draft used could never run -- the error came first, and
  # it killed `R CMD check` in an example. A lookup that cannot fall back is
  # not a lookup with a fallback.
  i <- match(ty[1], names(APA_TYPE_BUILDER))
  if (is.na(i)) "..." else paste0(APA_TYPE_BUILDER[[i]], "(...)")
}



#' Print a ready-to-paste apaquarto chunk for a table
#'
#' You have made a table and want it in your Quarto document, but you would
#' rather not write the code chunk yourself. Ask for it, in the R console:
#'
#' \preformatted{table1 <- apa.cor.table(data = attitude)
#' apa.quarto.template(x = table1)}
#'
#' This prints a finished chunk, named after your table:
#'
#' \preformatted{```{r}
#' #| label: tbl-table1
#' #| message: false
#' #| warning: true
#' # table1 <- apa.cor.table(...)   # <- the line that builds your table
#' apa.quarto.pdf(x = table1)
#' ```}
#'
#' Copy it into your \code{.qmd} document, replace the comment with the
#' line that builds \code{table1} -- \code{library(apaTables)} with it --
#' and render. The chunk needs nothing more: the table's title and note are
#' written from the table itself, every time you render, so there is no
#' \code{tbl-cap} or \code{apa-note} line to fill in and nothing that can
#' quietly stop matching your analysis.
#'
#' The display line matches your document's \code{format:} line -- the
#' \code{target} argument below picks \code{\link{apa.quarto.pdf}} (the
#' default), \code{\link{apa.quarto.docx}}, \code{\link{apa.quarto.html}},
#' or \code{\link{apa.quarto.revealjs}}. \code{target = "revealjs"} prints
#' a chunk showing the table alone, because a slide adds no title or note
#' unless asked (see \strong{Slides are different} on
#' \code{\link{apa.quarto.revealjs}}'s page). The
#' printed output is self-documenting either way: it says where to put the
#' chunk. Call this function in the R console -- not inside the Quarto
#' document itself -- and copy the chunk it prints into your \code{.qmd} file.
#'
#' \strong{The chunk is always two lines: build the table, then display it.}
#' If you passed the table-building call itself --
#' \code{apa.quarto.template(x = apa.cor.table(data = attitude))} -- both lines are
#' ready to run. If you passed a table you had already built, by name, the
#' first line is a \emph{comment} showing where your own table-building line
#' belongs; replace it with that line, or delete it if the table is built in
#' an earlier chunk.
#'
#' \strong{The chunk's \code{#| label:} line is not optional} on the three
#' manuscript destinations, unlike the optional \code{tbl-cap} and \code{apa-note} lines this
#' function no longer prints. A chunk without a label beginning \code{tbl-}
#' is not a table as far as
#' Quarto is concerned, so it gets no number, no title -- generated or
#' written by hand -- and no note. This function always writes one for you
#' on those three destinations; keep it if you write the chunk yourself. A
#' \code{target = "revealjs"} chunk has no \code{#| label:} line at all, and
#' needs none: a slide deck has no float, so a label would buy nothing.
#'
#' Every apaquarto document begins with a YAML header: the settings block
#' between \code{---} lines at the very top of the document. apaquarto
#' needs an author there -- with no author, or an empty one, the render
#' stops with a LaTeX error -- and it reports a problem unless that author
#' has an affiliation and is marked \code{corresponding: true}. This
#' header is enough to render:
#'
#' \preformatted{---
#' title: "My Paper"
#' author:
#'   - name: Jane Doe
#'     corresponding: true
#'     affiliations:
#'       - name: University of Guelph
#' format: apaquarto-pdf
#' ---}
#'
#' In the R console, create the table and then ask for its Quarto code:
#'
#' \preformatted{library(apaTables)
#' table1 <- apa.cor.table(data = attitude)
#' apa.quarto.template(x = table1)}
#'
#' The printed chunk displays \code{table1} but does not create it, so the
#' chunk you paste into your Quarto document must also carry the lines that
#' do -- \code{library(apaTables)} among them, without which the render
#' stops with \code{could not find function "apa.cor.table"}. This
#' particular table fits an upright (portrait) PDF page, and a table that
#' does not is handled without anything being added to the chunk -- see
#' \emph{wide tables} below:
#'
#' \preformatted{```{r}
#' #| label: tbl-table1
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.cor.table(data = attitude)
#' apa.quarto.pdf(x = table1)
#' ```}
#'
#' Nothing in that chunk mentions the title or the note, and nothing needs
#' to: \code{apa.quarto.pdf()} writes both from the table itself, with the
#' text styling intact (italics and super/subscripts render as they would
#' anywhere else in the document, and a multi-part note starts each part,
#' such as the significance-star sentence, on its own line). They are written
#' again on every render, so they stay correct when the analysis changes.
#' To use your own wording instead, add a \code{tbl-cap} line for the title
#' or an \code{apa-note} line for the note, or pass \code{title =} /
#' \code{note =} to the entry point function -- whichever is supplied wins
#' over the generated version. \code{\link{apa_caption}} returns the
#' generated title if you would rather start from it than write one from
#' scratch.
#'
#' \emph{Wide tables.} When the table is too wide for an upright (portrait)
#' PDF page, APA style places it on a rotated (landscape) page, and that
#' happens by itself: apaTables measures the table with the same calculation
#' the chunk will use, and asks apaquarto for a rotated page when one is
#' needed. This works whether your tables sit in the text or are collected at
#' the end of the manuscript, and there is nothing to add to the chunk and
#' nothing to add to the document's YAML header. When the table given here is
#' a wide one, this function says so and reports its width, so you know why a
#' page in your document will be turned. A document that already wraps a chunk
#' in \code{::: {.landscape}} markers keeps rendering exactly as before.
#'
#' \emph{Tall tables.} A table too tall for the page it goes on continues
#' onto the next one by itself, with the column headings repeated at the top
#' of each continuation and the note below the last piece. There is no
#' settings line to add: every apaTables table is written in a form that can
#' be split across pages, and apaquarto places it accordingly.
#'
#' Run this function in the R console and copy the printed chunk into your
#' Quarto document.
#'
#' Every chunk in a Quarto document must have a different name, so the
#' chunk's name is built from the name of the table you asked for:
#' \code{apa.quarto.template(x = table3)} names the chunk \code{tbl-table3}.
#' Two different tables therefore give two chunks that can both be pasted
#' into the same document. Dots and underscores in the table's name become
#' hyphens (\code{my.table} gives \code{tbl-my-table}), because a dot inside
#' a name would break the \code{@tbl-} reference that produces the table's
#' number in your text. When there is no name to use -- because the table
#' was written out in full rather than stored under a name -- the chunk is
#' named \code{tbl-mytable}, and a second such chunk in the same document
#' needs its name changed by hand. Give the \code{label} argument to choose
#' the name yourself.
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @param label A length-1 character string naming the chunk. By default the
#'   name is built from the name of the table given for \code{x}, so
#'   \code{apa.quarto.template(x = table3)} names the chunk \code{tbl-table3};
#'   when \code{x} is not a stored table but an expression written out in
#'   full, the name is \code{tbl-mytable}. A label you supply yourself is
#'   used exactly as given, so make it begin with \code{"tbl-"} -- Quarto
#'   only treats a chunk as a table when its label does. Writing
#'   \code{@tbl-table3} in your document text then produces that
#'   table's number (for example, "Table 1").
#' @param code A length-1 character string with the R code that produces the
#'   table, used in the display line of the chunk. You can usually ignore
#'   this argument: by default the chunk is built from what you typed for
#'   \code{x}, with the arguments of an apaTables call written by name -- so
#'   \code{apa.quarto.template(x = table1)} prints the line
#'   \code{apa.quarto.pdf(x = table1)}; a table function's call given as
#'   \code{x} is printed with each of its arguments named.
#' @param target Which of the format-named entry points to print the chunk
#'   for -- \code{"pdf"} (the default), \code{"docx"}, \code{"html"}, or
#'   \code{"revealjs"}. This only picks which name the printed chunk uses;
#'   because that name is forgiving, the chunk still renders correctly
#'   however the document's own YAML \code{format:} line is actually set.
#'   Picking the matching name simply keeps the printed chunk consistent
#'   with the YAML, and changes the wide-table advice below to name
#'   the right page (a docx table rotates like a PDF one; an HTML table has
#'   no page at all, so nothing rotates and nothing shrinks -- it renders at
#'   full width regardless, and nothing is printed about landscape).
#'   \code{target = "revealjs"} is different in one more way:
#'   a slide deck has no float, so the chunk it prints has no
#'   \code{#| label: tbl-...} line either -- see
#'   \code{\link{apa.quarto.revealjs}} for why a deck has no table number, and
#'   why title and note are opt-in there instead of automatic.
#' @return The chunk text as a single character string, returned invisibly
#'   (most users simply copy the printed output; the surrounding
#'   instructions are not part of the returned text).
#' @seealso
#' \code{\link{apa.quarto.new}(path = "my-paper")} starts an APA manuscript with the
#' apaquarto extension already installed; \code{\link{apa.quarto.setup}()}
#' installs the extension into a project you already have. A step-by-step
#' guide to placing apaTables tables in Quarto documents also comes
#' installed with the package. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#' @examples
#' # Print a ready-to-paste chunk for the table (PDF document by default)
#' table1 <- apa.cor.table(data = attitude)
#' apa.quarto.template(x = table1)
#'
#' # The same chunk for a Word document
#' apa.quarto.template(x = table1, target = "docx")
#' @export
apa.quarto.template <- function(x, label = NULL, code = NULL,
                                target = c("pdf", "docx", "html",
                                           "revealjs")) {
  # match.arg() both supplies the default ("pdf", the format the rest of this
  # function's own documentation and every YAML example uses) and validates
  # the value.
  target <- match.arg(target)
  is_revealjs <- identical(target, "revealjs")

  x_expr <- substitute(x)
  if (is.null(code)) {
    code <- paste(deparse(apa_name_call_args(x_expr), width.cutoff = 500L),
                  collapse = " ")
  }
  if (is.null(label)) {
    label <- apa_quarto_label(x_expr)
  }
  spec <- as_spec(x)
  # NO TITLE LINE AND NO apa-note LINE, and nothing computes either: there
  # is no `#| tbl-cap:` line to fill, so the caption is not computed on this
  # path at all. apa_caption() remains exported and documented -- it is how
  # an author gets the generated wording when they DO want to start from
  # it -- it is simply not called here. The entry point
  # (apa.quarto.pdf()/docx()/html()) supplies the title and note from the
  # spec by default, so a printed tbl-cap or apa-note line would only ever
  # repeat what the table already provides, retyped by hand. An author who
  # wants a different note adds #| apa-note: (or note = in the call) by
  # hand; nothing here needs to suggest the wording.
  #
  # THE SAME DECISION THE CHUNK WILL MAKE, from the same function, with the
  # same INPUT: spec$landscape is passed as
  # `landscape` explicitly. There is no chunk yet at template time (the
  # document does not exist), so apa_landscape_request()'s chunk-option leg
  # is not needed here -- spec$landscape already IS what that function would
  # return, because a Quarto chunk that later sets `#| apa-landscape:` only
  # OVERRIDES the argument; before the chunk exists, the argument is the
  # whole answer.
  #
  # Do not measure with flextable here (sum(flextable_dim(...)$widths)): the
  # LaTeX entry point lays tables out as native LaTeX, so the two
  # measurements describe different tables and the advice goes wrong. For
  # apa.cor.table(attitude) flextable reports 8.6 in and the native model
  # 5.29 in, so a flextable-based template prints "too wide for a portrait
  # PDF page" and wraps a table that fits upright in ::: {.landscape} -- and
  # the table lands on a rotated page it does not need. One fact, one
  # measurement.
  #
  # suppressWarnings: the ladder's warnings belong in the RENDERED DRAFT,
  # where the user sees them beside the table, not in the console output of
  # a function whose job is to print a chunk to copy.
  #
  # can_execute_rotation is FALSE only for target = "html": apaquarto's
  # rotation machinery (apatablelandscape.lua for docx, apafloatlatex.lua's
  # landscapewrap for PDF) is never invoked on an HTML render (gated by
  # apa_quarto_docx_target()/apa_landscape_executable()), so telling
  # an HTML author their table "will be placed on a rotated page" would be
  # advice the render cannot carry out. Pricing against can_execute_rotation
  # = FALSE instead lets the ladder answer honestly: such a table shrinks
  # rather than rotates, and needs_landscape stays FALSE, so no landscape
  # advice is printed for html at all.
  #
  # SLIDE SKIPS THE LADDER ENTIRELY, rather than calling it with rotation
  # disabled the way html does. apa_layout_decide()'s ladder measures against
  # a PAGE, in inches, and a deck has none -- apa.quarto.revealjs() fits a
  # table to the slide by an entirely different, already-automatic model
  # (R/quarto-slide.R's height/width constraints), so there is no width
  # figure here that would mean anything to print, and nothing to rotate.
  #
  # WHY is the table rotating (or not)? The advice printed below has three
  # cases and one wording cannot serve them all. The MEASURED wording (an NA
  # landscape, too wide upright) says "too wide"/"automatically"/"nothing to
  # add", and each is false for a table the user forced onto a rotated page
  # with landscape = TRUE: the table may fit upright easily, the rotation
  # happens because of the argument the user typed, not measurement, and
  # removing that argument would change the result. The third case --
  # landscape = FALSE forbidding a rotation the table would otherwise have
  # gotten -- reports the shrink instead. `landscape_requested` /
  # `landscape_forbidden` read spec$landscape (the same tri-state already
  # passed into apa_layout_decide() above) to tell the three cases apart;
  # `shrunk_upright` reads decision$size against the ladder's own default
  # (apa_size_for_width() only returns something smaller than
  # APA_QUARTO_DEFAULT_SIZE when the table did not fit its budget at full
  # size) -- no second measurement.
  if (is_revealjs) {
    needs_landscape <- FALSE
    landscape_requested <- FALSE
    shrunk_upright <- FALSE
  } else {
    # col_overhead_in matches the real render's own pricing: a docx table
    # is judged with Word's fixed cell padding counted,
    # so the advice printed here has to price it the same way or it
    # describes a decision the render will not make.
    decision <- suppressWarnings(apa_layout_decide(
      apa_display_plan(spec), landscape = spec$landscape,
      can_execute_rotation = target != "html",
      col_overhead_in =
        if (identical(target, "docx")) APA_WORD_CELL_PADDING_IN else 0))
    tbl_w <- decision$width
    needs_landscape <- isTRUE(decision$rotate)
    landscape_value <- apa_landscape_value(spec$landscape)
    landscape_requested <- needs_landscape && isTRUE(landscape_value)
    landscape_forbidden <- identical(landscape_value, FALSE)
    # target != "html": apa_layout_decide()'s SHRINK math is not
    # gated by can_execute_rotation the way its ROTATE math is -- it shrinks
    # decision$size below APA_QUARTO_DEFAULT_SIZE for a wide table whether or
    # not html is the target, because shrinking is not the thing
    # can_execute_rotation was written to gate. Left ungated here, this
    # branch would print "does not fit an upright (portrait) PDF page ...
    # stays upright and is printed at N pt instead" on an HTML render, where
    # neither clause is true: nothing rotates AND nothing shrinks on HTML,
    # which has no page to fit. needs_landscape and landscape_requested
    # above are already safe because they both require decision$rotate,
    # which can_execute_rotation DOES gate.
    shrunk_upright <- !needs_landscape && landscape_forbidden &&
      target != "html" && decision$size < APA_QUARTO_DEFAULT_SIZE
  }

  # ONE display line, whatever the page. The R code in a chunk cannot see
  # any markers around it, and it does not need to be told which page it is
  # on (no page = "landscape"): R measures the table and asks apaquarto for
  # the page, so there is only one fact and only one place it is stated.
  #
  # THE CALL MATCHES THE REQUESTED TARGET: the entry
  # points are forgiving about the ACTUAL render, but the template still has
  # to commit to a name, because it is the one artifact a user reads before
  # ever rendering anything.
  target_fn <- c(pdf = "apa.quarto.pdf", docx = "apa.quarto.docx",
                 html = "apa.quarto.html",
                 revealjs = "apa.quarto.revealjs")[[target]]

  # TWO LINES, NOT A PIPE. Build the object, then pass it to a plain call.
  # This is deliberate, because of REAL CRAN REJECTIONS, and this is the site
  # that matters most: prose is something a reader MIGHT see, while this is
  # code the package HANDS the user to paste.
  #
  # TWO CASES, because substitute() above captured the CALLER'S EXPRESSION:
  #
  #   apa.quarto.template(apa.cor.table(attitude))  -> the whole build call is
  #     known, so both lines are live code and the object takes the same name
  #     apa_quarto_label() already defaults to, keeping the chunk consistent
  #     with its own label.
  #
  #   apa.quarto.template(my_table)  -> only the NAME is known. The build line
  #     is a COMMENTED placeholder sitting exactly where the real line goes.
  #
  # WHY A COMMENT RATHER THAN A PROSE SENTENCE ("Remember to include the
  # code that creates `my_table`...") in a paragraph above the block. A
  # novice copies the block, runs it, and meets the failure having already
  # scrolled past the explanation -- the prose and the code are separated at
  # the moment the prose matters.
  #
  # BOTH CASES PRINT THE SAME SHAPE, which is the point: the thing being
  # taught is "build on one line, pass on the next", and a demonstrator that
  # looks different depending on how it was called stops demonstrating it.
  is_bare_name <- grepl("^[.a-zA-Z][._a-zA-Z0-9]*$", code)
  obj <- if (is_bare_name) code else APA_TEMPLATE_OBJECT
  build_line <- if (is_bare_name) {
    sprintf("# %s <- %s   # <- the line that builds your table",
            obj, apa_builder_hint(spec))
  } else {
    sprintf("%s <- %s", obj, code)
  }
  call_line <- sprintf("%s(x = %s)", target_fn, obj)

  # NO '#| apa-longtable: true', AND NOTHING REPLACES IT. That line asked
  # apaquarto to skip the float wrapper, because a longtable sealed in a
  # \begin{table} cannot page-break and loses every row past the bottom in
  # silence. apaTables now always emits a splittable longtable grid, and the
  # bundled apaquarto reads that off the LaTeX itself and skips the wrapper
  # with no attribute at all -- so the line is a fact about the table that the
  # extension already knows, and one a user would have to keep true by hand
  # as a table grew. A document that still carries the line renders exactly
  # as before; it is simply not something to teach.
  #
  # message: false keeps package-loading chatter out of the document;
  # warning: true (knitr's default, stated deliberately) lets apaTables
  # tell you in the draft if a table cannot fit.
  #
  # NO `#| tbl-cap:` LINE. Printing the title apaTables just generated as an
  # "editable starting point" would have the reader type back into their
  # document the very text the package writes for them -- and deliberately,
  # no shipped example hand-types a caption either: captions are generated
  # by apaTables. This chunk is the artifact the package HANDS people to
  # paste, so printing the line here would make the package the source of
  # the habit its own examples avoid. A typed title also silently stops
  # matching the moment the analysis above it changes; a generated one is
  # re-derived on every render.
  #
  # SLIDE GETS NO `#| label:` EITHER. The label exists only to build a
  # float -- something for Quarto to number -- and a deck has no float at all
  # (see apa.quarto.revealjs()'s own help page). Printing it would
  # ask an author to maintain a setting Quarto never reads on this
  # destination.
  lines <- if (is_revealjs) {
    c("```{r}",
      "#| message: false",
      "#| warning: true",
      build_line,
      call_line,
      "```")
  } else {
    c("```{r}",
      sprintf("#| label: %s", label),
      "#| message: false",
      "#| warning: true",
      build_line,
      call_line,
      "```")
  }
  txt <- paste(lines, collapse = "\n")

  # NO ::: {.landscape} markers, and no floatsintext instruction with them.
  # Both were required when rotation had to be asked for; neither is now, in
  # either table placement, and the instruction had become false -- rotation
  # under apaquarto's default end-of-manuscript placement no longer needs
  # floatsintext at all. The div still WORKS for a document that already has
  # one, which is why nothing was removed from the extension; it is simply not
  # something the template should teach a new author to maintain by hand.
  cat(if (is_revealjs) "Copy this chunk into your Quarto (.qmd) slide deck:\n\n"
      else "Copy this chunk into your apaquarto (.qmd) document:\n\n")
  cat(txt, "\n", sep = "")
  if (is_revealjs) {
    # A DIFFERENT MESSAGE, not a trimmed copy of the manuscript one -- title
    # and note are the OPPOSITE default on a deck. The manuscript entry
    # points supply both automatically and tbl-cap/apa-note override them;
    # apa.quarto.revealjs() supplies NEITHER unless title = TRUE / note = TRUE
    # is passed, so there is no generated wording to describe as "editable"
    # here (see apa.quarto.revealjs()'s own help page).
    cat(sprintf(paste0(
      "\nThis chunk shows the table alone, with no title or note: a slide ",
      "deck has no\nlist of tables to number against, so %s() never adds ",
      "a table number,\nand does not add a title or note unless you ask. ",
      "Pass title = TRUE and/or\nnote = TRUE to add the ones apaTables ",
      "generated, or a string for your own.\n"), target_fn))
  } else {
    # NEITHER a title line NOR a note line is printed: the
    # entry point supplies both from the table, so any line here could only
    # repeat what the package already writes. The message says the title and
    # note are coming and how to override them, rather than handing over
    # wording to maintain.
    cat(sprintf(paste0(
      "\nThe table's title and note need no lines here.\n",
      "%s() writes both from the table itself, every time\n",
      "you render, so they stay correct when your analysis changes.\n",
      "To use your own wording instead, add an #| tbl-cap line for the title\n",
      "or an #| apa-note line for the note (or pass title = / note = to it).\n"),
      target_fn))
  }
  # No "Remember to include the code that creates ..." sentence: the chunk's
  # own commented build line SHOWS that, in the place the missing line goes,
  # and saying it here too would state one fact twice, inviting the copies
  # to drift.
  # Computed ONCE, not once per branch below: the three
  # branches only fire when target is "pdf" or "docx" -- needs_landscape and
  # shrunk_upright both require can_execute_rotation / the explicit
  # target != "html" check above, and is_revealjs forces both FALSE -- so this
  # switch() never has to answer for html or slide. It is written as an
  # explicit match with no default-to-"PDF" precisely so that stays true: a
  # target this switch does not recognise raises an error naming the target,
  # rather than silently mislabelling the page the way `if (target == "docx")
  # "Word" else "PDF"` would for every OTHER target, including "html".
  if (needs_landscape || shrunk_upright) {
    page_word <- switch(target,
      pdf = "PDF", docx = "Word",
      stop("apa.quarto.template(): no page word for target '", target, "'"))
  }
  if (needs_landscape && landscape_requested) {
    # REQUESTED, not measured: spec$landscape is TRUE. Do not say "too
    # wide" (it may not be), "automatically" (the argument caused it), or
    # "nothing to add" (the argument is exactly what was added).
    cat(sprintf(paste0(
      "\nThis table is about %.1f inches wide. It will be placed on a ",
      "rotated (landscape)\n%s page because the call that built it passed ",
      "landscape = TRUE.\n"),
      tbl_w, page_word))
  } else if (needs_landscape) {
    # MEASURED: spec$landscape is NA and the table did not fit an upright
    # page. The wording the automatic path has always used.
    cat(sprintf(paste0(
      "\nThis table is about %.1f inches wide -- too wide for an upright ",
      "(portrait)\n%s page. It will be placed on a rotated (landscape) ",
      "page automatically,\nat full size. There is nothing to add to the ",
      "chunk or to your document's\nYAML header.\n"),
      tbl_w, page_word))
  } else if (shrunk_upright) {
    # FORBIDDEN: spec$landscape is FALSE on a table that would otherwise
    # have rotated. It stays upright, at a smaller size, rather than losing
    # a column. Reached only for pdf/docx (see the target != "html" gate
    # above) -- HTML gets neither this nor any other landscape advice: it
    # has no page, so nothing rotates and nothing shrinks.
    cat(sprintf(paste0(
      "\nThis table does not fit an upright (portrait) %s page at full ",
      "size. Because\nlandscape = FALSE was passed to the call that built ",
      "it, it stays upright and is\nprinted at %s pt instead, small enough ",
      "that no column is cut off.\n"),
      page_word, apa_latex_num(decision$size)))
  }
  invisible(txt)
}

# The chunk name for a table, taken from the name the caller typed.
#
# Every chunk in a Quarto document must have a different name, so a fixed
# name would make two pasted chunks collide and abort the render with
# "Duplicate chunk label". The name typed for `x` is already recovered by
# substitute() for the display line, and it is the one thing on hand that
# differs between two tables.
#
# Only a bare name can be used: a pipeline or a function call is not a name
# and has no natural chunk name, so those fall back to the fixed
# "tbl-mytable" (the printed chunk shows the name, so a second such chunk is
# visibly the user's to rename).
#
# Anything outside letters and digits becomes a hyphen. That matters for the
# dots R users put in object names: "@tbl-my.table" in the document text
# stops the reference at the dot, so a dotted name would produce a chunk
# whose cross-reference silently fails.
apa_quarto_label <- function(expr) {
  if (!is.name(expr)) return("tbl-mytable")
  nm <- gsub("[^A-Za-z0-9]+", "-", as.character(expr))
  nm <- gsub("^-+|-+$", "", nm)
  if (!nzchar(nm)) return("tbl-mytable")
  paste0("tbl-", nm)
}

# NO HEIGHT MODEL, deliberately -- do not add one (apa_table_height() and
# its helpers are gone). It would answer one question -- will this table run
# past the bottom of its page, so that the chunk needs
# '#| apa-longtable: true'? -- and that question no longer exists. apaTables
# always emits a splittable longtable grid, and the bundled apaquarto detects
# one and declines to seal it in a float, so no height has to be predicted
# for a row to survive. Height is cosmetic (\Needspace* decides whether a
# short table moves down whole), and its argument is a hint: too large moves
# a table that did not need moving, too small lets it split, and neither
# loses a row. A height model would have to be RIGHT, and nothing would read
# it.


#' Knit-print method for apa_table_spec (displays the table in a Quarto chunk)
#'
#' Called automatically by knitr when an `apa_table_spec` object is returned
#' from a chunk. Delegates to the same rendering \code{\link{apa.quarto.pdf}}
#' / \code{\link{apa.quarto.docx}} / \code{\link{apa.quarto.html}} share, so a
#' bare `table1` on its own line in a chunk takes exactly the same path as the
#' explicit call the template writes -- including the automatic rotated page
#' for a table too wide to stand upright. "Rendering" the document turns it
#' into the finished Word, PDF, or HTML file; this method supplies the table's
#' contribution to that finished file. There is no function name here for a
#' name/target mismatch message to name (that guidance is a property of the
#' three named entry points, not of this automatic path), so none is ever
#' printed for a bare table.
#'
#' @param x An `apa_table_spec` object.
#' @param ... Additional arguments passed on when the table is not going into
#'   a PDF.
#' @return A \code{knit_asis} object.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.apa_table_spec <- function(x, ...) {
  apa_quarto_knit_print(x, ...)
}

# apa_quarto_render() (R/quarto-table.R) returns a knit_asis object on every
# path, so there is no flextable case to hand to knitr::knit_print() here --
# the `...` is accepted for interface stability but apa_quarto_render() does
# not use it.
apa_quarto_knit_print <- function(x, ...) {
  apa_quarto_render(x)
}

# One note (richtext or character) as markdown for the apa-note chunk
# setting: italics, bold, and super/subscripts become markdown styling, and
# literal *, ^, ~, _ in the text are escaped so apaquarto's markdown parsing
# shows them verbatim (an unescaped leading "* " would be eaten as a bullet
# marker, deleting a significance star).
rt_to_markdown <- function(rt) {
  runs <- if (is.character(rt)) list(rt_run(rt)) else unclass(rt)
  one <- function(r) {
    txt <- gsub("([*^~_])", "\\\\\\1", r$text)
    if (identical(r$script, "super")) txt <- paste0("^", txt, "^")
    if (identical(r$script, "sub"))   txt <- paste0("~", txt, "~")
    if (isTRUE(r$bold))   txt <- paste0("**", txt, "**")
    if (isTRUE(r$italic)) txt <- paste0("*", txt, "*")
    txt
  }
  paste0(vapply(runs, one, character(1)), collapse = "")
}

# NO apa_restore_emph() HERE, deliberately. flextable's LaTeX dependencies
# load ulem WITHOUT [normalem], which turns \emph into underlining
# document-wide, so apaquarto's italic "Note." and any *italics* in apa-note
# render UPRIGHT AND UNDERLINED in PDF wherever apa_flextable() reaches a
# LaTeX destination (measured, rasterized at 300 dpi). Nothing does: every
# LaTeX render goes through apa.quarto.pdf()'s native LaTeX emitter, no
# `ulem` package is loaded anywhere in its output, and apa_flextable() is
# reachable only from apa.save() and from direct unit-test calls with no
# knit session in progress. test-quarto-table.R asserts the function no
# longer exists. Routing a LaTeX render through apa_flextable() again would
# bring the underlining back.

#' Knit-print method for apa_table builder results (displays the table in a chunk)
#'
#' Called automatically by knitr when an `apa.*.table()` result is returned from
#' a chunk. "Rendering" the document turns it into the finished Word, PDF, or
#' HTML file. The table body is displayed the same way
#' \code{\link{apa.quarto.pdf}} / \code{\link{apa.quarto.docx}} /
#' \code{\link{apa.quarto.html}} display it (apaquarto supplies the number,
#' title, and "Note." framing from the chunk's settings lines); a table
#' object with no spec falls back to the default knitr printing.
#'
#' @param x An `apa_table` object (the result of an `apa.*.table()` builder).
#' @param ... Additional arguments passed on when the table is not going into
#'   a PDF.
#' @return The result of \code{knitr::knit_print}: a \code{knit_asis} object
#'   when the object carries a spec, otherwise the default.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.apa_table <- function(x, ...) {
  if (!is.null(x$spec)) {
    return(apa_quarto_knit_print(x$spec, ...))
  }
  NextMethod()
}
