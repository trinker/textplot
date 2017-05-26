#' Wrapp Text With HTML Tags
#'
#' Wrap text with select HTML tags.  \code{tag} is generic in comparison to
#' specific tag types such as \code{p}.  Functions also contain an underscore
#' version (e.g., \code{span_}).  This version uses a built in
#' \code{\link[base]{paste}} to paste all elements together.  For example the
#' following are equivalent: \cr \cr
#' \code{h1(paste(LETTERS, ": ", em(letters)))} \cr
#' \code{h1_(LETTERS, ": ", em(letters))} \cr \cr
#' This is more natural to write with and more akin to how HTML tags work.
#'
#' @param x A text string.
#' @param left A left tag.
#' @param right A right tag.
#' @param extra Additional arguments to pass to a left tag such as styling.
#' @param \dots Multiple text strings.
#' @return Returns a vector of strings wrpped in HTML tags.
#' @rdname tag
#' @examples
#' \dontrun{#'
#' tag(LETTERS, "<iframe>")
#'
#' p(LETTERS)
#' h1_(LETTERS, ": ", em(letters))
#' p(paste(b(LETTERS), 1:26, letters))
#'
#' span(LETTERS)
#' set.seed(10)
#' (body <-p(span(LETTERS, extra=sprintf("style=\"color:%s\"", sample(colors(), 26)))))
#' template2html(insert_body(highlight_template(c(x="")), body))
#' open_html()
#' }
tag <- function(x, left, right = gsub("^<", "</", left)){
    paste0(left, x, right)
}

#' @rdname tag

p <- function(x) tag(x, "<p>")

#' @rdname tag

h1 <- function(x) tag(x, "<h1>")

#' @rdname tag

h2 <- function(x) tag(x, "<h2>")

#' @rdname tag

h3 <- function(x) tag(x, "<h3>")

#' @rdname tag

h4 <- function(x) tag(x, "<h4>")

#' @rdname tag

span <- function(x, extra = "") tag(x, sprintf("<span %s>", extra), "</span>")

#' @rdname tag

b <- function(x) tag(x, "<b>")

#' @rdname tag

em <- function(x) tag(x, "<em>")

#' @rdname tag

u <- function(x) tag(x, "<u>")


#' @rdname tag

tag_ <- function(..., left, right = gsub("^<", "</", left)){
    paste0(left, paste(...), right)
}

#' @rdname tag

p_ <- function(...) tag_(..., left = "<p>")

#' @rdname tag

h1_ <- function(...) tag_(..., left ="<h1>")

#' @rdname tag

h2_ <- function(...) tag_(..., left ="<h2>")

#' @rdname tag

h3_ <- function(...) tag_(..., left ="<h3>")

#' @rdname tag

h4_ <- function(...) tag_(..., left ="<h4>")

#' @rdname tag

span_ <- function(..., extra = "") tag_(..., left =sprintf("<span %s>", extra), right="</span>")

#' @rdname tag

b_ <- function(...) tag_(..., left ="<b>")

#' @rdname tag

em_ <- function(...) tag_(..., left ="<em>")

#' @rdname tag

u_ <- function(...) tag_(..., left ="<u>")


