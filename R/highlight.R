#' Highlight Text
#'
#' \code{hilight_term} - Highlight regex matching substrings.
#'
#' @param text.var The text string variable.
#' @param map A named list or two column dataframe.  For lists the names are colors
#' and the vectors are the conditional matches.  For dataframes the first column
#' is a color and the secon is the match condition.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one plot for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.
#' @param first.appearance logical.  If \code{TRUE} the first regex that matches
#' a sentence will be applied, otherwise the last matching regex will be applied.
#' @param ignore.case logical.  If \code{FALSE}, the pattern initial matching is
#' case sensitive and if \code{TRUE}, case is ignored during initial matching.
#' @param \ldots ignored.
#' @rdname hilight
#' @importFrom data.table := .N
#' @export
#' @examples
#' \dontrun{
#'
#' ## highlight regex expressions
#' map1 <- list(
#'     `#FF69B4` = c('\\bwe(\'[a-z]+)?\\b'),
#'     `#7CFC00` = c('\\bhe is', "he's"),
#'     yellow = 'you(\'(ll|[vr]e))?\\b',
#'     gray70 = '\\btalk'
#' )
#'
#' term_regex <- with(presidential_debates_2012,
#'     hilight_term(dialogue, map1, list(person, time)))
#'
#' plot(term_regex)
#'
#' ## tidier
#' library(tidyverse)
#'
#' map1B <- list(
#'     `orange` = c('\\bwe(\'[a-z]+)?\\b'),
#'     `pink` = c('that[\'a-z]*\\b'),
#'     yellow = 'you(\'(ll|[vr]e))?\\b',
#'     gray = '\\bI\\b'
#' )
#'
#' presidential_debates_2012 %>%
#'     dplyr::filter(person %in% c('ROMNEY', 'OBAMA')) %$%
#'     hilight_term(
#'         text.var = dialogue,
#'         map = map1B,
#'         grouping.var = person
#'     ) %>%
#'     plot()
#'
#' ## highlight tokens
#' map2 <- list(
#'     `#FF69B4` = c('talk', 'you'),
#'     `#7CFC00` = c('he', "he's", 'we\'re', 'we'),
#'     yellow = 'right',
#'     gray70 = c('.', '?', '!')
#' )
#'
#' token_match <- with(presidential_debates_2012,
#'     hilight_token(dialogue, map2, list(person, time)))
#'
#' plot(token_match)
#'
#'
#' ## highlight sentences regex
#' map3 <- list(
#'     `#FF69B4` = 'think',
#'     `#7CFC00` = c('he is', "he's", 'you(\'[vr]e)?\\b')
#' )
#'
#'
#' sent_regex <- with(presidential_debates_2012,
#'     hilight_sentence(dialogue, map3, list(person, time)))
#'
#' plot(sent_regex)
#'
#' ## highlight sentences index
#' set.seed(10)
#' map_index <- list(
#'     yellow = sample(1:2912, 200),
#'     orange = sample(1:2912, 200),
#'     `#ff69b4` = sample(1:2912, 200)
#' )
#'
#' map_index[[2]] <- map_index[[2]][!map_index[[2]] %in% map_index[[1]]]
#' map_index[[3]] <- map_index[[3]][!map_index[[3]] %in% unlist(map_index[1:2])]
#'
#'
#' sent_index <- with(presidential_debates_2012,
#'     hilight_sentence(dialogue, map_index, list(person, time)))
#'
#' plot(sent_index)
#'
#' }
hilight_term <- function(text.var, map, grouping.var = NULL, ignore.case = TRUE, ...){

    stopifnot(is.atomic(text.var))

    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {

        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- grouping.var
        } else {
            grouping <- unlist(grouping.var)
        }

    }

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
            )
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }

    ## to assign classes to text and then link class to color in script
    map <- collapse_map(check_map(map))
    reps <- paste0(mark_start(map[['class']]), "\\1", mark_end)

    text.var <- p(.mgsub(paste0("(", map[['mark']], ")"), reps,
        text.var = text.var, fixed=FALSE, ignore.case = ignore.case))

    dat <- data.table::data.table(stats::setNames(data.frame(
        text.var = text.var,
        grouping, stringsAsFactors = FALSE
    ), c('text.var', G)))[]

    ## add grouping variables as headers
    expr <- parse(text=paste0('paste(', paste(G, collapse = ", "), ", sep = '.')"))[[1]]
    dat <- dat[, `grouping` := h3(eval(expr))][,
        list(`text.var` = paste(text.var, collapse = " ")), by = 'grouping'][,
            `text.var` := paste(grouping, text.var, sep = '\n')][]
    body <- paste(dat[['text.var']], collapse = "\n")

    out <- list(body = body, style = map_to_style(unique(map[, c('color', 'class')])), html = hilight_html)
    class(out) <- 'hilight'
    out

}


#' Highlight Text
#'
#' \code{hilight_token} - Highlight matching tokens.
#'
#' @rdname hilight
#' @export
hilight_token <- function(text.var, map, grouping.var = NULL, ignore.case = FALSE, ...){

    mark <- replacement <- element_id <- token_id <- NULL

    stopifnot(is.atomic(text.var))

    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {

        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- grouping.var
        } else {
            grouping <- unlist(grouping.var)
        }

    }

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
            )
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }

    ## to assign classes to text and then link class to color in script
    map <- data.table::data.table(check_map(map))[,
        'replacement' := paste0(mark_start(class))][]

    data.table::setkey(map, 'mark')

    dat <- data.table::data.table(stats::setNames(data.frame(
            text.var = text.var,
            grouping, stringsAsFactors = FALSE
        ), c('text.var', G)))

    token_dat <- textshape::split_token(dat[, c('text.var')], 'text.var', lower = FALSE)[,
        'mark' := low(text.var, ignore.case)][]
    data.table::setkey(token_dat, 'mark')

    token_dat <- map[token_dat][,
        mark := ifelse(!is.na(replacement), paste0(replacement, text.var, mark_end), text.var)][
            order(element_id, token_id)][,
                list(text.var = p(paste(mark, collapse = " "))), by = 'element_id'][]

    dat <- data.table::data.table(dat[, G, with = FALSE], token_dat[, 'text.var', with = FALSE])

    ## add grouping variables as headers
    expr <- parse(text=paste0('paste(', paste(G, collapse = ", "), ", sep = '.')"))[[1]]
    dat <- dat[, `grouping` := h3(eval(expr))][,
        list(`text.var` = paste(text.var, collapse = " ")), by = 'grouping'][,
            `text.var` := paste(grouping, text.var, sep = '\n')][]
    body <- gsub('(\\s+)(<mark class="[a-z]+">[;:,.?!])', '\\2',
        paste(dat[['text.var']], collapse = "\n"), perl=TRUE)

    out <- list(body = body, style = map_to_style(unique(map[, c('color', 'class')])), html = hilight_html)
    class(out) <- 'hilight'
    out

}

low <- function(x, lower) {if (lower) tolower(x) else x}


#' Highlight Text
#'
#' \code{hilight_sentence} - Highlight matching sentences.
#'
#' @rdname hilight
#' @export
hilight_sentence <- function(text.var, map, grouping.var = NULL,
    first.appearance = TRUE, ignore.case = TRUE, ...){

    mark <- element_id <- NULL

    stopifnot(is.atomic(text.var))

    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {

        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- grouping.var
        } else {
            grouping <- unlist(grouping.var)
        }

    }

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
            )
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }

    map <- check_map(map)

    if (all(is.numeric(map[['mark']]))) {

        ## to assign classes to text and then link class to color in script
        map <- data.table::data.table(map)

        data.table::setkey(map, 'mark')
        class_col <- stats::setNames(map[['mark']], map[['class']])

        ## split text into sentences by grouping vars
        dat <- data.table::data.table(stats::setNames(data.frame(
            text.var = text.var,
            grouping, stringsAsFactors = FALSE
        ), c('text.var', G)))[, `mark` := 1:.N][]

        data.table::setkey(dat, 'mark')

        ## left join map
        dat <- map[dat]

        ## pasting and collapsing by turn of talk
        dat <- dat[, `text.var` := ifelse(
            is.na(class),
            text.var,
            paste0(mark_start(class), text.var, mark_end))
        ]

        dat <- dat[, `text.var` := p(text.var)][order(mark),][,
            list(`text.var` = paste(text.var, collapse = "")), by = c(G)][]

    } else {

        ## to assign classes to text and then link class to color in script
        map <- collapse_map(map)
        data.table::setkey(map, 'class')
        class_col <- stats::setNames(map[['mark']], map[['class']])

        ## split text into sentences by grouping vars
        dat <- stats::setNames(data.frame(
            text.var = text.var,
            grouping, stringsAsFactors = FALSE
        ), c('text.var', G))

        ## determine the matching class
        dat <- textshape::split_sentence(dat, 'text.var')[,
            `class` := mclass(text.var, pattern = class_col,
                ignore.case = ignore.case, first.appearance = first.appearance)][]

        data.table::setkey(dat, 'class')

        ## left join map
        dat <- map[dat]

        ## pasting and collapsing by turn of talk
        dat <- dat[, `text.var` := ifelse(
            is.na(class),
            text.var,
            paste0(mark_start(class), text.var, mark_end))
        ][, list(`text.var` = paste(text.var, collapse = " ")), by = c(G, 'element_id', 'sentence_id')][]

        dat <- dat[, list(`text.var` = paste(text.var, collapse = "")), by = c(G, 'element_id')][,
            `text.var` := p(text.var)][order(element_id),][,
            list(`text.var` = paste(text.var, collapse = "")), by = c(G)][]

    }

    ## add grouping variables as headers
    expr <- parse(text=paste0('paste(', paste(G, collapse = ", "), ", sep = '.')"))[[1]]
    dat <- dat[, `grouping` := h3(eval(expr))][, `text.var` := paste(grouping, text.var, sep = '\n')][]
    body <- paste(dat[['text.var']], collapse = "\n")

    out <- list(body = body, style = map_to_style(unique(map[, c('color', 'class')])), html = hilight_html)
    class(out) <- 'hilight'
    out

}




#' Plots a hilight Object
#'
#' Plots a hilight object
#'
#' @param x A \code{hilight} object.
#' @param path A path ending in .html to output the plot to.
#' @param open logical  If \code{TRUE} and interactive, the plot will be opened
#' in the browser or RStudio.
#' @param \ldots ignored.
#' @method plot hilight
#' @export
plot.hilight <- function(x, path = file.path(tempdir(), 'hilight.html'), open = TRUE, ...){


    if(isTRUE(getOption('knitr.in.progress'))){

        knitout <- paste(x[['style']], x[['body']], sep = "\n")

        cat(knitout)


    } else {

        x[['html']][grepl('^BODYHERE$', x[['html']])] <- x[['body']]
        x[['html']][grepl('^STYLEHERE$', x[['html']])] <- x[['style']]

        ## make the file
        cat(x[['html']], file = path)

        if (open) {

            isRStudio <- (Sys.getenv("RSTUDIO") == "1")
            if(isRStudio){
                rstudioapi::viewer(path)
            }else {
                utils::browseURL(path)
            }
        }

        return(invisible(path))

    }

}











map_to_style <- function(map){


    marks <- sprintf(paste(mark_temp, collapse="\n"), map[['class']], map[['color']])

    out <- sprintf(paste(hilight_style, collapse="\n"), paste(marks, collapse="\n"), "%s")
    class(out) <- c("highlight_style", class(out))
    out
}



hilight_style <- c("<style>", "%s", "h1 { ",
    "    display: block;", "    font-size: 1.2em;", "    margin-top: 0.0em;",
    "    margin-bottom: 0.0em;", "    margin-left: 0;", "    margin-right: 0;",
    "    font-weight: bold;", "}", ".indented {", "    margin-left: 5%%%%;",
    "    margin-right: 5%%%%;", "}", "</style>")


mark_temp <- c(
    "mark.%s {",
    "    background-color: %s;",
    "    color: black;",
    "}"
)

hilight_html <- c("<!DOCTYPE html>", "<html lang=\"en\">", "", "<head>", "<meta charset=\"utf-8\">",
    "<title>Highlighting</title>", "</head>", "", "STYLEHERE", "", "<body>", "",
    "BODYHERE", "", "</body>", "", "</html>")

