#' Lexical Dispersion
#'
#' Generate a lexical dispersion dataset (location of terms).  Typically the
#' user will want to use \code{lexical_dispersion_plot} directly but this allows
#' the return of the data that generates the plot.
#'
#' @param text.var The text variable.
#' @param term.list  A vector of quoted terms or a named list of quoted terms.
#' If the latter terms will be combined into a single unified theme named
#' according to the list names.  Note that terms within the vectors of the list
#' cannot be duplicated.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.
#' @param rm.var The repeated measures variables.  Default \code{NULL} generates
#' one facet for all text.  Also takes a single repeated measures variable or
#' a list of 1 or more grouping variables.
#' @param group.names A vector of names that corresponds to group.var.  Generally
#' for internal use.
#' @param time.names A vector of names that corresponds to rm.var.  Generally
#' for internal use.
#' @param ignore.case logical.  If \code{TRUE} matching will be done without
#' regard to case.
#' @param \ldots Ignored.
#' @return Returns dispersion plot data (location of the terms) and default
#' plots the dispersion plot.
#' @keywords dispersion
#' @export
#' @importFrom graphics plot text
#' @importFrom data.table .N .GRP
#' @note The match.terms is character sensitive.  Spacing is an important way
#' to grab specific words and requires careful thought.  Using "read" will find
#' the words "bread", "read" "reading", and "ready".  If you want to search
#' for just the word "read" you'd supply a vector of c(" read ", " reads",
#' " reading", " reader").
#' @examples
#' x <- lexical_dispersion(sam_i_am, c(' not ', ' eat ', ' sam ', ' (sam|eat) '))
#' plot(x)
lexical_dispersion <- function(text.var, term.list, grouping.var = NULL,
    rm.var = NULL, group.names, time.names, ignore.case = TRUE, ...) {

    value <- NULL

    if (is.null(grouping.var)){
        G <- NULL
        grouping <- NULL
    } else {
        if (is.list(grouping.var) & length(grouping.var) > 1) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- grouping.var
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
            grouping <- unlist(grouping.var)
        }

        if(!missing(group.names)) {
            G <- group.names
        }
    }

    if (is.null(rm.var)){
        R <- NULL
        timing <- NULL
    } else {
        if (is.list(rm.var) & length(rm.var) > 1) {
            m <- unlist(as.character(substitute(rm.var))[-1])
            R <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            timing <- rm.var
        } else {
            R <- as.character(substitute(rm.var))
            R <- R[length(R)]
            timing <- unlist(rm.var)
        }

        if(!missing(time.names)) {
            R <- time.names
        }
    }

    if (length(R) > 2) stop("A max of 2 variables can be assigned to `rm.var`")

    ## merge the demographics and the term location data
    by_dat <- NULL

    if (!is.null(G) | !is.null(R)){
        if (!is.null(G) & !is.list(grouping)) {
            grouping <- list(grouping)
        }
        if (!is.null(R) & !is.list(timing)) {
            timing <- list(timing)
        }

        if (!is.null(R)) {
            by_dat <- stats::setNames(as.data.frame(unlist(list(grouping, timing), recursive=FALSE),
                stringsAsFactors = FALSE), c(G, R))
            data.table::setDT(by_dat)

            by_dat[, 'time_id' := .GRP, by = R][, 'row_id' := 1:.N, by = 'time_id']
        } else {
            by_dat <- stats::setNames(as.data.frame(grouping,
                stringsAsFactors = FALSE), G)
            by_dat[["time_id"]] <- 1
            data.table::setDT(by_dat)
            by_dat[, 'row_id' := 1:.N, by = 'time_id']
        }
    } else {
        by_dat <- data.table::data.table(time_id = rep(1, length(text.var)))
        by_dat[, 'row_id' := 1:.N, by = 'time_id']
    }

    out <- locate_terms(text.var = text.var, term.list = term.list,
        ignore.case = ignore.case, along = by_dat[, c('time_id', 'row_id'), with = FALSE])

    lens <- out[, .N, by = "time_id"]
    ## drop non-hit rows
    out <- out[(value), ][, 'value' :=  NULL]
    if (nrow(out) == 0) {
        warning("No terms could be located in the text variable.")
        return(invisible(FALSE))
    }

    out <- merge(out, by_dat, by = c("time_id", "row_id"))

    class(out) <- unique(c("lexical_dispersion", class(out)))
    locations <- new.env(FALSE)
    attributes(out)[["locations"]] <- out
    attributes(out)[["groupings"]] <- G
    attributes(out)[["timings"]] <- R
    attributes(out)[["length"]] <- lens
    attributes(out)[['terms']] <- term.list
    
    out
}


#' Plots a lexical_dispersion object
#'
#' Plots a lexical_dispersion object.
#'
#' @param x The lexical_dispersion object.
#' @param color The color of the word symbols.
#' @param bg.color The background color.
#' @param horiz.color The color of the horizontal tracking stripe.  Use
#' \code{horiz.color = bg.color} to eliminate.
#' @param total.color The color to use for summary `all` group.  If \code{NULL}
#' totals are dropped.
#' @param symbol The word symbol.  Default is \code{"|"}.
#' @param title Title of the plot
#' @param rev.factor logical.  If \code{TRUE} reverses the plot order of the
#' factors.
#' @param wrap a character to wrap around the words (enables the reader to
#' visualize spaces).  Default is \code{"'"}, use \code{""} to remove.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param size The size of the plotting symbol.
#' @param scales Should scales be fixed (\code{"fixed"}, the default), free
#' (\code{"free"}), or free in one dimension (\code{"free_x"}, \code{"free_y"})
#' @param space If \code{"fixed"}, the default, all panels have the same size.
#' If \code{"free_y"} their height will be proportional to the length of the y
#' scale; if \code{"free_x"} their width will be proportional to the length of
#' the x scale; or if \code{"free"} both height and width will vary.
#' @param \ldots Ignored.
#' @method plot lexical_dispersion
#' @export
plot.lexical_dispersion <- function(x, color = "blue", bg.color = "grey90", 
    horiz.color = "grey85", total.color = "black", symbol = "|", 
    title = "Lexical Dispersion Plot", rev.factor = TRUE, wrap = "'", 
    xlab = "Dialogue (Words)", ylab = NULL, size = 4, scales="free", 
    space="free", ...){

    grouping <- term <- NULL
    tlvls <- paste0(" ", wrap, attributes(x)[['terms']], wrap)
    
    #if (!isTRUE(x)) return(NULL)
    attrs <- attributes(x)
    
    grps <- attributes(x)[["groupings"]]
    if (is.null(ylab)) {
        if (is.null(grps)) ylab <- "All" else ylab <- paste(simpleCap(grps), collapse = " & ")
    }

    if (!is.null(grps)) {
        mygrps <- parse(text=sprintf("paste(%s, sep=\".\")", paste(grps, collapse=", ")))
        suppressWarnings(x[, 'grouping' := eval(mygrps)])

        x[, 'grouping' := factor(grouping, levels = sort(unique(grouping), rev.factor))]
    } else {
        suppressWarnings(x[, 'grouping' := "All"])
    }

    ## Add totals if total.color != NULL
    if (!is.null(total.color) && !is.null(grps)) {
        x[, 'sub' := 'All']
        lvls <- levels(x[["grouping"]])
        x <-  data.table::melt(x, measure.vars = c("grouping", "sub"),
            variable.name = "summary", value.name ="grouping")[,
            'grouping' := factor(grouping, levels = c("All", lvls))]
        cols <- c(color, total.color)
    } else {
        x[, 'summary' := "grouping"]
        cols <- color
    }
 # browser()
    ## Add term wrapping
    # x <- x[, "term" := paste0(" ", wrap, term, wrap)]
    x <- x[, "term" := factor(paste0(" ", wrap, term, wrap), levels = tlvls)]

    summary <- NULL

    the_plot <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = 'word_id', y = 'grouping')) +
        ggplot2::geom_point(ggplot2::aes(color = summary),
            shape = symbol, size = size) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg.color),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = horiz.color),
            strip.text.y = ggplot2::element_text(angle=0, hjust = 0),
            strip.background = ggplot2::element_blank()) +
        ggplot2::ylab(ylab) +
        ggplot2::xlab(xlab) +
        ggplot2::ggtitle(title) +
        ggplot2::scale_colour_manual(values = cols, guide=FALSE)

    if(is.null(attrs[["timings"]])) {
        the_plot <- the_plot + ggplot2::facet_grid(stats::reformulate(".", 'term'), scales=scales, space=space)
    } else {
        the_plot <- the_plot + ggplot2::facet_grid(stats::reformulate(attrs[["timings"]], 'term'), scales=scales, space=space)
    }
    if (is.null(attrs[["groupings"]])){
        the_plot <- the_plot +
            ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank())
    }

    the_plot  +
        scale_x_continuous(labels = numform::ff_denom())
}


#' @importFrom data.table :=
locate_terms <- function(text.var, term.list, ignore.case = TRUE, along = NULL, ...){

    text <- word_id <- words <- NULL

    if (is.null(names(term.list))) names(term.list) <- rep("", length(term.list))

    if(!is.list(term.list)) {
        names(term.list)[names(term.list) == ""] <- term.list[names(term.list) == ""]
    } else {
        noname <- names(term.list) == ""
        single <- sapply(term.list, length) == 1
        names(term.list)[noname & single] <- term.list[noname & single]
    }

    if (!is.list(term.list)) term.list <- as.list(term.list)

    terms_unlisted <- unlist(term.list)
    sub_outs <- stringi::stri_detect_regex(terms_unlisted, "[:alnum:]\\s+[:alnum:]")
    if (sum(sub_outs) > 0){
        sub_outs <- terms_unlisted[sub_outs]
        sub_ins <- gsub("(?<!^)\\s+(?!$)", "dispholderdisp", sub_outs, perl=TRUE)

        for (i in seq_len(length(sub_outs))){
            text.var <- gsub(sub_outs[i], sub_ins[i], text.var , ignore.case = ignore.case)
        }

    }

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
        identical, "")])))
    term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))
    term.list <- lapply(term.list, function(x) gsub("(?<=\\()\\s+|\\s+(?=\\))", "\\\\b", x, perl=TRUE))
    term.list <- lapply(term.list, function(x) gsub("\\s+", "dispholderdisp", x))

    dat <- data.table::data.table(text=text.var, along)
    dat <- dat[, list(words = stringi::stri_extract_all_words(text)), by = names(along)][,
        list(words = unlist(words)), by = names(along)][, word_id := 1:.N, by = 'time_id']

    term_names <- names(term.list)
    dat[, (term_names) := lapply(term.list, function(x) stringi::stri_detect_regex(words,
        x, case_insensitive=ignore.case))][, words := NULL]

    dat <- data.table::melt(dat, 1:3, 4:ncol(dat), "term")
    data.table::setkey(dat, 'time_id', 'row_id', 'word_id')

    class(dat) <- unique(c("locate_terms", class(dat)))

    dat
}


#' #' Prints a lexical_dispersion Object
#' #'
#' #' Prints a lexical_dispersion object.
#' #'
#' #' @param x lexical_dispersion object.
#' #' @param plot logical.  If \code{TRUE} the plot will automatically plot.
#' #' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' #' to add additional plot layers.
#' #' @param \ldots Ignored.
#' #' @method print lexical_dispersion
#' #' @export
#' print.lexical_dispersion <- function(x, plot = TRUE, ...){
#'
#'     if (plot) {
#'         print(plot(x, ...))
#'     } else {
#'         class(x) <- class(x)[!class(x) %in% c("lexical_dispersion", "locate_terms")]
#'         print(x)
#'     }
#'
#' }
#'
#'
