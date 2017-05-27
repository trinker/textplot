# map <- list(
#     `#FF69B4` = 'think',
#     `#7CFC00` = c('he is', "he's", 'you(\'[vr]e|\\b)'),
#     red = 'it'
# )
#
# map2 <- structure(list(color = c("#FF69B4", "#7CFC00", "#7CFC00", "#7CFC00"
# ), mark = c("think", "he is", "he's", "you('[vr]e|\\b)")), .Names = c("color",
# "mark"), row.names = c(NA, -4L), class = c("data.frame"
# ))
#
# check_map(map)
# check_map(map2)
check_map <- function(mark_map){

    color <- NULL

    if (is.list(mark_map) && !is.data.frame(mark_map)){
        mark_map <- dplyr::data_frame(
            color = rep(names(mark_map), lengths(mark_map)),
            mark = unlist(mark_map)
        )
    }

    stopifnot(all(c('color', 'mark') %in% names(mark_map)))
    if(anyDuplicated(mark_map[['mark']])) stop('Duplicate marks found.')
    mark_map <- mark_map[, c('color', 'mark')]
    key <- dplyr::data_frame(
        color = unique(mark_map[['color']]),
        class = text_id(length(color))
    )

    out <- dplyr::left_join(mark_map, key, by = 'color')
    out[['color']] <- assert_hex(out[['color']])
    out

}


## collapse_map(check_map(map))
collapse_map <- function(map){

    mark <- NULL

    data.table::data.table(map)[, `mark` := paste0('(', mark, ')')][,
        list(mark = paste(mark, collapse = "|")),
        by = c('color', 'class')][, c('color', 'mark', 'class'), with = FALSE]

}

## generate unique letter based IDs
text_id <- function(n) {


    z <- stringi::stri_rand_strings(n, nchar(n) + 20, '[a-z]')

    while(length(z) != length(unique(z))){
       z[duplicated(z)]  <- stringi::stri_rand_strings(length(z[duplicated(z)]), nchar(n) + 20, '[a-z]')
    }

    z
}



.mgsub <- function (pattern, replacement, text.var, fixed = TRUE,
	order.pattern = fixed, perl = TRUE, ...) {

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, perl = perl, ...)
    }

    text.var
}

mark_start <- function(class){
    sprintf("<mark class=\"%s\">", class)
}

mark_end <- "</mark>"

# x <- c("This is software testing: looking for (word) pairs the dog!",
# "peas and carrots  \n         This [is] a software testing again.",
# "For.", "the dog want the book peas and carrots", "Here: this is more Software Testing, want the book looking again for word pairs.",
# "peas and carrots")
# pattern <- setNames(c('this is', 'peas\\b', ':'), c('fgh', 'sdr', 'ppp'))
# mclass (x, pattern)
# mclass (x, pattern, first.appearance = FALSE)
#' @importFrom data.table :=
mclass <- function (x, pattern, first.appearance = TRUE, ignore.case = TRUE, ...) {

    DF <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)

    if (any(!nzchar(pattern))) {
        good_apples <- which(nzchar(pattern))
        pattern <- pattern[good_apples]
        replacement <- replacement[good_apples]
        warning("Empty pattern found (i.e., `pattern = \"\"`).\nThis pattern and replacement have been removed.")
    }

    hits <- data.table::copy(data.table::setDT(DF))[,
        `:=`(names(pattern), lapply(pattern, function(y) stringi::stri_detect_regex(x, y,
            opts_regex = stringi::stri_opts_regex(case_insensitive = ignore.case)))), ][,
        `:=`("x", NULL)][]

    if (!isTRUE(first.appearance)){
        hits <- hits[,rev(colnames(hits)), with = FALSE]
    }

    nms <- colnames(hits)
    unlist(lapply(apply(hits, 1, function(x) nms[x]), `[`, 1), use.names = FALSE)

}



## Helper function to capitalize
simpleCap <- function(x) {
    x <- gsub("(\\w)(\\w*)","\\U\\1\\L\\2", x, perl=TRUE)
    .mgsub(c("And", "Of"), c("and", "of"), x, perl = FALSE)
}

is.list_o_vectors <- function (x) {
    is.list(x) && !is.data.frame(x) && all(sapply(x, is.vector))
}


list_namer <- function (x) {
    nms <- names(x)
    if (is.null(nms))
        nms <- rep("", length(x))
    blanks <- nms == ""
    if (sum(blanks) == 0)
        return(x)
    singles <- sapply(x, length) == 1
    names(x)[blanks & singles] <- as.character(x[blanks & singles])
    blanks[blanks & singles] <- FALSE
    left_overs <- !singles & blanks
    if (sum(left_overs) != 0) {
        newnms <- paste0("X", 1:sum(left_overs))
        looptime <- 1
        while (newnms %in% names(x)) {
            newnms[newnms %in% names(x)] <- paste(newnms[newnms %in%
                names(x)], looptime, sep = ".")
            looptime <- 1 + 1
        }
        names(x)[left_overs] <- newnms
    }
    x
}
