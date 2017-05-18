#' Word Trees
#'
#' A port of Google Chart's word tree \url{https://developers.google.com/chart/interactive/docs/gallery/wordtree}.
#'
#' @param text.var The text string variable.
#' @param word A character string with the word to search for.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.  If \code{TRUE} an \code{id} variable is
#' used with a \code{seq_along} the \code{text.var}.
#' @param path A path ending in .html to output the plot to.
#' @param open logical.  If \code{TRUE} the output .html file is opened via
#' \code{\link[utils]{browseURL}}.
#' @param width The width of the plot in px.
#' @param height The height of the plot in px.
#' @param ignore.case logical.  If \code{FALSE}, the pattern initial matching is
#' case sensitive and if \code{TRUE}, case is ignored during initial matching.
#' @param \ldots ignored.
#' @return Makes a .html file and returns the path invisibly.
#' @references \url{https://developers.google.com/chart/interactive/docs/gallery/wordtree}
#' @keywords wordtree
#' @export
#' @examples
#' \dontrun{
#' word_tree(sam_i_am, word = 'I', open = TRUE)
#' word_tree(sam_i_am, word = 'do', open = TRUE)
#' word_tree(sam_i_am, word = 'not', open = TRUE)
#'
#' with(presidential_debates_2012, word_tree(dialogue, word = 'I',
#'     list(person, time), open = TRUE))
#' with(presidential_debates_2012, word_tree(dialogue, word = 'america', l
#'     ist(person, time), open = TRUE, path = 'treemap.html'))
#' with(presidential_debates_2012, word_tree(dialogue, word = 'He',
#'     list(person, time), open = TRUE, path = 'treemap.html'))
#' with(presidential_debates_2012, word_tree(dialogue, word = 'we',
#'     list(person, time), open = TRUE, path = 'treemap.html', height = 1200))
#' with(presidential_debates_2012, word_tree(dialogue, word = 'our',
#'     list(person, time), open = TRUE, path = 'treemap.html', height = 1200))
#' }
word_tree <- function(text.var, word, grouping.var = NULL,
    path = file.path(tempdir(), 'treemap.html'), open = TRUE, width = 900,
    height = 600, ignore.case = TRUE, ...){

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


    ## split text by group var
    txts <- split(text.var, grouping)
    txts <- txts[sapply(txts, length) != 0]

    ## get elements for parts
    js <- lapply(txts, function(x) word_tree_helper(x, word, js_script(), ignore.case = , ignore.case))
    ids <- stringi::stri_pad_left(seq_along(js), 10, '0')
    titles <- names(js)

    ## construct parts
    parts <- Map(function(js, id, title, width, height) {tree_map_parts(js, id, title, width, height)}, js, ids, titles, width, height)

    parts_com <- list(js = NA, div = NA)
    parts_com[['js']] <- paste(unlist(lapply(parts, '[[', 1)), collapse = "\n")
    parts_com[['div']] <- paste(unlist(lapply(parts, '[[', 2)), collapse = "\n")


    ## make the file
    cat(contruct_tree_map(parts_com), file = path)
    if (open) utils::browseURL(path)

    class(path) <- c('textplot', 'character')
    return(invisible(path))
}


word_tree_helper <- function(text.var, word, script, ignore.case = TRUE, ...){

    wrd <- paste0('\\b', word, '\\b')
    dat <- text.var[stringi::stri_detect_regex(stats::na.omit(text.var), wrd, opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))]

    if (length(dat) == 0) return(NULL)
    strings <- stringi::stri_replace_all_regex(dat, paste0('^.*\\b', word, '\\b'), word, opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
    strings <- stringi::stri_replace_all_regex(trimws(gsub('[^a-z\']+', ' ', tolower(strings))), paste0('(^', tolower(word), ")('.*$)"), "$1 $2")
    strings <- stringi::stri_replace_all_fixed(strings, "'", "\\'")
    script[script == 'DATAHERE'] <- paste(sprintf("        ['%s'],", strings), collapse = "\n")

    gsub('WORD_HERE', tolower(word), paste(script, collapse = '\n'))
}


tree_map_parts <- function(js, id, title = "", width = 900, height = 600){

    js <- gsub('ID_HERE', id, js)
    if (title != "") title <- paste0('<h3>', title, '</h3>\n')
    div <- paste0(title, '    <div id="', id, '" style="width: ', width, "px; height: ", height, 'px;"></div>')
    list(js = js, div = div)
}


contruct_tree_map <- function(parts, html = html_script(), ...){

    html[html == "JS_SCRIPT_HERE"] <- paste(parts[['js']], collapse = "\n")
    html[html == "DIV_TAGS_HERE" ] <- paste(parts[['div']], collapse = "\n")

    out <- paste(html, collapse = "\n")
    class(out) <- 'tree_map'
    out
}


js_script <- function() {
    c(
        "   <script type=\"text/javascript\">", "      google.charts.load('current', {packages:['wordtree']});",
        "      google.charts.setOnLoadCallback(drawChart);", "", "      function drawChart() {",
        "        var data = google.visualization.arrayToDataTable(",
        "          [ ['Phrases'],", "DATAHERE", "          ]", "        );",
        "", "        var options = {", "          wordtree: {", "            format: 'implicit',",
        "            word: 'WORD_HERE'", "          }", "        };", "",
        "        var chart = new google.visualization.WordTree(document.getElementById('ID_HERE'));",
        "        chart.draw(data, options);", "      }", "    </script>"
    )
}

html_script <- function(){
    c(
        "<html>", "  <head>",
        "    <script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>",
        "JS_SCRIPT_HERE",
        "  </head>", "  <body>", "DIV_TAGS_HERE", "  </body>", "</html>"
    )
}

