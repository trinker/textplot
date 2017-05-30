word_cloud <- function(text.var, term.list = NULL, grouping.var = NULL,
    group.names, ignore.case = TRUE, fixed = FALSE, stopwords = NULL, 
    min.freq = 1, caps = TRUE, caps.list = NULL, random.order = FALSE, 
    rot.per = 0.0, cloud.colors = NULL, title = TRUE, cloud.font = NULL, 
    title.font = NULL, title.color = "black", title.padj = -4.5, 
    title.location = 3, title.cex = NULL, title.names = NULL,
    proportional = FALSE, max.word.size = NULL, min.word.size = 0.5,
    legend = NULL, legend.cex = .8, legend.location = c(-.03, 1.03), 
    char2space = "~~")...){
    
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
    
    ## matching term list based on term.list (fixed allows for regex matching)     
}


heat_cloud <- function(text.var, grouping.var = NULL, 
    group.names, ignore.case = TRUE, rev.binary = FALSE, X = "red", 
    Y = "blue", stem = FALSE, stopwords = NULL, caps = TRUE, caps.list = NULL, 
    I.list = TRUE, random.order = FALSE, rot.per = 0.0, min.freq = 1, 
    max.word.size = NULL, min.word.size = 0.5, breaks = 10, cloud.font = NULL, 
    title = NULL, title.font = NULL, title.color = "black", title.padj = .25, 
    title.location = 3, title.cex = NULL, legend.cex = .8, 
    legend.location = c(.025, .025, .25, .04), char2space = "~~", ...){

    
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
    
    ## check for grouping var being binary
    
    # wordcloud::wordcloud(word.freq$Words, word.freq$total, colors = word.freq$colors,
    #     min.freq = 1, ordered.colors = TRUE, random.order = FALSE, rot.per=0,
    #     scale = c(5, .7))    
    # 
    #     # Add legend
    # colfun <- colfunc(length(levels(word.freq$fill.var)))
    # plotrix::color.legend(.025, .025, .25, .04, qcv(Romney,Obama), colfun)
}



