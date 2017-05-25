#' Palettes and Palette Viewing
#'
#' \code{hilighter_cols} - A palette of 10 hilighter colors.
#'
#' @rdname palette
#' @export
#' @examples
#' view_cols(hilighter_cols)
#' view_cols(c("#FFA500", "#B3B3B3", "#FFFF00", 'red', 'blue', 'black'))
hilighter_cols <- c("#FFA500", "#E9E9E9", "#FFFF00", "#FFC0CB", "#00FFFF", "#FF69B4",
    "#7FFF00", "#CDBE70", "#E066FF", "#FFD39B")

#' Palettes and Palette Viewing
#'
#' \code{view_cols} - Plots vector of colors.
#'
#' @param x A vector of colors (either R \code{colors()} or hexadecimal).
#' @return \code{view_cols} plots the colors as bars
#' @rdname palette
#' @export
view_cols <- function(x) {

    cols <- rev(assert_hex(x))

    cols <- factor(cols, levels=cols)

    ggplot2::ggplot(data.frame(x = cols, y = 1), ggplot2::aes(x=cols, y=y, fill=cols)) +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::scale_fill_manual(values = as.character(cols)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            legend.position='none'
        ) +
        ggplot2::labs(y = NULL, x=NULL)


}

#' Palettes and Palette Viewing
#'
#' \code{assert_hex} - Convert colors to hexadecimal.
#'
#' @return \code{assert_hex} returns a vector of hexadecimal strings
#' @rdname palette
#' @export
assert_hex <- function(x){
    unname(unlist(lapply(x, function(y) if(grepl('#([0-9a-fA-F]{3}){1,2}', y)) { y } else { col2hex(y) })))
}


col2hex <- function(x){
    if (!all(x %in% grDevices::colors())) stop('colors must be either hexadecimal or r colors; see `colors()`')
    m <- col2rgb(x, alpha = FALSE)
    rgb(m[1,], m[2,], m[3,], maxColorValue=255)
}




