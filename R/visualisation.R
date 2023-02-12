#' Custom ggplot2 theme
#'
#' @export
theme_sap <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_line(colour = "grey", linetype = "dotted"),
      axis.line = element_line(colour = "black"),
      text = element_text(size = 12, colour = "black"),
      axis.text = element_text(colour = "black")
    )
}
