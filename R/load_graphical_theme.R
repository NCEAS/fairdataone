# Load modified ADC theme for ggplot2


#' Load modified ADC theme for ggplot2
#'
#' @return theme for ggplot
#' @import ggplot2
#' @export
#'
#' @examples
#' theme_ADC_modified <- load_graphical_theme()
load_graphical_theme <- function() {

  library(ggplot2)

  theme_ADC_modified <-
    theme_bw(base_size = 12, base_family = "Helvetica") +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        margin = margin(10, 0, 10, 0),
        color = "#1D244F"
      ),
      plot.subtitle = element_text(
        size = 14,
        margin = margin(0, 0, 10, 0),
        color = "#1D244F"
      ),
      axis.text.x = element_text(
        color = "#1D244F"
      ),
      axis.text.y = element_text(size = 18, color = "#1D244F"),
      axis.title.x = element_text(
        color = "#1D244F",
        size = 20
      ),
      axis.title.y = element_text(
        color = "#1D244F",
        angle = 90,
        size = 20
      ),
      panel.background = element_rect(fill = "white"),
      axis.line = element_line(color = "#1D244F"),
      panel.grid.major = element_line(colour = "gray20", size = 0.05),
      panel.grid.minor = element_line(colour = "gray20", size = 0.05),
    ) +
    theme(legend.position='top',
          legend.justification='center',
          legend.direction='horizontal',
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.title = element_text(size=15),
          legend.text = element_text(size=14))

  return(theme_ADC_modified)

}

