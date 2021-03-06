#' Theme Set
#'
#' Function to custiomize ggplots. Dark layout
#' @param table preprocessed data
#' @keywords data visualization
#' @return Returns customized graph
#'
#' @examples
#'
#' data %>%
#' ggplot(aes(distance_index, fill = label)) +
#' geom_histogram() +
#' jtTheme()
#'
#' @export
#'

jtTheme <- function(base_size = 13,
                    base_family = "",
                    base_line_size = base_size/20,
                    base_rect_size = base_size/20) {
  theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size) +
    theme(panel.background = element_rect(fill = "#14113D",
                                          colour = "#14113D"),
          plot.background = element_rect(fill = "#14113D",
                                         colour = "#14113D"),
          panel.border = element_rect(fill = NA, colour = NA),
          panel.grid = element_line(colour = "#D9D9D9"),
          panel.grid.minor = element_line(size = rel(0.5)),
          strip.background = element_rect(fill = "#D9D9D9",
                                          colour = "#D9D9D9"),
          title = element_text(color = "329E99",
                               family = "Titillium Web"),
          legend.key = element_rect(fill = "#14113D", colour = NA),
          axis.title.x = element_text(color = "#329E99",
                                      size = base_size,
                                      family = base_family,
                                      face = "bold"),
          axis.title.y = element_text(color = "#329E99",
                                      size = base_size,
                                      family = base_family,
                                      face = "bold"),
          axis.text.x = element_text(color = "#329E99",
                                     size = base_size,
                                     family = base_family,
                                     face = "bold"),
          axis.text.y = element_text(color = "#329E99",
                                     size = base_size,
                                     family = base_family,
                                     face = "bold"),
          legend.background = element_rect(fill = "#14113D",
                                           color = "#071561"),
          legend.text = element_text(color = "#329E99"),
          complete = TRUE
    )
}
