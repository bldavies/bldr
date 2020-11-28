#' Set default ggplot2 theme
#'
#' \code{set_ggtheme} sets the default ggplot2 theme to match my preferences.
#'
#' @export
set_ggtheme <- function() {
  ggplot2::theme_set(
    ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          face = "bold"
        ),
        plot.subtitle = ggplot2::element_text(
          margin = ggplot2::margin(b = 10)
        ),
        strip.text = ggplot2::element_text(
          face = "bold",
          hjust = 0,
          margin = ggplot2::margin(b = 5),
          size = 11
        )
      )
  )
}
