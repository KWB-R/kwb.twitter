#' plot_timelines_per_tweeter
#'
#' @param timelines as retrieved by rtweet::get_timelines()
#' @return plot tweet timelines per user
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' 
plot_timelines_per_tweeter <- function(timelines) {

timelines %>% 
  dplyr::mutate(year = lubridate::year(.data$created_at)) %>% 
  ggplot2::ggplot(ggplot2::aes(col =.data$screen_name)) + 
  ggplot2::geom_point(ggplot2::aes(x = .data$created_at, 
                                   y = .data$screen_name)) +
  ggplot2::labs(x = "Date", y = "Twitter_Name", col = "Twitter_Name") +
  ggplot2::facet_wrap(~ .data$year, scales = "free_x", ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "left",
                 axis.title.y=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks.y=ggplot2::element_blank()) 
  
}