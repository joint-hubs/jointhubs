#' Season Summary
#'
#' Function to summarise preprocessed timeseries.
#' @param table preprocessed data
#' @keywords data processing
#' @return Returns summarised table
#'
#' @examples
#'
#' clean <- table %>%
#'  jtClean(
#'   date = input$data,
#'   sales = input$sales,
#'   product = FALSE,
#'   min_date,
#'   max_date
#'   )
#'
#' totals <- clean %>%
#'  jtTotals()
#'
#' seasons <- totals %>%
#'  jtSeasons()
#'
#' @export
#'

jtSeasons <- function(table){
  table <- table %>%
    group_by(year) %>%
    mutate(year_mean = mean(Sprzedaz, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(month.lbl) %>%
    summarise( Periods = n(),
               `Total Sales` = sum(Sprzedaz, na.rm = TRUE),
               Change = round(mean((Sprzedaz / year_mean - 1),
                                   na.rm = TRUE)*100, 2),
               .groups = "drop") %>%
    rename(month = month.lbl)

  return(table)
}
