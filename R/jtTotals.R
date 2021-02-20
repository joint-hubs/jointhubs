#' Totals calculation
#'
#' Function to avoid aggregation if multiple columns are provided
#' @param clean preprocessed data
#' @keywords data processing
#' @return Returns table with additional fixed sales value
#'
#' @examples
#'
#' clean <- table %>%
#' jtClean(
#'  date = input$data,
#'  sales = input$sales,
#'  product = FALSE,
#'  min_date,
#'  max_date
#'  )
#'
#'  totals <- clean %>%
#'  jtTotals()
#'
#' @export
#'

jtTotals <- function(clean){
  table <- clean %>%
    group_by(Data) %>%
    summarise(Sprzedaz = sum(Sprzedaz, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(Data) %>%
    mutate(Sprzedaz = ifelse(Sprzedaz <= 0, .00001, Sprzedaz)) %>%
    mutate(`Change` = round((Sprzedaz / lag(Sprzedaz) - 1) *100, 2)) %>%
    jtEnhancets() %>%
    timetk::tk_augment_timeseries_signature(.date_var = Data)

  jtMice <- mice(table,
                 m=2,
                 maxit=3,
                 meth='cart')

  table <- complete(jtMice)

  return(table)
}
