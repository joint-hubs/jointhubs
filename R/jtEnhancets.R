#' Enhancing timeseries
#'
#' Function to create multiple features using past values.
#' @param table whole data
#' @param sales name of the column containing sales values
#' @keywords data processing
#' @return Returns table with additional columns
#' @export
#'

jtEnhancets <- function(table, sales = "Sprzedaz"){

  table <- table %>%
    rename(Sprzedaz = parse_character(sales)) %>%
    mutate(lower1 = lag(Sprzedaz, n = 3) - lag(Sprzedaz, n = 1),
           lower2 = lag(Sprzedaz, n = 7) - lag(Sprzedaz, n = 3),
           higher1 = lag(Sprzedaz, n = 1) - lag(Sprzedaz, n = 3),
           higher2 = lag(Sprzedaz, n = 3) - lag(Sprzedaz, n = 7),
           h1 = lag(Sprzedaz) + higher1,
           l1 = lag(Sprzedaz) + lower1,
           h2 = lag(Sprzedaz) + higher2,
           l2 = lag(Sprzedaz) + lower2,
           explanator_h1l2 = round(sqrt(h1*l2), 4),
           explanator_h1h2 = round(sqrt(h1*h2), 4),
           explanator_h1l1 = round(sqrt(h1*l1), 4),
           explanator_h1h1 = round(sqrt(h1*h1), 4),
           explanator_l1l2 = round(sqrt(l1*l2), 4),
           explanator_l1h2 = round(sqrt(l1*h2), 4),
           explanator_l1l1 = round(sqrt(l1*l1), 4)) %>%
    ungroup() %>%
    select(-lower1, -lower2, -higher1, -higher2,
           -h1, -l1, -h2, -l2)

  for (i in 2:8) {
    col_name <- paste0("avg_", str_sub(paste0(0, i),-2,-1))
    table <- table %>%
      mutate(!!col_name := dplyr::lag(RcppRoll::roll_meanr(Sprzedaz, i), 1))
  }

  return(table)
}
