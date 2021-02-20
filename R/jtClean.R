#' Feature labelling
#'
#' Function to set data structure
#' @param table whole data
#' @param date string with the original name of the column containing dates
#' @param date string with the original name of the column containing sales
#' @param date string with the original name of the column containing product names
#' @keywords data collection
#' @return Returns table with proper data structure
#'

jtClean <- function(table, date, sales, product, min_date, max_date){

  if(!product){
    table <- table %>%
      rename(Data = parse_character(date),
             Sprzedaz = parse_character(sales)) %>%
      mutate(Data = anydate(Data)) %>%
      filter(Data >= anydate(min_date),
             Data <= anydate(max_date))
  }else{
    table <- table %>%
      rename(Data = parse_character(date),
             Sprzedaz = parse_character(sales),
             Product= parse_character(product)) %>%
      mutate(Data = anydate(Data)) %>%
      filter(Data >= anydate(min_date),
             Data <= anydate(max_date))
  }

  return(table)
}
