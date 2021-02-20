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
