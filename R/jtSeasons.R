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
