#' Lotto data collection and processing
#'
#' Data collected from http://www.mbnet.com.pl/dl.txt
#'


jtGetLotto <- function(path = "http://www.mbnet.com.pl/dl.txt"){

  data <- read.table(path,
                     sep = " ", stringsAsFactors = FALSE) %>%
    setNames(c("id", "date", "numbers")) %>%
    mutate(date = as.Date(paste(
      substr(date, 7, str_length(date)),
      substr(date, 4, 5),
      substr(date, 1, 2),
      sep = "-"))) %>%
    separate(numbers, paste0("number", 1:6),
             sep = ",")

  message("Data collected from: ", path)

  data <- data %>%
    mutate_at(vars(-date), as.numeric) %>%
    mutate(sum = round(number1 + number2 + number3 + number4 +
                         number5 + number6),
           sum_of_squares =  round(number1^2 + number2^2 + number3^2 +
                                     number4^2 + number5^2 + number6^2)) %>%
    mutate(distance_index = round(sum_of_squares / sum, 2))

  helper <- list()
  helper$prob_tables <- data %>%
    select(number1,
           number2,
           number3,
           number4,
           number5,
           number6) %>%
    pivot_longer(everything()) %>%
    count(value) %>%
    rename(Number = value,
           `Times selected` = n) %>%
    mutate(Probabilty = round(`Times selected`/sum(`Times selected`)*100,4))

  return(list(
    data = data,
    helper = helper
  ))
}

jtGetDesiredLotto <- function(table){
  data <- list()
  data$clean_filtered <- table %>%
    filter(distance_index >= quantile(distance_index, .2) & distance_index <= quantile(distance_index, .8)) %>%
    filter(sum_of_squares >= quantile(sum_of_squares, .2) & sum_of_squares <= quantile(sum_of_squares, .8)) %>%
    filter(sum >= quantile(sum, .2) & sum <= quantile(sum, .8))

  data$random_pos1 <- quantile(data$clean_filtered$number1, .8)
  data$random_pos2 <- quantile(data$clean_filtered$number2, .8)
  data$random_pos3 <- quantile(data$clean_filtered$number3, .8)
  data$random_pos4 <- quantile(data$clean_filtered$number4, .2)
  data$random_pos5 <- quantile(data$clean_filtered$number5, .2)
  data$random_pos6 <- quantile(data$clean_filtered$number6, .2)

  data$clean_filtered_ids <- data$clean_filtered %>%
    filter(number1 <= data$random_pos1,
           number2 <= data$random_pos2,
           number3 <= data$random_pos3,
           number4 >= data$random_pos4,
           number5 >= data$random_pos5,
           number6 >= data$random_pos6) %>%
    pull(id)

  return(data$clean_filtered_ids)
}

jtEnhanceLotto <- function(table){

  model <- list()
  model$raw <- table %>%
    select(-id)

  model$raw[nrow(model$raw)+1,] <- NA
  model$raw <- model$raw %>%
    mutate(date = ifelse(!is.na(date),
                         date,
                         as.Date("2021-02-11")),
           date = anytime::anydate(date))

  model$data_full <- model$raw %>%
    tk_augment_timeseries_signature(.date_var = date) %>%
    select(-half, -hour, -minute, -second, -hour12, -am.pm, -week2, -mday, -week.iso,
           -wday.xts, -qday, -yday, -index.num, -year.iso, -month.xts,
           -wday.lbl, -month.lbl, -date, -diff)

  model$data <- model$data_full %>%
    filter(complete.cases(.))

  return(model)
}

predictLotto <- function(data){


  model1 <- data %>%
    select(-number2, -number3, -number4,
           -number5, -number6)
  result1 <- jtXGBoost.lotto(model1, "number1")$forecasts

  message(paste("Number 1 predicted", result1$number1))

  model2 <- data %>%
    select(-number3, -number4,
           -number5, -number6) %>%
    mutate(number1 = ifelse(is.na(number1),
                            result1$number1,
                            number1))
  result2 <- jtXGBoost.lotto(model2, "number2")$forecasts
  message(paste("Number 2 predicted", result2$number2))

  model3 <- data %>%
    select(-number4,
           -number5, -number6) %>%
    mutate(number1 = ifelse(is.na(number1),
                            result1$number1,
                            number1),
           number2 = ifelse(is.na(number2),
                            result2$number2,
                            number2))
  result3 <- jtXGBoost.lotto(model3, "number3")$forecasts
  message(paste("Number 3 predicted", result3$number3))

  model4 <- data %>%
    select(-number5, -number6) %>%
    mutate(number1 = ifelse(is.na(number1),
                            result1$number1,
                            number1),
           number2 = ifelse(is.na(number2),
                            result2$number2,
                            number2),
           number3 = ifelse(is.na(number3),
                            result3$number3,
                            number3))
  result4 <- jtXGBoost.lotto(model4, "number4")$forecasts
  message(paste("Number 4 predicted", result4$number4))

  model5 <- data %>%
    select(-number6) %>%
    mutate(number1 = ifelse(is.na(number1),
                            result1$number1,
                            number1),
           number2 = ifelse(is.na(number2),
                            result2$number2,
                            number2),
           number3 = ifelse(is.na(number3),
                            result3$number3,
                            number3),
           number4 = ifelse(is.na(number4),
                            result4$number4,
                            number4))
  result5 <- jtXGBoost.lotto(model5, "number5")$forecasts
  message(paste("Number 5 predicted", result5$number5))

  model6 <- data %>%
    mutate(number1 = ifelse(is.na(number1),
                            result1$number1,
                            number1),
           number2 = ifelse(is.na(number2),
                            result2$number2,
                            number2),
           number3 = ifelse(is.na(number3),
                            result3$number3,
                            number3),
           number4 = ifelse(is.na(number4),
                            result4$number4,
                            number4),
           number5 = ifelse(is.na(number5),
                            result5$number5,
                            number5))
  result6 <- jtXGBoost.lotto(model6, "number6")$forecasts %>%
    mutate(number6 = ifelse(number6 > 49, 49, number6))
  message(paste("Number 6 predicted", result6$number6))

  return(result6)

}



