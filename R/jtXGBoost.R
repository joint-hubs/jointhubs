jtXGBoost <- function(clean){
  #' Function to run XGBoost for preprocessed sales data.
  #'  Using `xgboost` and `mice`
  #'
  #' @param table preprocessed data
  #'
  #' @return Returns table with predictions.
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
  #' xgbData <- clean %>%
  #' jtXGBoost()
  #'
  #' @export
  #'

    h <- 12

  set.seed(777)
  xgbData <- list()
  xgbData$df_test_complete <- clean
  xgbData$seasons <- clean %>%
    jtTotals() %>%
    jtSeasons() %>%
    select(month, `Change`) %>%
    clean_names()

  df <- xgbData$df_test_complete

  # add forecast rows (empty for now)

  df <- df %>%
    dplyr::mutate(horizon = "actual") %>%
    arrange(desc(Data))

  df1 <- df %>%
    group_by(Data, horizon) %>%
    summarise(Sprzedaz = sum(Sprzedaz, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame()
  df1[nrow(df1)+h,] <- NA

  diffs <- abs(df[1,"Data"] - df[2,"Data"])

  df1[nrow(df1)+h, "Data"] <- max(df$Data) + h*diffs

  df2 <- df1 %>%
    mutate(Data = na.approx(Data))%>%
    mutate(Data = as.Date(Data, format = "%Y-%m-%d")) %>%
    mutate(horizon = ifelse(is.na(horizon),
                            "future", horizon)) %>%
    jtEnhancets() %>%
    timetk::tk_augment_timeseries_signature(.date_var = Data) %>%
    left_join(xgbData$seasons,
              by = c("month.lbl" = "month"))

  xgbData$full_data <- df2 %>%
    dplyr::select(Data, Sprzedaz, explanator_h1l1,
                  explanator_h1h2, #explanator_h1l2,
                  explanator_h1h1, #explanator_l1l2,
                  #explanator_l1h2, explanator_l1l1,
                  avg_02, avg_03, avg_05, half,
                  quarter, month, week, horizon,
                  change)

  xgbData$train_base <- xgbData$full_data %>%
    dplyr::select(-horizon) %>%
    filter(complete.cases(.))
  message("xgbData prepared.")

  xgbData$trainMatrix <- Matrix::sparse.model.matrix(Sprzedaz ~
                                                       Data + explanator_h1l1 +
                                                       explanator_h1h2 +
                                                       #explanator_h1l2 +
                                                       explanator_h1h1 +
                                                       #explanator_l1l2 +
                                                       #explanator_l1h2 +
                                                       #explanator_l1l1 +
                                                       avg_03 + avg_05 +
                                                       half + quarter +
                                                       change +
                                                       week,
                                                     data = xgbData$train_base,
                                                     contrasts.arg = c("change"),
                                                     sparse = FALSE,
                                                     sci = FALSE
  )

  xgbData$label <- xgbData$train_base$Sprzedaz
  xgbData$trainDMatrix <- xgboost::xgb.DMatrix(data = xgbData$trainMatrix,
                                               label = xgbData$label)

  # prepare model
  xgbData$params <- list(booster = "gbtree",
                         objective = "reg:linear",
                         eta = 0.15,
                         gamma = 0.05)


  xgbData$xgb.tab <- xgboost::xgb.cv(data = xgbData$trainDMatrix,
                                     param = xgbData$params,
                                     maximize = FALSE,
                                     evaluation = "rmse",
                                     nrounds = 13,
                                     nthreads = 10,
                                     nfold = 2,
                                     early_stopping_round = 3)
  xgbData$num_iterations <- xgbData$xgb.tab$best_iteration

  # train model
  xgbData$model <- xgboost::xgb.train(data = xgbData$trainDMatrix,
                                      maximize = FALSE,
                                      evaluation = 'rmse',
                                      nrounds = xgbData$num_iterations)

  message("Model trained.")
  # forecast
  xgbData$forecasts <- xgbData$full_data

  for (i in which(xgbData$forecasts$horizon == 'future')){

    test_xgb <- xgbData$forecasts[i, ]

    test_xgb <- test_xgb %>%
      dplyr::select(-horizon) %>%
      mutate(Sprzedaz = 0)

    tmp_chk <- test_xgb %>%
      filter(complete.cases(.))
    if(nrow(tmp_chk) > 0){

      testMatrix <- Matrix::sparse.model.matrix(Sprzedaz ~
                                                  Data + explanator_h1l1 +
                                                  explanator_h1h2 +
                                                  #explanator_h1l2 +
                                                  explanator_h1h1 +
                                                  #explanator_l1l2 +
                                                  #explanator_l1h2 +
                                                  #explanator_l1l1 +
                                                  avg_03 + avg_05 +
                                                  half + quarter +
                                                  change +
                                                  week,
                                                data = test_xgb,
                                                contrasts.arg = c("change"),
                                                sparse = FALSE,
                                                sci = FALSE
      )
      message(i - nrow(df), " Value forecasted")

      xgbData$forecasts$Sprzedaz[i] <- predict(xgbData$model, testMatrix)

      xgbData$forecasts <- xgbData$forecasts %>%
        jtEnhancets() %>%
        dplyr::select(Data, Sprzedaz, explanator_h1l1,
                      explanator_h1h2, #explanator_h1l2,
                      explanator_h1h1, #explanator_l1l2,
                      #explanator_l1h2, explanator_l1l1,
                      avg_02, avg_03, avg_05, half,
                      quarter, change, week, horizon)
    }

  }

  return(xgbData$forecasts)
}


jtXGBoost.lotto <- function(model, dependent_var){
  #' Function to run XGBoost for preprocessed Lotto data. Allows to
  #'  use as a target `dependent_var` for formula construction. The rest of the
  #'  variables are explanatory.
  #'  Using `xgboost`, `read.csv("lotto.csv")`
  #'
  #' @param model preprocessed data with selected features
  #' @param dependent_var character with the name of the variable
  #'
  #' @return Returns table with predictions.
  #'
  #' @example jtXGBoost.lotto(model1, "number1")$forecasts
  #'
  #' @export
  #'

  lotto <- read.csv("lotto.csv")
  set.seed(sample(10000, 1))

  xgbData <- list()
  xgbData$full_data <- model
  xgbData$train_base <- model %>%
    filter(complete.cases(.)) %>%
    select(-id)

  xgbData$trainMatrix <-
    Matrix::sparse.model.matrix(
      as.formula(paste(parse_character(dependent_var), "~ .")),
      data = xgbData$train_base,
      sparse = FALSE,
      sci = FALSE
    )

  xgbData$label <- xgbData$train_base %>%
    select(parse_character(dependent_var)) %>%
    pull()

  xgbData$trainDMatrix <- xgboost::xgb.DMatrix(data = xgbData$trainMatrix,
                                               label = xgbData$label)

  # prepare model
  xgbData$params <- list(booster = "dart",
                         objective = "reg:linear",
                         eta = 0.05,
                         gamma = 0.2,
                         sampling_method = "gradient_based",
                         tree_method = "exact",
                         max_delta_step = 4,
                         max_depth = 4)


  xgbData$xgb.tab <- xgboost::xgb.cv(data = xgbData$trainDMatrix,
                                     param = xgbData$params,
                                     maximize = FALSE,
                                     evaluation = "rmse",
                                     nrounds = 7,
                                     nthreads = 3,
                                     nfold = 3,
                                     early_stopping_round = 2)
  xgbData$num_iterations <- xgbData$xgb.tab$best_iteration

  # train model
  xgbData$model <- xgboost::xgb.train(data = xgbData$trainDMatrix,
                                      maximize = FALSE,
                                      evaluation = 'rmse',
                                      nrounds = xgbData$num_iterations)

  # forecast
  xgbData$forecasts <- data.frame()

  for (i in which(is.na(xgbData$full_data$sum))){

    test_xgb <- xgbData$full_data[i, ]
    test_lotto <- lotto[sample(1:nrow(lotto), 1),]

    test_xgb <- test_xgb %>%
      select(-id) %>%
      mutate_at(vars(parse_character(dependent_var)), function(x) 0) %>%
      mutate(sum = test_lotto$sum,
             sum_of_squares = test_lotto$sum_of_squares,
             distance_index = test_lotto$distance_index)

    testMatrix <- Matrix::sparse.model.matrix(as.formula(paste(parse_character(dependent_var), "~ .")),
                                              data = test_xgb,
                                              sparse = FALSE,
                                              sci = FALSE
    )

    prediction <- predict(xgbData$model, testMatrix)

    test_xgb <- test_xgb %>%
      mutate_at(vars(parse_character(dependent_var)), function(x) round(prediction))

    xgbData$forecasts <- rbind(xgbData$forecasts, test_xgb)
  }

  return(xgbData)
}

