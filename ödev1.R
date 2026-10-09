# 1. Gerekli k??t??phaneyi y??kleme
if (!require("readxl")) install.packages("readxl")

# 2. Esnek ve Hata Toleransl?? Optimum Moving Average Fonksiyonu
find_optimal_ma_from_excel <- function(file_path, column_name = "Engine Failures", metric = "MAD", max_n = 10, skip_rows = 2) {
  
  # Excel dosyas??n?? ba??l??k sat??r??n?? atlayarak oku (skip = 2 ile 3. sat??rdaki ba??l??klar?? al??r)
  df <- readxl::read_excel(file_path, skip = skip_rows)
  
  # S??tun ismini bulma (k??????k/b??y??k harf veya bo??luk hatalar??n?? tolera eder)
  matched_col <- names(df)[trimws(tolower(names(df))) == trimws(tolower(column_name))]
  
  if (length(matched_col) == 0) {
    # E??er ba??l??k atlamas??z do??rudan 1. sat??rda ise bir de ??yle dener
    df <- readxl::read_excel(file_path)
    matched_col <- names(df)[trimws(tolower(names(df))) == trimws(tolower(column_name))]
  }
  
  if (length(matched_col) == 0) {
    stop(paste("Hata:", column_name, "isimli s??tun Excel dosyas??nda bulunamad??! Mevcut s??tunlar:", paste(names(df), collapse = ", ")))
  }
  
  data <- as.numeric(df[[matched_col[1]]])
  data <- data[!is.na(data)] # Say??sal olmayan veya bo?? de??erleri temizler
  N <- length(data)
  
  best_n <- 2
  best_error <- Inf
  results_summary <- data.frame(n = integer(), MAD = numeric(), MSE = numeric(), MAPE = numeric())
  
  max_n <- min(max_n, floor(N / 2))
  
  for (n in 2:max_n) {
    forecasts <- rep(NA, N)
    errors <- rep(NA, N)
    
    for (t in (n + 1):N) {
      forecasts[t] <- mean(data[(t - n):(t - 1)])
      errors[t] <- data[t] - forecasts[t]
    }
    
    valid_errors <- errors[(n + 1):N]
    valid_actuals <- data[(n + 1):N]
    
    mad_val <- mean(abs(valid_errors))
    mse_val <- mean(valid_errors^2)
    mape_val <- mean(abs(valid_errors / valid_actuals)) * 100
    
    results_summary <- rbind(results_summary, data.frame(n = n, MAD = mad_val, MSE = mse_val, MAPE = mape_val))
    
    current_error <- switch(metric, "MAD" = mad_val, "MSE" = mse_val, "MAPE" = mape_val)
    
    if (current_error < best_error) {
      best_error <- current_error
      best_n <- n
    }
  }
  
  best_forecasts <- rep(NA, N)
  best_errors <- rep(NA, N)
  for (t in (best_n + 1):N) {
    best_forecasts[t] <- mean(data[(t - best_n):(t - 1)])
    best_errors[t] <- data[t] - best_forecasts[t]
  }
  
  output_df <- data.frame(
    Period = 1:N,
    Actual = data,
    Forecast_MA = round(best_forecasts, 2),
    Error = round(best_errors, 2)
  )
  
  return(list(
    optimal_n = best_n,
    selected_metric = metric,
    min_error_value = round(best_error, 2),
    all_metrics_table = results_summary,
    forecast_table = output_df
  ))
}

# 3. Kodu ??al????t??rma
sonuc <- find_optimal_ma_from_excel(
  file_path = "engine_failures_data.xlsx",
  column_name = "Engine Failures",
  metric = "MAD",
  max_n = 5
)

# 4. Sonu??lar?? Konsola Yazd??rma
print(paste("En uygun n de??eri:", sonuc$optimal_n))
print(sonuc$forecast_table)