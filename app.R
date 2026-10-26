# Gerekli kütüphanelerin otomatik kontrolü ve yüklenmesi
required_packages <- c("shiny", "readxl", "forecast")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- USER INTERFACE (UI) ---
ui <- navbarPage(
  title = "Production Planning & Control App",
  
  tabPanel("Forecasting Module",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Data & Settings"),
               
               # 1. Dosya Yükleme (.xlsx, .xls, .csv)
               fileInput("file1", "Upload Dataset (.xlsx, .xls, .csv)",
                         accept = c(".xlsx", ".xls", ".csv")),
               
               # 2. Seri / Sütun Seçimi
               uiOutput("column_select_ui"),
               
               hr(),
               h4("Forecasting Methods"),
               
               # Yönergedeki Tüm Yöntemler
               selectInput("method_select", "Select Method:",
                           choices = c("Moving Average (MA)" = "ma",
                                       "Weighted Moving Average (WMA)" = "wma",
                                       "Simple Exponential Smoothing (SES)" = "ses",
                                       "Holt's Method (Trend)" = "holt",
                                       "Winters' Method (Seasonal)" = "winters",
                                       "ARIMA / Box-Jenkins" = "arima")),
               
               # --- Dinamik Parametre Kontrolleri ---
               conditionalPanel(
                 condition = "input.method_select == 'ma'",
                 checkboxInput("auto_ma", "Auto Search Best Period (n)", FALSE),
                 conditionalPanel(
                   condition = "input.method_select == 'ma' && input.auto_ma == FALSE",
                   sliderInput("ma_n", "Moving Average Period (n):", min = 2, max = 12, value = 3)
                 ),
                 selectInput("ma_metric", "Optimization Metric:", choices = c("MAD", "MSE", "MAPE"))
               ),
               
               conditionalPanel(
                 condition = "input.method_select == 'wma'",
                 sliderInput("wma_n", "WMA Period (n):", min = 2, max = 12, value = 3)
               ),
               
               conditionalPanel(
                 condition = "input.method_select == 'ses'",
                 sliderInput("ses_alpha", "Alpha (Smoothing Constant):", min = 0.01, max = 0.99, value = 0.2, step = 0.01)
               ),
               
               conditionalPanel(
                 condition = "input.method_select == 'holt'",
                 checkboxInput("opt_holt", "Auto Optimize Parameters (Alpha/Beta)", TRUE)
               ),
               
               conditionalPanel(
                 condition = "input.method_select == 'winters'",
                 selectInput("winters_seasonal", "Seasonal Type:", choices = c("Additive" = "additive", "Multiplicative" = "multiplicative")),
                 sliderInput("winters_freq", "Seasonal Frequency (Period Length):", min = 2, max = 24, value = 12)
               ),
               
               conditionalPanel(
                 condition = "input.method_select == 'arima'",
                 checkboxInput("auto_arima", "Use Auto ARIMA (Best Fit)", TRUE),
                 numericInput("arima_p", "AR order (p):", value = 1, min = 0, max = 5),
                 numericInput("arima_d", "Differencing (d):", value = 1, min = 0, max = 2),
                 numericInput("arima_q", "MA order (q):", value = 1, min = 0, max = 5)
               )
             ),
             
             # --- Ana Ekran ve Sekmeler ---
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Forecast Table", 
                          h3("Forecast & Error Output"),
                          tableOutput("forecast_table")),
                 tabPanel("Plot", 
                          h3("Actual vs Forecast Visualization"),
                          plotOutput("forecast_plot", height = "450px")),
                 tabPanel("Error Metrics",
                          h3("Performance Summary (MAD, MSE, MAPE)"),
                          verbatimTextOutput("metrics_summary")),
                 tabPanel("ACF / PACF (Box-Jenkins)",
                          h3("Autocorrelation & Partial Autocorrelation Plots"),
                          plotOutput("acf_pacf_plot", height = "500px"))
               )
             )
           )
  ),
  
  tabPanel("Capacity Planning", h3("Capacity planning module placeholder."))
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # 1. Dosya Okuma Reaktif Fonksiyonu
  dataset <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$datapath)
    
    df <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file1$datapath)
      } else if (ext == "csv") {
        read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    return(df)
  })
  
  # 2. Sayısal Sütun Seçim Arayüzü
  output$column_select_ui <- renderUI({
    df <- dataset()
    req(df)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("selected_column", "Select Target Series (Column):", choices = numeric_cols)
  })
  
  # 3. Kapsamlı Hesaplama Motoru
  forecast_results <- reactive({
    df <- dataset()
    req(df, input$selected_column, input$method_select)
    
    data <- as.numeric(df[[input$selected_column]])
    data <- data[!is.na(data)]
    N <- length(data)
    
    forecasts <- rep(NA, N)
    method_name <- ""
    start_idx <- 1
    
    if (input$method_select == "ma") {
      # Otomatik n arama (Optimization) veya manuel n seçimi
      if (input$auto_ma) {
        best_n <- 2
        best_score <- Inf
        max_limit <- min(10, N - 2)
        if (max_limit < 2) max_limit <- 2
        
        for (cand_n in 2:max_limit) {
          temp_f <- rep(NA, N)
          for (t in (cand_n + 1):N) {
            temp_f[t] <- mean(data[(t - cand_n):(t - 1)])
          }
          v_act <- data[(cand_n + 1):N]
          v_for <- temp_f[(cand_n + 1):N]
          errs <- v_act - v_for
          
          score <- switch(input$ma_metric,
                          "MAD" = mean(abs(errs)),
                          "MSE" = mean(errs^2),
                          "MAPE" = mean(abs(errs / v_act)) * 100)
          
          if (!is.na(score) && score < best_score) {
            best_score <- score
            best_n <- cand_n
          }
        }
        n <- best_n
      } else {
        n <- input$ma_n
      }
      
      if (n >= N) return(NULL)
      method_name <- paste0("Moving Average (n=", n, ifelse(input$auto_ma, " - Auto Optimized", ""), ")")
      for (t in (n + 1):N) {
        forecasts[t] <- mean(data[(t - n):(t - 1)])
      }
      start_idx <- n + 1
      
    } else if (input$method_select == "wma") {
      n <- input$wma_n
      if (n >= N) return(NULL)
      method_name <- paste0("Weighted MA (n=", n, ")")
      weights <- 1:n
      for (t in (n + 1):N) {
        past_vals <- data[(t - n):(t - 1)]
        forecasts[t] <- sum(past_vals * weights) / sum(weights)
      }
      start_idx <- n + 1
      
    } else if (input$method_select == "ses") {
      alpha <- input$ses_alpha
      method_name <- paste0("Simple Exp. Smoothing (alpha=", alpha, ")")
      forecasts[2] <- data[1]
      for (t in 3:N) {
        forecasts[t] <- alpha * data[t - 1] + (1 - alpha) * forecasts[t - 1]
      }
      start_idx <- 2
      
    } else if (input$method_select == "holt") {
      method_name <- "Holt's Linear Trend Method"
      ts_data <- ts(data)
      fit <- HoltWinters(ts_data, gamma = FALSE)
      fitted_vals <- fitted(fit)
      start_idx <- 2 + (length(data) - length(fitted_vals[,1]))
      forecasts[start_idx:N] <- fitted_vals[, 1]
      
    } else if (input$method_select == "winters") {
      freq <- input$winters_freq
      if (N <= 2 * freq) return(NULL)
      method_name <- "Winters' Triple Exp. Smoothing"
      ts_data <- ts(data, frequency = freq)
      fit <- HoltWinters(ts_data, seasonal = input$winters_seasonal)
      fitted_vals <- fitted(fit)
      start_idx <- length(data) - length(fitted_vals[,1]) + 1
      forecasts[start_idx:N] <- fitted_vals[, 1]
      
    } else if (input$method_select == "arima") {
      ts_data <- ts(data)
      if (input$auto_arima) {
        fit <- auto.arima(ts_data)
        method_name <- paste0("Auto ARIMA(", paste(fit$arma[c(1,6,2,3,7,4,5)], collapse=","), ")")
      } else {
        fit <- Arima(ts_data, order = c(input$arima_p, input$arima_d, input$arima_q))
        method_name <- paste0("ARIMA(", input$arima_p, ",", input$arima_d, ",", input$arima_q, ")")
      }
      fitted_vals <- fitted(fit)
      forecasts <- as.numeric(fitted_vals)
      start_idx <- 2
    }
    
    # Hata Metrikleri Hesaplama (MAD, MSE, MAPE)
    valid_actuals <- data[start_idx:N]
    valid_forecasts <- forecasts[start_idx:N]
    
    valid_idx <- which(!is.na(valid_actuals) & !is.na(valid_forecasts))
    v_act <- valid_actuals[valid_idx]
    v_for <- valid_forecasts[valid_idx]
    
    errs <- v_act - v_for
    mad_val <- mean(abs(errs))
    mse_val <- mean(errs^2)
    mape_val <- mean(abs(errs / v_act)) * 100
    
    out_df <- data.frame(
      Period = 1:N,
      Actual = data,
      Forecast = round(forecasts, 2),
      Error = round(data - forecasts, 2)
    )
    
    list(
      table = out_df,
      method = method_name,
      mad = round(mad_val, 2),
      mse = round(mse_val, 2),
      mape = round(mape_val, 2),
      raw_data = ts(data)
    )
  })
  
  # 4. Tablo Çıktısı
  output$forecast_table <- renderTable({
    res <- forecast_results()
    req(res)
    res$table
  })
  
  # 5. Hata Metrikleri Metin Çıktısı
  output$metrics_summary <- renderPrint({
    res <- forecast_results()
    req(res)
    cat("Model Performance Metrics for:", res$method, "\n")
    cat("--------------------------------------------------\n")
    cat("MAD  (Mean Absolute Deviation)     :", res$mad, "\n")
    cat("MSE  (Mean Squared Error)          :", res$mse, "\n")
    cat("MAPE (Mean Absolute Percentage Err):", res$mape, "%\n")
  })
  
  # 6. Grafik Çıktısı (Actual vs Forecast)
  output$forecast_plot <- renderPlot({
    res <- forecast_results()
    req(res)
    df <- res$table
    
    plot(df$Period, df$Actual, type = "o", col = "#1F4E78", pch = 16, lwd = 2,
         ylim = c(min(df$Actual, na.rm=T)*0.8, max(df$Actual, na.rm=T)*1.2),
         xlab = "Time Period", ylab = "Value", main = paste("Actual vs", res$method))
    lines(df$Period, df$Forecast, type = "o", col = "#D9534F", lty = 2, lwd = 2, pch = 17)
    legend("topleft", legend = c("Actual Data", "Forecast"), col = c("#1F4E78", "#D9534F"), lty = 1:2, lwd = 2, pch = c(16, 17))
  })
  
  # 7. Box-Jenkins için ACF / PACF Grafik Çıktısı
  output$acf_pacf_plot <- renderPlot({
    res <- forecast_results()
    req(res)
    par(mfrow = c(2, 1))
    Acf(res$raw_data, main = "Autocorrelation Function (ACF)")
    Pacf(res$raw_data, main = "Partial Autocorrelation Function (PACF)")
    par(mfrow = c(1, 1))
  })
}

# Uygulamayı Çalıştır
shinyApp(ui = ui, server = server)