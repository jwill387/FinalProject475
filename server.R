server <- function(input, output) {
  
  output$ts_plot <- renderPlotly(
    autoplot(aus_production)
  )
  
  
  output$user_plot <- renderPlotly({
    if (input$selected_plot == "Seasonality") {
      season <- aus_production[, c("Quarter", input$selected_var)]
      season %>%
        gg_subseries()
      
    } else if (input$selected_plot == "Autocorrelation") {
      autocor <- aus_production[, c("Quarter", input$selected_var)]
      autocor %>%
        ACF(lag_max = input$selected_lags) %>%
        autoplot()
      
    } else if (input$selected_plot == "Decomposition") {
      decomp <- aus_production[, c("Quarter", input$selected_var)]
      decomp %>%
        model(classical_decomposition()) %>%
        components() %>%
        autoplot()
    }
  })
  
  
  output$ts_custom <- renderPlotly({
    date_range <- yearquarter(input$selected_date_range)
    min_date <- date_range[1]
    max_date <- date_range[2]

    filtered <- aus_production[aus_production$Quarter >= min_date, ]
    filtered <- filtered[filtered$Quarter <= max_date, ]
    filtered <- filtered[, c("Quarter", input$selected_ts_var)]
    
    please_work <- filtered %>%
      autoplot() +
      labs(title = paste(input$selected_ts_var, "Production from", min_date, "to", max_date),
           x = "Year and Quarter")
    
    please_work <- please_work + geom_smooth(method = lm)
    
    ggplotly(please_work)
  })
  
  
  output$simple_plots <- renderPlot({
    date_range <- yearquarter(input$selected_date_range2)
    min_date <- date_range[1]
    max_date <- date_range[2]
    
    filtered <- aus_production[aus_production$Quarter >= min_date, ]
    filtered <- filtered[filtered$Quarter <= max_date, ]
    filtered <- filtered[, c("Quarter", input$aubs_selected_var)]
    
    fit <- filtered[, c("Quarter", input$aubs_selected_var)] %>% 
      model(
        Naive = NAIVE(),
        SeasonalNaive = SNAIVE(),
        Mean = MEAN(),
        Drift = RW( ~ drift())
      )
    
    fit %>% 
      select(input$selected_model) %>% 
      forecast(h = input$selected_h) %>%
      autoplot(filtered)
  })
  
  
  output$ets_plots <- renderPlot({
    date_range <- yearquarter(input$selected_date_range3)
    min_date <- date_range[1]
    max_date <- date_range[2]
    
    filtered <- aus_production[aus_production$Quarter >= min_date, ]
    filtered <- filtered[filtered$Quarter <= max_date, ]
    filtered <- filtered[, c("Quarter", input$selected_var3)]
    
    fit <- filtered[, c("Quarter", input$selected_var3)] %>% 
      model(
        Holt = ETS( ~ error("A") + trend("A") + season("N")),
        HoltWinters = ETS( ~ error("M") + trend("A") + season("M"))
      )
    
    fit %>% 
      select(input$selected_model3) %>% 
      forecast(h = input$selected_h3) %>% 
      autoplot(filtered)
  })
  
  output$manualui <- renderUI({
    if (input$selected_model4 == "ManualARIMA") {
      div(
        prettyRadioButtons(
          inputId = "p",
          label = "Select p (ManualARIMA):", 
          choices = c(0, 1, 2),
          inline = TRUE, 
          fill = TRUE
        ),
        
        prettyRadioButtons(
          inputId = "d",
          label = "Select d (ManualARIMA):", 
          choices = c(0, 1, 2),
          inline = TRUE, 
          fill = TRUE
        ),
        
        prettyRadioButtons(
          inputId = "q",
          label = "Select q (ManualARIMA):", 
          choices = c(0, 1, 2),
          inline = TRUE, 
          fill = TRUE
        ),
        
        prettyRadioButtons(
          inputId = "ps",
          label = "Select P (ManualARIMA Seasonal):", 
          choices = c(0, 1, 2),
          inline = TRUE, 
          fill = TRUE
        ),
        
        prettyRadioButtons(
          inputId = "ds",
          label = "Select D (ManualARIMA Seasonal):", 
          choices = c(0, 1),
          inline = TRUE, 
          fill = TRUE
        ),
        
        prettyRadioButtons(
          inputId = "qs",
          label = "Select Q (ManualARIMA Seasonal):", 
          choices = c(0, 1, 2),
          inline = TRUE, 
          fill = TRUE
        ),
      )
    }
  })
  
  output$arima_plots <- renderPlot({
    date_range <- yearquarter(input$selected_date_range4)
    min_date <- date_range[1]
    max_date <- date_range[2]
    
    filtered <- aus_production[aus_production$Quarter >= min_date, ]
    filtered <- filtered[filtered$Quarter <= max_date, ]
    filtered <- filtered[, c("Quarter", input$selected_var4)]
    
    p <- as.numeric(input$p)
    d <- as.numeric(input$d)
    q <- as.numeric(input$q)
    ps <- as.numeric(input$ps)
    ds <- as.numeric(input$ds)
    qs <- as.numeric(input$qs)
    
    fit <- filtered[, c("Quarter", input$selected_var4)] %>% 
      model(
        AutoARIMA = ARIMA(),
        ManualARIMA = ARIMA( ~ 0 + pdq(p, d, q) + PDQ(ps, ds, qs))
      )
    
    fit %>% 
      select(input$selected_model4) %>% 
      forecast(h = input$selected_h4) %>% 
      autoplot(filtered)
  })
}