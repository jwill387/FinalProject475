ui <- fluidPage({
  titlePanel("FinalProject")
  navbarPage("Time Series Modelling", theme = shinytheme("slate"),
             tabsetPanel(
               tabPanel(
                 title = "Instructions",
                 p("1. Beer Production in Australia"),
                 p("No user action required. A full time series model is displayed for the beer production in Australia"),
                 p("*****"),
                 p("2. Selected Time Series"),
                 p("User selects their desired plot (Seasonality, Autocorrelation, Decomposition)"),
                 p("User selects their desired variable to examine in the time series"),
                 p("User selects the number of lags used in the autocorrelation plot"),
                 p("*****"),
                 p("3. Custom Time Series"),
                 p("User selects their desired variable to plot over the time series"),
                 p("User selects their desired date range to observe"),
                 p("User is able to interact with the plot by zooming in (click and drag), zooming out (double click), compare data to the trend line, and download their current plot"),
                 p("*****"),
                 p("4. Simple Models"),
                 p("User selects their desired variable to plot over the time series"),
                 p("User selects their desired simple model to fit over the data (Naive, SeasonalNaive, Mean, Drift"),
                 p("User uses a slider to select the number of periods that the model will forecast"),
                 p("User selects their desired date range to observe"),
                 p("*****"),
                 p("5. Holts & Holts/Winters Models"),
                 p("User selects their desired variable to plot over the time series"),
                 p("User selects their desired simple model to fit over the data (Holts, HoltsWinters"),
                 p("User uses a slider to select the number of periods that the model will forecast"),
                 p("User selects their desired date range to observe"),
                 p("*****"),
                 p("6. Holts & Holts/Winters Models"),
                 p("User selects their desired variable to plot over the time series"),
                 p("User selects their desired simple model to fit over the data (AutoARIMA, ManualARIMA"),
                 p("If the user selects ManualARIMA, a series of radio buttons will appear allowing the user to select the nonseasonal and seasonal autoregressive terms, number of differences, and moving average terms"),
                 p("User uses a slider to select the number of periods that the model will forecast"),
                 p("User selects their desired date range to observe")
                 ),
               
               tabPanel(
                 title = "1. Beer Production in Austrailia",
                 plotlyOutput("ts_plot")
                 ),
               
               tabPanel(
                 title = "2. Selected Time Series",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "selected_plot",
                       label = "Select Plot",
                       choices = c("Seasonality", "Autocorrelation", "Decomposition")
                       ),
                     
                     selectInput(
                       inputId = "selected_var",
                       label = "Select Variable",
                       choices = names(aus_production)[-1]
                       ),
                     
                     selectInput(
                       inputId = "selected_lags",
                       label = "Select Max Lags",
                       choices = c(1:18)
                       )
                     ),
                   
                   mainPanel(
                     plotlyOutput("user_plot"),
                     p("Seasonality: The seasonality is how repetitive a pattern is over the course of a year"),
                     p("Example: Ice cream sales always peak in the Summer, and always dip down around fall. This is highly seasonal"),
                     p("*****"),
                     p("Autocorrelation: The degree of similarity between the current time series and a lagged version of itself over given intervals"),
                     p("Example: If it rains today, it is more likely to rain tomorrow than it would be if it were clear skies today"),
                     p("*****"),
                     p("Decomposition: The remainder of a graph when trend, seasonality, and noise are subtracted out"),
                     p("Example: How much does random chance contribute to the fact that it could rain tomorrow?")
                    ),
                   )
                ),
    
    
    tabPanel(
      title = "3. Custom Time Series",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_ts_var",
            label = "Select Variable",
            choices = names(aus_production)[-1]
            ),
      
      dateRangeInput(
        inputId = "selected_date_range",
        label = "Select Date Range:",
        min = min(aus_production$Quarter),
        max = max(aus_production$Quarter),
        start = min(aus_production$Quarter),
        end = max(aus_production$Quarter)
        )
      
      ),
      
      mainPanel(
      plotlyOutput("ts_custom")
        ),
      )
    ),
    
    tabPanel(
      title = "4. Simple Models",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "aubs_selected_var",
            label = "Select Variable:",
            choices = names(aus_production)[-1]
          ),
          
          selectInput(
            inputId = "selected_model",
            label = "Select Simple Model:",
            choices = c("Naive", "SeasonalNaive", "Mean", "Drift")
          ),
          
          sliderInput(
            inputId = "selected_h",
            label = "Select Periods to Forecast:",
            min = 1,
            max = 12,
            value = 6
          ),
          
          dateRangeInput(
            inputId = "selected_date_range2",
            label = "Select Date Range:",
            min = min(aus_production$Quarter),
            max = max(aus_production$Quarter),
            start = min(aus_production$Quarter),
            end = max(aus_production$Quarter)
          ),
        ),
        
      mainPanel(
        plotOutput("simple_plots")
      ),
    )
    ),
    
    tabPanel(
      title = "5. Holts & Holts/Winters Models",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_var3",
            label = "Select Variable:",
            choices = names(aus_production)[-1]
          ),
          
          awesomeRadio(
            inputId = "selected_model3",
            label = "Select Model:",
            choices = c("Holt", "HoltWinters")
          ),
      
          sliderInput(
            inputId = "selected_h3",
            label = "Select Periods to Forecast:",
            min = 1,
            max = 12,
            value = 6
          ),
      
          dateRangeInput(
            inputId = "selected_date_range3",
            label = "Select Date Range:",
            min = min(aus_production$Quarter),
            max = max(aus_production$Quarter),
            start = min(aus_production$Quarter),
            end = max(aus_production$Quarter)
          ),
        ),
        
      mainPanel(
      plotOutput("ets_plots")
      ),
      )
    ),
    
    tabPanel(
      title = "6. ARIMA",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_var4",
            label = "Select Variable:",
            choices = names(aus_production)[-1]
          ),
          
          awesomeRadio(
            inputId = "selected_model4",
            label = "Select Model:",
            choices = c("AutoARIMA", "ManualARIMA")
          ),
          
          uiOutput("manualui"),
          
          
          
          sliderInput(
            inputId = "selected_h4",
            label = "Select Periods to Forecast:",
            min = 1,
            max = 12,
            value = 6
          ),
          
          dateRangeInput(
            inputId = "selected_date_range4",
            label = "Select Date Range:",
            min = min(aus_production$Quarter),
            max = max(aus_production$Quarter),
            start = min(aus_production$Quarter),
            end = max(aus_production$Quarter)
          ),
        ),
      
      mainPanel(
      plotOutput("arima_plots")
      ),
    )
  ),
  )
  )
})
