# Yes
ui <- fluidPage({
  titlePanel("Final")
  navbarPage("Final Project", theme = shinytheme("slate"),
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
                 p("User is able to interact with the plot by zooming in (click and drag), zooming out (double click), compare data to the trend line, and download their current plot")
                 ),
               
               tabPanel(
                 title = "Beer Production in Austrailia",
                 plotlyOutput("ts_plot")
                 ),
               
               tabPanel(
                 title = "Selected Time Series",
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
                     p("Decomposition: The remainder of a graph when trend, seasonality, and noise are subtracted out")
                    ),
                   )
                ),
    
    
    tabPanel(
      title = "Custom Time Series",
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
        ),
      ),
      
      mainPanel(
      plotlyOutput("ts_custom")
        ),
      )
    ),
    
    tabPanel(
      title = "Simple Models",
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
      title = "Holts & Holts/Winters Models",
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
      title = "ARIMA",
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
