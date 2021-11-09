library(shiny)
library(shinydisconnect)
library(shinythemes)
library(plotly)
library(dygraphs)
library(reactable)

shinyUI(
  fluidPage(
    theme=shinythemes::shinytheme("journal"),
    disconnectMessage(
      text = "An error occurred. Please refresh the page and try again.",
      refresh = "Refresh",
      width = 450,
      top = 50,
      size = 22,
      background = "white",
      colour = "#444444",
      overlayColour = "black",
      overlayOpacity = 0.6,
      refreshColour = "#337ab7",
      css = ""
    ),
    titlePanel(fluidRow(
      column(10, titlePanel(title = "Forecast algoritms test-bench")),
      column(2, actionButton("disconnect", "Disconnect the app"))
    )),
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # TabSet Definition ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
    tabsetPanel
    (id="tbs",
      type = "tabs",
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Data Load ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      tabPanel(
        "Data Load",
        #tags$h3(helpText("Data exploration ...")),
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        # Criteria an configuration definitions ####
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        sidebarPanel(
          fluidRow(
            fileInput(
              inputId = "ficheiro",
              "Choose CSV File",
              #accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
              accept = ".csv",
              multiple = FALSE
            )),
          fluidRow(
            checkboxInput(
              inputId = 'cabecalhos',
              label = "Headers",
              value = TRUE),
            selectInput(
              "separador",
              "Column separator",
              choices = c(
                "Semicolon" = ";",
                "Colon" = ",",
                "Tab" = "\tb",
                "Space" = " "
              ),
              selected = c(";"),
              multiple = FALSE
            ),
            actionButton("loadFileButton", "Load File!", class = "btn-success")
          ),#End Row
          fluidRow(
            column(4,numericInput("colDateNumber", "Date Number", 1, 1, 20, 1)),
            column(4,numericInput("colValueNumber", "Value Number", 2, 1, 20, 1))
          ),
          fluidRow(
            column(12,
                   selectInput(
                     "dateType",
                     "Data Type",
                     choices = c(
                       "Daily" = "day",
                       #"Weekly" = "week",
                       "Monthly" = "month",
                       "Yearly" = "year"
                     ),
                     selected = c("month"),
                     multiple = FALSE
                   ))
          ),
          fluidRow(
            column(12,
                   selectInput(
                     "dateFormat",
                     "Date Format",
                     choices = c(
                       "Year/Month/Day" = "ymd",
                       "Year/Day/Month" = "ydm",
                       "Month/Day/Year" = "mdy",
                       "Month/Year/Day" = "myd",
                       "Day/Month/Year" = "dmy",
                       "Day/Year/Month" = "dym"
                     ),
                     selected = c("ymd"),
                     multiple = FALSE
                   ))
          ),
          fluidRow(
            column(12,actionButton("df2_Button", "Adjust Data", class = "btn-success"))
          ),
          width = 2
        ),#End sidebarPanel
        
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        # Main  panel 
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        mainPanel(
          fluidRow(
            column(3,
                   tags$h4(helpText("Top Records ...")),
                   tableOutput("topRecordsTable")),
            column(3,
                   tags$h4(helpText("Tail Records ...")),
                   tableOutput("tailRecordsTable")),
            column(3,
                   tags$h4(helpText("Summary statistics...")),
                   tableOutput("summaryDateDataTable")),
            column(3,
                   tableOutput("summaryValueDataTable"))
          ),
          fluidRow(
            dygraphOutput("TimeSeriesPlot", height = "250"),
          ),
          width = 9
        ) #End Main Page
        
      ),# End Panel-Data Load
      
      
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Data Preparation ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      navbarMenu(
        "Data Preparation",
        
        tabPanel(
          "Anomalies",value="Anomalies",
          sidebarPanel(width = 2,
                       fluidRow(
                         sliderInput(
                           "alpha",
                           "Select IQR Factor",
                           ticks=FALSE,
                           min = 0,
                           max = 4,
                           value = 3.0,
                           step = 0.5)
                       ),
                       fluidRow(
                         selectInput(
                           "imputationMethod",
                           "Imputation Methods",
                           choices = c(
                             "Linear Interpolation" = "linear.interp",
                             "Spline Interpolation" = "spline.interp",
                             "Stineman Interpolation" = "stine.interp",
                             "MICE" = "mice",
                             "Kalman StructTS" = "kalman.1",
                             "Kalman StructTS Smooth" = "kalman.2",
                             "Kalman Space Models" = "kalman.3",
                             "Seasonally Decomposed - Interpolation" = "seadec.1",
                             "Seasonally Decomposed - Mean" = "seadec.2",
                             "Seasonally Decomposed - Kalman" = "seadec.3"),
                           selected = c("linear.interp"),
                           multiple = FALSE)
                       ),
                       fluidRow(
                         actionButton("imp_Button2", "Test", class = "btn-success"),
                         actionButton("imp_Button", "Apply", class = "btn-success")
                       )),
          column(10,
                 fluidRow(tableOutput("AnomalyDiagnosticDataOutliersSummaryTable")),
                 fluidRow(tableOutput("anomalyDiagnosticTable")),
                 fluidRow(
                   column(6,plotOutput("naSeriesPlot", height = "400")),
                   column(6,plotlyOutput("anomalyDiagnosticPlot", height = "400"))))),
        tabPanel(
          "Differencing",value="Differencing",
          sidebarPanel(width = 2,
                       fluidRow(
                         column(12,numericInput("diffLag", "Diferencing", 1, 1, 12, 1))
                       ),
                       fluidRow(
                         actionButton("diff_tst_Button", "Test", class = "btn-success"),
                         actionButton("diff_apply_Button", "Apply", class = "btn-success"),
                         
                       )),
          column(10,
                 column(4,tableOutput("getDiffStationaryTable")),
                 column(8,
                 verbatimTextOutput("verb"),
                 dygraphOutput("diffSeriesPlot", height = "350"))
          )
        )
      ), # End navbarMenu Data Preparation
      
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Data Exploration ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      navbarMenu(
        "Data Exploration",
        
        tabPanel(
          "Original Serie",value="OriginalSerie",
          tags$h3(helpText("Original Series ...")),
          dygraphOutput("IndividualSeriesPlotly")
        ),
        tabPanel(
          "Series Periods",value="SeriesPeriods",
          tags$h3(helpText("Series Periods ...")),
          plotOutput("seriesPeriodsPlot", height = "750")
        ),
        tabPanel(
          "Monthly analysis",value="MonthlyAnalisys",
          tags$h3(helpText("Monthly Periods ...")),
          plotOutput("MonthlySeriesPeriodsPlot", height = "750")
        ),
        tabPanel(
          "Quarterly analysis",value="QuarterlyAnalisys",
          tags$h3(helpText("Quarterly Periods ...")),
          plotOutput("QuarterlySeriesPeriodsPlot", height = "750")
        ),
        tabPanel(
          "Half-Year analysis",value="Half-YearAnalisys",
          tags$h3(helpText("Half-Year Periods ...")),
          plotOutput("HalfYearSeriesPeriodsPlot", height = "750")
        ),
        tabPanel(
          "Yearly analysis",value="YearlyAnalisys",
          tags$h3(helpText("Yearly Periods ...")),
          plotOutput("YearSeriesPeriodsPlot", height = "750")
        ),
        tabPanel(
          "Yearly data summary",value="YearlyDataSummary",
          tags$h3(helpText("Yearly data summary ...")),
          plotlyOutput("IndividualBoxPlotly", height = "750")
        ),
        tabPanel(
          "Evolutions",value="DataEvolutions",
          tags$h3(helpText("Yearly and Monthly evolution ...")),
          plotlyOutput("evolutionFacetPlot", height = "750")
        )
      ), # End navbarMenu Data Exploration
      
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Data Analysis ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      navbarMenu(
        "Decompositon",
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        # Main  panel 
        # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
        tabPanel(
          "STL - Aditional",value="STLAditional",
          sidebarPanel(
            checkboxGroupInput("decompositionAnalysis",
                               label=h3("Select Analysis"),
                               choices = c("Original"="Original","Trend"="Trend","Seasonaliy"="Seasonal12","Residuals"="Remainder","TrendResiduals"="TrendResiduals"),
                               selected = c("Trend","TrendResiduals"),
                               inline=FALSE
            ),
            width=3), #End sidebarPanel
          #tags$h3(helpText("STL decomposition ...")),
          column(2,fluidRow(
                 plotOutput("STLStrengthPlots",height = "150px"),
                 plotlyOutput("STLdecompositionPlot",height = "300px")
                 )),
          column(7,dygraphOutput("STLdecompositionPlot2",height = "450px")),
        )#,
      ),# End navbarMenu Data analysis
      
      
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Cross validation ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      navbarMenu(
        "Cross Validation",
        
        tabPanel(
          "Definition",value="CVDefinition",
          sidebarPanel(
            width = 4,
            
            fluidRow(
              selectInput(
                "cvModels",
                "Select forecast methods",
                choices = c(
                  "PROPHET" = "PROPHET",
                  "NNETAR" = "NNETAR",
                  "THETAF" = "THETAF",
                  "ETS ANN" = "ETS ANN",
                  "ETS MAM" = "ETS MAM",
                  "ETS ZZZ" = "ETS ZZZ",
                  "BATS" = "BATS",
                  "Auto Arima" = "Auto Arima",
                  "Struct" = "Struct",
                  "Holt Winters" = "HW",
                  "NAIVE" = "NAIVE",
                  "SNAIVE" = "SNAIVE",
                  "RWF" = "RWF"
                ),
                selected = c(
                  "PROPHET",
                  "NNETAR",
                  "THETAF",
                  "ETS ANN",
                  "ETS MAM",
                  "ETS ZZZ",
                  "BATS",
                  "Auto Arima",
                  "Struct",
                  "HW",
                  "NAIVE",
                  "SNAIVE",
                  "RWF"
                ),
                multiple = TRUE
              )
            ),
            fluidRow(
              sliderInput(
                "cvInitial",
                "Series initial position.",
                min = 1,
                max = 50,
                value = 1,
                step = 1
              )
            ),
            fluidRow(
              sliderInput(
                "cvWindow",
                "Number of trainning values.",
                min = 1,
                max = 60,
                value = 12,
                step = 1
              )
            ),
            fluidRow(
              sliderInput(
                "cvH",
                "Number of forecast values.",
                min = 1,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            fluidRow(
              sliderInput(
                "cvStep",
                "Number of step positions.",
                min = 1,
                max = 24,
                value = 1,
                step = 1
              )
            ),
            
            fluidRow(
              sliderInput(
                "cvMax",
                "Maximum number of Cross validation series.",
                min = 1,
                max = 20,
                value = 6,
                step = 1
              )
            ),
            fluidRow(
              selectInput(
                "cvDataType",
                "CV-Data Type",
                choices = c(
                  "Fixed Window" = "Fixed Window",
                  "Random Fixed Window" = "Random Fixed Window",
                  "Rolling" = "Rolling",
                  "Random Rolling" = "Random Rolling"
                ),
                selected = c("Fixed Window"),
                multiple = FALSE
              )
            ),
            fluidRow(actionButton("calc_CV", "Calculate", class = "btn-success")),
          ),
          column(8,
                 fluidRow(plotOutput("cvDataPlot", height = "750")),)
          
        ),
        
        tabPanel(
          "Error Analysis",value="ErrorAnalysis",
          sidebarPanel(width = 3,
                       fluidRow(
                         selectInput(
                           "cvErrorVal",
                           "CV-Error",
                           choices = c(
                             "Medium Error" = "ME",
                             "Mean Squared Error" = "MSE",
                             "Root Mean Squared Error" = "RMSE",
                             "Medium Absolute Error" = "MAE",
                             "Medium Absolute Percentage Error" = "MAPE"),
                           selected = c("RMSE"),
                           multiple = FALSE)
                       ),
          ),
          column(9,
                 fluidRow(
                   tableOutput("cvErrorRankingTable"),
                   tableOutput("cvErrorTable"),
                   plotOutput("cvErrorPlot"),
                   height = "750")
          ),#end column
        ),
        
        tabPanel(
          "Error analysis Plots",value="ErrorAnalisysPlots",
          tags$h3(helpText("Cross Validation - Models evaluation")),
          plotOutput("CrossValuesErrorsBoxPlot", height = "750")
        ),
        
        tabPanel(
          "Error Plots",value="ErrorPlots",
          tags$h3(helpText("Cross Validation - Models evaluation")),
          plotOutput("cvErrorsPlot", height = "750")
        )
        
        
      ), # End navbarMenu Cross Validation
      
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      # Forecast ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
      navbarMenu(
        "Forecasting",
        
        tabPanel(
          "Point Forecast",value="ForecastDefinition",
          sidebarPanel(width = 2,
                       fluidRow(
                         selectInput(
                           "forecastModels",
                           "Select forecast methods",
                           choices = c(
                             "PROPHET"="PROPHET",
                             "NNETAR"="NNETAR",
                             "THETAF"="THETAF",
                             "ETS ANN"="ETS ANN",
                             "ETS MAM"="ETS MAM",
                             "ETS ZZZ"="ETS ZZZ",
                             "BATS"="BATS",
                             "Auto Arima"="Auto Arima",
                             "Struct"="Struct",
                             "Holt Winters"="HW",
                             "NAIVE"="NAIVE",
                             "SNAIVE"="SNAIVE",
                             "RWF"="RWF"
                           ),
                           selected = c(
                             "PROPHET",
                             "NNETAR",
                             "THETAF",
                             "ETS ANN",
                             "ETS MAM",
                             "ETS ZZZ",
                             "BATS",
                             "Auto Arima",
                             "Struct",
                             "HW",
                             "NAIVE",
                             "SNAIVE",
                             "RWF"
                           ),
                           multiple = TRUE
                         ),
                         sliderInput(
                           "nFor",
                           "Number of forecast values.",
                           min = 1,
                           max = 24,
                           value = 1,
                           step = 1)
                       ),
                       fluidRow(
                         actionButton("calcForecast", "Calculate", class = "btn-success")
                       ),
          ),
          column(10,
                 fluidRow(
                   tableOutput("ForecastValuesTable"),
                   dygraphOutput("ForecastValuesPlots", 
                                 height = "350")),
          )
          
        ), # End TabPanel
        tabPanel(
          "Interval Forecast",value="InterForecDefinition",
          sidebarPanel(width = 2,
                       fluidRow(
                         selectInput(
                           "intervalforecastModels",
                           "Select forecast method",
                           choices = c(
                             "THETAF"="THETAF",
                             "ETS ANN"="ETS ANN",
                             "ETS MAM"="ETS MAM",
                             "ETS ZZZ"="ETS ZZZ",
                             "BATS"="BATS",
                             "Auto Arima"="Auto Arima",
                             "Struct"="Struct",
                             "Holt Winters"="HW",
                             "NAIVE"="NAIVE",
                             "SNAIVE"="SNAIVE",
                             "RWF"="RWF"
                           ),
                           selected = c("RWF"),
                           multiple = FALSE
                         ),
                         sliderInput(
                           "nIntFor",
                           "Number of forecast values.",
                           min = 1,
                           max = 24,
                           value = 1,
                           step = 1)
                       ),
                       fluidRow(
                         actionButton("calcIntervalForecast", "Calculate", class = "btn-success")
                       ),
          ),
          column(10,
                  fluidRow(
                    tableOutput("IntervalForecastValuesTable"),
                    dygraphOutput("IntervalForecastValuesPlots",height = "350")
                    ),
          )
          
        ) # End TabPanel
      ) # End navbarMenu Forecast
    ) # TabSetPanel
    
  )#End fluidPage
) #End shinyUI