library(shiny)
library(dplyr)
library(readr)
library(dygraphs)
library(xts)
library(eflows)


# Functions ---------------------------------------------------------------
source("proto_backshift.R", local = TRUE)
# Data --------------------------------------------------------------------

# flows <- read_csv("tests/backshift/flows_export.csv")
# apx <- read_csv("tests/backshift/apx_export.csv") %>% 
#   select(c(1,2))
# rawdata <- left_join(flows, apx, by = "datetime") 
# 

rawdata <- tibble(datetime = sept$datetime[1:168], 
                  production = sept$solar[1:168], 
                  consumption = sept$d_household[1:168], 
                  price_euro_mwh = sept$eprice[1:168])

# ts_consumption <- rawdata %>% 
#   select(datetime, consumption) %>% 
#   df_to_ts()
# 
# ts_price <- rawdata$price_euro_mwh
# 



ts_consumption <- sept$d_household[1:168]

ts_price <- sept$eprice[1:168]


# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = "style.css",
                # Application title
                titlePanel("Backshift example"),
                
                sidebarLayout(
                  
                  # Sidebar with a slider input
                  sidebarPanel(width = 3,
                               p("Example of how a heat storage could be optimized by consuming beforehand, 
                                 with knowledge of future energy prices and a forecast of the consumption."), 
                               p("The calculations include restrictions about buffer size and the heat power that can be stored."),
                               sliderInput("range",
                                           "Hours to foresee:",
                                           min = 1,
                                           max = 24,
                                           value = 6),
                               sliderInput("loss",
                                           "Buffer loses per hour (%):",
                                           min = 0,
                                           max = 15,
                                           value = 2),
                               sliderInput("eff_to",
                                           "Efficiency to battery (%):",
                                           min = 85,
                                           max = 100,
                                           value = 100),
                               sliderInput("eff_from",
                                           "Efficiency from battery (%):",
                                           min = 85,
                                           max = 100,
                                           value = 100),
                               sliderInput("cap",
                                           "Maximum energy to the buffer (kW)",
                                           min = 1,
                                           max = 30,
                                           value = 5),
                               sliderInput("vol",
                                           "Volume buffer (kWh)",
                                           min = 1,
                                           max = 40,
                                           value = 8), 
                               sliderInput("vol_init",
                                           "Initial soc (%)",
                                           min = 0,
                                           max = 100,
                                           value = 0), 
                               actionButton("recalculate", "Recalculate", icon = icon("refresh")), 
                               uiOutput("changes")
                               
                               
                               ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(width = 9,
                            dygraphOutput("plot_price", height = 200),
                            dygraphOutput("plot_cost", height = 200),
                            dygraphOutput("plot_consumption", height = 200),
                            dygraphOutput("plot_buffer", height = 200)
                  )
                )
                )


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  res0 <- reactive({
    input$recalculate
    
    backshift(ts_consumption, ts_price, 
              range = isolate(input$range), 
               eff = list(1 - (isolate(input$loss)/100),
                          (isolate(input$eff_to)/100), 
                          (isolate(input$eff_from)/100)), 
               cap = list(0,isolate(input$cap)), 
               vol = isolate(input$vol), 
               vol_init = (isolate(input$vol_init)/100) * isolate(input$vol))
  })
  
  res <- reactive({
    as_tibble(data.frame(datetime= sept$datetime[1:168] , coredata(res0()))) %>% 
      left_join(rawdata, by = "datetime", suffix = c("_final", "_initial")) %>% 
      mutate(cost_final = consumption_final * price_euro_mwh,
             cost_initial = consumption_initial * price_euro_mwh)
  })
  
  output$plot_price <- renderDygraph({
    res() %>% 
      select(datetime, price_euro_mwh) %>% 
      df_to_ts() %>% 
      dygraph(main = "price", group = "g", height = 200) %>% 
      dyHighlight() %>% 
      dyOptions(fillGraph = TRUE)
  })
  output$plot_consumption <- renderDygraph({
    res() %>% 
      select(datetime, consumption_final, consumption_initial) %>% 
      df_to_ts() %>% 
      dygraph(main = "consumption", group = "g", height = 200) %>% 
      dyHighlight() %>% 
      dyOptions(fillGraph = TRUE)
  })
  output$plot_cost <- renderDygraph({
    res() %>% 
      select(datetime, cost_final, cost_initial) %>% 
      df_to_ts() %>% 
      dygraph(main = "cost", group = "g", height = 200) %>% 
      dyHighlight() %>% 
      dyOptions(fillGraph = TRUE)
  })
  output$plot_buffer <- renderDygraph({
    res() %>% 
      select(datetime, batt) %>% 
      df_to_ts() %>% 
      dygraph(main = "buffer", group = "g", height = 200) %>% 
      dyHighlight() %>% 
      dyOptions(fillGraph = TRUE)
  })
  
  summ <- reactive({
    res() %>% select(-datetime) %>% as.matrix() %>% apply(2, sum)
    
  })
  
  output$change_cost <- renderPrint({
    round(unname(summ()["cost_final"]/summ()["cost_initial"]),4)
   
  })
  
  output$change_comp <- renderPrint({
    round(unname(summ()["consumption_final"]/summ()["consumption_initial"]),4)
  })
  
  output$changes <- renderUI({
    div(
      h4("Cost change"),
      verbatimTextOutput("change_cost"),
      h4("Consumption change"),
      verbatimTextOutput("change_comp")
    )
  })
  
}


# Run ---------------------------------------------------------------------
shinyApp(ui, server)