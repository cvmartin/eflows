library(shiny)
library(eflows)
library(dplyr)
library(dygraphs)


# UI ----------------------------------------------------------------------

ui <- fluidPage(# Application title
                titlePanel("Backshift example"),
                
                sidebarLayout(
                  
                  # Sidebar with a slider input
                  sidebarPanel(width = 2,
                               sliderInput("self_discharge",
                                           "Self discharge (% per timestep)",
                                           min = 0,
                                           max = 100,
                                           value = 1),
                               sliderInput("eff_to",
                                           "Efficiency to battery (%):",
                                           min = 85,
                                           max = 100,
                                           value = 95),
                               sliderInput("eff_from",
                                           "Efficiency from battery (%):",
                                           min = 85,
                                           max = 100,
                                           value = 95)), 
                               uiOutput("changes")
                               ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(width = 9,
                            dygraphOutput("plot_appreciate", height = 200)
                  )
                )
                


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  res0 <- reactive({
    mod <- eflows:::depreciate(
      # sept$d_household[10:20],
      rep(sept$d_household[20],11),
      self_discharge = input$self_discharge/100,
      eff = list(input$eff_to/100,input$eff_from/100), 
      backwards = TRUE
    ) 
    
    cbind(datetime = sept$datetime[5:25], 
          original = sept$d_household[5:25],
          modified = c(rep(NA,5), mod, rep(NA,5))) %>% 
      as_data_frame()
  })
  
  output$plot_appreciate <- renderDygraph({
    dygraph(res0())
  })
  
 
  
}


# Run ---------------------------------------------------------------------
shinyApp(ui, server)