

library(shiny)
library(knitr)
library(tidyverse)
library(lubridate)
library(bizdays)
library(tidyquant)

table_stock = tq_get(c("AAPL","GOOG","AMZN"), get = "stock.prices", from = '2020-07-01',to = "2020-12-02")


#get individual asset returns grouped by asset
stock_returns <- c("AAPL", "GOOG", "AMZN") %>%
  tq_get(get  = "stock.prices",
         from = "2020-07-01",
         to   = "2020-12-02") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Ra")



# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Investment Project",
  mainPanel(
    tabsetPanel(
      tabPanel("Stock Information",
               
               # Create a new Row in the UI for selectInputs
               fluidRow(
                 column(4,
                        selectInput("sym",
                                    "Symbol:",
                                    c("All",
                                      unique(as.character(table_stock$symbol))))
                        
                 ),
                 column(4,
                        selectInput("date",
                                    "Date:",
                                    c("All",
                                      unique(as.character(table_stock$date))))
                        
                        
                 )
               ),
               # Create a new row for the table.
               DT::dataTableOutput("table")),
      
      tabPanel("Portfolio Performance",
               column(4, wellPanel(
                 sliderInput("n1", "Weight of AAPL:",
                             min = 0, max = 1, value = 0.25, step = 0.05),
                 
                 sliderInput("n2", "Weight of GOOG:",
                             min = 0, max = 1, value = 0.50, step = 0.05),
                 
                 sliderInput("n3", "Weight of AMZN:",
                             min = 0, max = 1, value = 0.25, step = 0.05)
               )),
               
               column(5,
                      "The plot below will be not displayed when the sum of Weights is not euqal to 1.",
                      
                      conditionalPanel("input.n1 + input.n2 + input.n3 == 1",
                                       plotOutput("portfolioPlot", height = 700, width = 900)
                   )
               )
          )
     )

   )

)


# Define server logic required to draw a histogram
server <- function(input, output) {
 
   output$table <- DT::renderDataTable(DT::datatable({
    data <- table_stock
    if (input$sym != "All") {
      data <- data[data$symbol == input$sym,]
    }
    if (input$date != "All") {
      data <- data[data$date == input$date,]
    }
    data
  }))
   
   output$portfolioPlot <- renderPlot({
     wts <- c(input$n1, input$n2, input$n3)
     portfolio_growth <- stock_returns %>%
       tq_portfolio(assets_col   = symbol, 
                    returns_col  = Ra, 
                    weights      = wts, 
                    col_rename   = "investment.growth",
                    wealth.index = TRUE) %>%
       mutate(investment.growth = investment.growth * 250000)
     
     portfolio_growth %>%
       ggplot(aes(x = date, y = investment.growth)) +
       geom_line(size = 2, color = palette_light()[[1]]) +
       labs(title = "Portfolio Growth",
            x = "", y = "Portfolio Value") +
       geom_smooth(method = "loess") +
       theme_tq() +
       scale_color_tq() +
       scale_y_continuous(labels = scales::dollar)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

