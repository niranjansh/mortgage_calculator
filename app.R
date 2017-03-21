## app.R ##
library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "Mortgage Calculator"
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(width = NULL, status = "warning",
               numericInput("loan_amt", label = "Mortgage Amount", value = 100000),
               numericInput("int_rate", label= "Interest Rate (%)", value = 3.75),
               numericInput("term", label = "Mortgage Period (years)", value = "30"),
               actionButton("calc_m", "Calculate Mortgage")
           )
    ),
     column(width = 3,
       box(width = NULL, status = "warning",
           textOutput("monthly_payment"),
           textOutput("total_cost_of_mortgage"),
           textOutput("total_interest_paid")
           )
     )
    
  )
 )


ui <- dashboardPage(
        header,
        dashboardSidebar(disable = TRUE),
        body
)

server <- function(input, output) { 
  mortgage <- reactive ({
    loan_amt <- input$loan_amt
    int_rate <- input$int_rate/(12*100)
    term <- input$term*12
    
    monthly_payment <- (loan_amt*int_rate)/(1-1/((1+int_rate)^term))
    monthly_payment
  })
  
  output$monthly_payment <- renderText({paste0("Monthly payment: ",
    prettyNum(round(mortgage(),0),big.mark=","))})
  output$total_cost_of_mortgage <- renderText({
    paste0("Total mortgage payment: ",
           prettyNum(round(mortgage()*input$term*12,0),big.mark=","))})
  output$total_interest_paid <- renderText({
    paste0("Total interest paid: ",
           prettyNum(round(mortgage()*input$term*12 - input$loan_amt,0),big.mark=","))})
}

shinyApp(ui, server)