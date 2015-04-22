library(shiny)
setwd("C:/Users/rcarvalho/Desktop/R-code-files/project/")

amortization <- function(cost, downpayment, interest, term) {
       sprintf("$%.2f", 
        (cost)*(1-downpayment/100)*(interest/1200)*((1+interest/1200)^(term*12))/((1+interest/1200)^(term*12)-1)
        )        
}

shinyServer(
                        function(input, output) {
                             output$text1 <- renderText({amortization(input$cost,input$downpayment,input$interest,input$term)})
}
)