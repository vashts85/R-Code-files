library(shiny)
setwd("C:/Users/rcarvalho/Desktop/R-code-files/project/")

shinyUI(fluidPage(
        pageWithSidebar(
                # Application title
                headerPanel("Amortization prediction"),
                sidebarPanel(
                        numericInput('cost', 'Cost of Home (In USD)', 405000),
                        sliderInput('downpayment', label="Downpayment",min=0, max=50, value=20, post="%"),
                        sliderInput('interest',label="Interest rate", min=0, max=10, value=3, post="%"),
                        sliderInput('term', label="Term", min=0, max=30, value=15, post=" years")
                      ),
                mainPanel(
                        h4('Your monthly mortgage payment is:'),
                        verbatimTextOutput("text1")
                )
        )
))