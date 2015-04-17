require(UsingR)
require(HistData)
require(shiny)
library(Rcpp)
data(Galton)

shinyServer(
  function(input, output) {
    output$myHist <- renderPlot({
      hist(Galton$child, xlab='child height', col='lightblue',main='Histogram')
      mu <- input$mu
      lines(c(mu, mu), c(0, 200),col="red",lwd=5)
      mse <- mean((Galton$child - mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
    })
    
  }
)

 
