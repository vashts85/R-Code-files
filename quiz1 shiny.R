install.packages("shiny")
install.packages("manipulate")
# 
# runApp(displayMode= 'showcase')

# Question 1
data(cars)
library(manipulate)
myPlot <- function(s) {
  plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
  abline(0, s)
}

myPlot

manipulate(myPlot(s), s=slider(0,2, step=0.1))

# Question 2

install.packages("rCharts")
install.packages("googleVis")

library(rCharts)
rPlot(Wind ~ Temp, data=airquality, type='bar', color='Wind')

names(iris) = gsub('\\.', '', names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, type = 'point', color = 'Species')


data(airquality)
dTable(airquality, sPaginationType = "full_numbers")

# gvisLineChart

# Question 4

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science FTW!"),
  sidebarPanel(
    h2('Big text'),
    h3('Sidebar')
  ),
  mainPanel(
    h3('Main Panel text')
  )
))

# Question 5

shinyUI(pageWithSidebar(
  headerPanel("Example plot"),
  sidebarPanel(
    sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)
  ),
  mainPanel(
    plotOutput('newHist')
  )
))