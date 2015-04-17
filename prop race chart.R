require(reshape2)
require(knitr)


setwd("C:/Users/rcarvalho/Desktop/R-Code-files/")
data=read.csv("prop by race.csv")

data$Poll=format(as.Date(data$Poll, "%m/%d/%Y"))


data=transform(data,White=White*100, Black=Black*100, Hispanic=Hispanic*100)

require(ggplot2)
require(rCharts)
m1<-mPlot(x="Poll", postUnits="%",  smooth="false", y=c("White","Black","Hispanic"), 
          type="Line", data=data, lineColors=c("darkblue","red","orange"))
print(m1)

m1$save('test2.html')