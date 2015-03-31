rm(list=ls())

require(scales)
require(reshape2)
require(ggplot2)
require(plotly)
require(grid)
require(gridExtra)
require(openxlsx)

setwd("C:/Users/rcarvalho/Desktop/")

df=read.xlsx("Responses - All Services.xlsx", startRow=3, rows=3:68)
df=df[,-1]
df<-as.data.frame(t(df))
class(df)
colnames(df)

df[1,]
colnames(df)<-df[1,2:65]
head(df)
df$MEPS=as.Date(df$MEPS, origin="1899-12-30")
head(df$MEPS)

df.long <- melt(df, id="MEPS")