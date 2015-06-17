rm(list=ls())

require(scales)
require(reshape2)
require(ggplot2)
require(plotly)
require(grid)
require(gridExtra)

setwd("C:/Users/rcarvalho/Desktop/R-Code-Files/")

df=read.csv("recruits bubble.csv")

df=df[,1:3]; df

names(df)[1] <- "Rating"
df$Rating <- as.character(df$Rating)

namebank<-(df[[1]])

?plot

plot(df$Importance, df$Association, pch=1, cex=5) 

text(df$Importance, df$Association,labels=namebank, cex= 0.7, pos=3)
text(df$Importance, df$Association,labels=df$Association, cex= 0.7)

,main="Absolute Losses vs. Relative Losses(in%)",xlab="Losses (absolute, in miles of millions)",ylab="Losses relative (in % of January´2007 value",col="blue", pch = 19, cex = 1, lty = "solid", lwd = 2,text(percet_losses, abs_losses,namebank))

View(mtcars)
df$max<-as.numeric(apply(df, 1, max))

ggplot(data=df, aes(x=Importance, y=Association, label=Rating))+
         geom_text()
  geom_point(size=10)+
  coord_cartesian(ylim=c(0, .6), xlim=c(.2,.9))+
  geom_text(aes(Rating), size=4, color="white", show_guide=FALSE)


  geom_text(aes(Importance, namebank), size=4, color="white", vjust=-1, show_guide=FALSE)
  #+ #Data labels, fontface=2 is for bold



apply(seq(1:1000)/3,1,sum)

for (i in seq(1:1000)/3) {
print(is.integer(i))
}
x=1.999999999999 
x


sum(seq(1:999)[seq(1:999) %% 3 == 0 | seq(1:999) %% 5 == 0])

is.integer(11)

is.integer(seq(1:1000)/3)

p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))

p + geom_text()
# Change size of the label
p + geom_text(size=10)
p <- p + geom_point()

# Set aesthetics to fixed value
p + geom_text()
p + geom_point() + geom_text(hjust=0, vjust=0)
p + geom_point() + geom_text(angle = 45)

# Add aesthetic mappings
p + geom_text(aes(colour=factor(cyl)))
p + geom_text(aes(colour=factor(cyl))) + scale_colour_discrete(l=40)

p + geom_text(aes(size=wt))
p + geom_text(aes(size=wt)) + scale_size(range=c(3,6))

# You can display expressions by setting parse = TRUE.  The
# details of the display are described in ?plotmath, but note that
# geom_text uses strings, not expressions.
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
              parse = TRUE)

# Add an annotation not from a variable source
c <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
# Or, you can use annotate
c + annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")

# Use qplot instead
qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
      geom=c("point", "text"))
qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
  geom_text(colour = "red")

# You can specify family, fontface and lineheight
p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
p + geom_text(fontface=3)
p + geom_text(aes(fontface=am+1))
p + geom_text(aes(family=c("serif", "mono")[am+1]))