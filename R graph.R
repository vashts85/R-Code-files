rm(list=ls())

setwd("C:/Users/rcarvalho/Desktop/")
data=read.csv("book1.csv")
data=data[1:162,]

data$RR=as.numeric(sub("%","", data$RR))

list1=levels(data$MEPS)[2:4]
list2=levels(data$MEPS)[5:7]


require(ggplot2)
           
for (name in 1:2) {
  png(paste("plot",name,".png",sep=""),width=800, height=600)
  g=ggplot(data=data[eval(parse(text = paste("data$MEPS %in% list",name,sep=""))),],
           aes(x=Year, y=RR, colour=MEPS, group=MEPS))+
    geom_line(size=.8)+scale_y_continuous(breaks=seq(0, 100, 10))+labs(y="Response Rate")+
    coord_cartesian(ylim=c(0, 100))
  print(g)
  dev.off()
}
 
#################################################################################

rm(list=ls())

require(scales)
require(reshape2)
require(ggplot2)

setwd("C:/Users/rcarvalho/Desktop/")

df=read.csv("all response rates.csv")
df[-1]<-data.frame(apply(df[-1], 2, function(x) 
    as.numeric(sub("%","",as.character(x)))))

df$Year=paste("20",df$Year,"01", sep="")
df$Year=gsub("-","",df$Year)
df$Year=as.Date(df$Year, format="%Y%B%d", tz="")
df$Time=df$Year 
df$Year=format(df$Year, "%Y")
head(df)

df.long <- melt(df, id=c("Time","Year"), value.name="RR", variable.name="MEPS")

pdf("all plots3.pdf", paper="USr", height=8, width=10)  
for (i in levels(df.long$MEPS)) {
  g= ggplot(data=df.long[as.character(df.long$MEPS) %in% i,],
         aes(x=Time, y=RR, colour=MEPS, group=MEPS, label=RR))+geom_line(size=.8)+
    scale_y_continuous(breaks=seq(0, 100, 10))+
    labs(y="Response Rate")+
    coord_cartesian(ylim=c(0, 120))+
    geom_text(vjust=-1, size=4, angle=0, colour="black")+
    ggtitle(paste(i,"Response Rates"))+
    theme(plot.title=element_text(size=18, face="bold", vjust=1),
          axis.title=element_text(size=16),
          axis.text.x=element_text(size=10, angle=90),
          legend.position="none")+
          scale_x_date(labels = date_format("%b-%y"), breaks=date_breaks("month"))+
    facet_grid(~Year, scales="free_x", space="free_x") 
    print(g)
}
dev.off()


ggplot(data=df.long[as.character(df.long$MEPS) %in% "SPOKANE",],
          aes(x=Time, y=RR, colour=MEPS, group=MEPS, label=RR))+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  labs(y="Response Rate")+
  coord_cartesian(ylim=c(0, 105))+
  geom_text(vjust=-1, size=4, angle=0, colour="black")+
  ggtitle(paste("i","Response Rates"))+
  theme(plot.title=element_text(size=18, face="bold", vjust=1),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=10, angle=90),
        legend.position="none",
        panel.background = element_rect(linetype=0),
        panel.grid.major=element_line(linetype=0),
        panel.grid.minor=element_line(linetype=0))+  
  scale_x_date(labels = date_format("%b-%y"), breaks=date_breaks("month"))+
  geom_line(size=.8)+
  facet_grid(~Year, scales="free_x", space="free_x") 

dput(df.long[df.long$MEPS=="SPOKANE",])

df.long$MEPS

names(df.long)[names(df.long)=="MEPS"] <- "locale"











print(g)


#################################################################################



capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

MEPS_names=levels(df.long$MEPS)
class(MEPS_names)
MEPS_names=sub("FORT.","FORT ",df.long$MEPS)
table(MEPS_names)
MEPS_names=sub("\\.\\.","\\. ",MEPS_names)
table(MEPS_names)
MEPS_names
capwords(tolower(levels(df.long$MEPS)))

clean_meps_names=