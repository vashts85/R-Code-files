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
require(plotly)
require(grid)

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



names(df.long)[names(df.long)=="MEPS"] <- "locale"

#ggplot(data=subset(df.long, df.long$locale %in% levels(df.long$locale)[5:8]),




ggplot(data=subset(df.long, df.long$locale %in% c("SPOKANE","BALTIMORE","SEATTLE")),
         aes(x=Time, y=RR, colour=locale, group=locale, label=RR))+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  labs(y="Response Rate")+
  coord_cartesian(ylim=c(0, 110))+
  geom_line(size=.5)+
  geom_point()+
  ggtitle(paste("i","Response Rates"))+
  theme(plot.title=element_text(size=18, face="bold", vjust=1),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=10, angle=90),
        axis.line=element_line(colour="black", size=.2),
        legend.position="top",
        legend.title=element_blank(),
        legend.margin=unit(-0.6, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.border = element_line(colour="darkred", size=0.5, linetype="dashed", fill=NA),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(colour="gray", linetype="solid", size=.2))+ # or theme_blank())
  scale_x_date(labels = date_format("%b"), breaks=date_breaks("month"))+
  facet_grid(~Year, scales="free", space="free")+
  geom_text(aes(Time, RR),vjust=-1, size=4, fontface=2, show_guide=FALSE) #+ #Data labels, fontface=2 is for bold
  #geom_text(data=df.long[df$Time=="2014-12-01" & df.long$local %in% "SPOKANE",], aes(label=locale))

?ggplot2::theme

gt <- ggplot_gtable(ggplot_build(p)) #Takes existing plot and stores into a gtable (programmable structure)
gt$layout$clip[gt$layout$name == "panel"] <- "off" #Disables clipping
pdf("test.pdf", paper="USr", height=8, width=10)  
grid.draw(gt)
dev.off()


facet_grid(~ main.cat, scales = 'free')









library(ggplot2)
library(directlabels)
direct.label(q,list(fontface="italic","smart.grid"))

panel.background = element_rect(fill = "transparent",colour = NA),
plot.background = element_rect(fill = "transparent",colour = NA))

dput(df.long[df.long$locale=="SPOKANE",])

df.long$MEPS











print(g)

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