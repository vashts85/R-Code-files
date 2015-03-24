rm(list=ls())

setwd("C:/Users/rcarvalho/Desktop/")
data=read.csv("book1.csv")
data=data[1:162,]

data$RR=as.numeric(sub("%","", data$RR))


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

#################################################
#######Fixing labeling of MEPS names
#################################################

names(df.long)[names(df.long)=="MEPS"] <- "locale"

MEPS_names=levels(df.long$locale)
MEPS_names
MEPS_names=gsub("\\."," ",MEPS_names)
MEPS_names
MEPS_names=gsub("  ",", ",MEPS_names)
MEPS_names

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

MEPS_names=capwords(tolower(MEPS_names))
MEPS_names=sub("Portland, Me", "Portland, ME", MEPS_names)
MEPS_names=sub("Pittsburgh 1", "Pittsburgh", MEPS_names)
MEPS_names=sub("St,", "St.", MEPS_names)



levels(df.long$locale)<-MEPS_names

#################################################
#################################################
#################################################




#################################################################################



meps_seq=paste(seq(1,65, by=5),":",seq(1,65, by=5)+4, sep="")

pdf("test.pdf", paper="USr", height=8, width=10)  
for (i in meps_seq) { 
  
  list=levels(df.long$locale)[eval(parse(text=i))]
  
  p=ggplot(data=subset(df.long, df.long$locale %in% list),
           aes(x=Time, y=RR, colour=locale, group=locale, label=RR))+
    scale_y_continuous(breaks=seq(0, 100, 10))+
    labs(y="Response Rate")+
    coord_cartesian(ylim=c(0, 110))+
    geom_line(size=.5)+
    geom_point()+
    ggtitle("MEPS Response Rates")+
    theme(plot.title=element_text(size=18, face="bold", vjust=1),
          axis.title=element_text(size=16),
          axis.text.x=element_text(size=10, angle=90),
          axis.line=element_line(colour="black", size=.2),
          legend.background = element_rect(fill="transparent"),
          legend.position="top",
          legend.title=element_blank(),
          legend.margin=unit(-0.6, "cm"),
          legend.position="none",
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
  
  gt <- ggplot_gtable(ggplot_build(p)) #Takes existing plot and stores into a gtable (programmable structure)
  gt$layout$clip[gt$layout$name == "panel"] <- "off" #Disables clipping
  grid.arrange(gt)
  }
dev.off()
require(gridExtra)

print(g)


#################################################
#######Test plot
#################################################

p=ggplot(data=subset(df.long, df.long$locale %in% c("Spokane","Baltimore","Seattle")),
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
        legend.background = element_rect(fill="transparent"),
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

gt <- ggplot_gtable(ggplot_build(p)) #Takes existing plot and stores into a gtable (programmable structure)
gt$layout$clip[gt$layout$name == "panel"] <- "off" #Disables clipping
grid.draw(gt, recording=TRUE)

#################################################
#################################################
#################################################