rm(list=ls())

require(scales)
require(reshape2)
require(ggplot2)
require(plotly)
require(grid)
require(gridExtra)
library(R2PPT)


setwd("C:/Users/rcarvalho/Desktop/R-Code-Files/")

df=read.csv("ResponseRates_Running (6-5-15).csv")

# Labels for the MEPS
{
meps_labels<-c("ALBANY"=1,
               "BALTIMORE"=2,
               "BOSTON"=3,
               "BUFFALO"=4,
               "NEW YORK "=5,
               "HARRISBURG"=6,
               "FORT DIX"=10,
               "PITTSBURGH"=11,
               "PORTLAND, ME"=12,
               "SPRINGFIELD"=13,
               "SYRACUSE"=14,
               "TAMPA"=17,
               "ATLANTA"=20,
               "BECKLEY"=21,
               "CHARLOTTE"=22,
               "MIAMI"=23,
               "FORT JACKSON"=24,
               "JACKSONVILLE"=25,
               "KNOXVILLE"=26,
               "LOUISVILLE"=27,
               "MONTGOMERY"=28,
               "NASHVILLE"=29,
               "SAN JUAN"=30,
               "RALEIGH"=31,
               "FORT LEE"=32,
               "ALBUQUERQUE"=36,
               "AMARILLO"=37,
               "DALLAS"=38,
               "DENVER"=39,
               "EL PASO"=40,
               "HOUSTON"=41,
               "JACKSON"=42,
               "KANSAS CITY"=43,
               "LITTLE ROCK"=44,
               "MEMPHIS"=45,
               "NEW ORLEANS"=46,
               "OKLAHOMA CITY"=47,
               "SAN ANTONIO"=48,
               "SHREVEPORT"=49,
               "LANSING"=50,
               "CHICAGO"=54,
               "CLEVELAND"=56,
               "COLUMBUS"=57,
               "DES MOINES"=58,
               "DETROIT"=59,
               "FARGO"=60,
               "INDIANAPOLIS"=61,
               "MILWAUKEE"=62,
               "MINNEAPOLIS"=63,
               "OMAHA"=64,
               "SIOUX FALLS"=65,
               "ST. LOUIS "=66,
               "SAN DIEGO"=67,
               "BOISE"=70,
               "BUTTE"=71,
               "SACRAMENTO"=72,
               "HONOLULU"=73,
               "LOS ANGELES"=74,
               "SAN JOSE"=75,
               "PHOENIX"=76,
               "PORTLAND, OR"=77,
               "SALT LAKE CITY"=78,
               "SEATTLE"=79,
               "SPOKANE"=80,
               "ANCHORAGE"=81)
}

### Labeling the MEPS and correcting capitalization
{
df$meps_labels <- names(meps_labels)[match(df$MEPSID, meps_labels)]
df$meps_labels <- as.factor(df$meps_labels )


capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

levels(df$meps_labels)<-capwords(tolower(levels(df$meps_labels)))
levels(df$meps_labels)=sub("Portland, Or", "Portland, OR", levels(df$meps_labels))
levels(df$meps_labels)=sub("Portland, Me", "Portland, ME", levels(df$meps_labels))
}

### Labeling the Services
df$ServiceID<-factor(as.factor(df$ServiceID), labels=c("Army", "Navy", "Marines","Air Force"))

### Calculating Response Rates
df$RR <- round(df$CompletedSurvey/df$TotalSurvey*100)
df$RR[df$SurveyReturnedMonth==7 & df$SurveyReturnedYear==2014 & df$MEPSID==42 & df$RR=="NaN"] <- 0 #One location has 0 completes and 0 totals

# Fixing what look like data entry errors (Switched columns)
df$RR[df$RR>100] <- (df$TotalSurvey/df$CompletedSurvey*100)[df$RR>100]

### Calculating time variables for chart
{
df$Time=paste(df$SurveyReturnedYear,"01",df$SurveyReturnedMonth, sep="")
df$Time=as.Date(df$Time, format="%Y%d%m", tz="")
df$Year=format(df$Time, "%Y")
}


### Calculating time variables for chart
df$hjust <- ave(as.numeric(df$Time),df$Year,
                FUN=function(x)
                  ifelse(x==min(x),0.1,ifelse(x==max(x),0.8,0.4)))


### Chart macro
mypres <- PPT.Open(file=paste(getwd(),"test.pptx",sep="/"),method="RDCOMClient")

meps_seq=paste(seq(1,65, by=3),":",seq(1,65, by=3)+2, sep="")

for (i in unique(df$meps_labels)) {
  p<<- ggplot(data=df[df$meps_labels==i,],
       aes(x=Time, y=RR, colour=ServiceID, group=ServiceID, label=round(RR)))+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  labs(y="Response Rate")+
  coord_cartesian(ylim=c(0, 110))+
  geom_line(size=.5)+
  geom_point()+
  scale_color_manual(values=c("green4","blue4","red4","dodgerblue"))+
  ggtitle(paste(i,": Historical Response Rates", sep=" "))+
  theme(plot.title=element_text(size=18, face="bold", vjust=1),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=10, angle=90),
        axis.line=element_line(colour="black", size=.2),
        legend.background = element_rect(fill="transparent"),
        legend.position="top",
        legend.title=element_blank(),
        legend.margin=unit(-0.6, "cm"),
        legend.text=element_text(size=14),
        panel.grid.major.y=element_line(colour="gray", linetype="solid", size=.2))+ # or theme_blank())
  scale_x_date(labels = date_format("%b"), breaks=date_breaks("month"))+
  facet_grid(~Year, scales="free", space="free")
  

p2<<-ggplot(df[df$meps_labels==i,], aes(x = Time, y = ServiceID, label=format(round(RR), nsmall=0), 
                                        colour = ServiceID, hjust=hjust)) +
  geom_text(size = 3.5) +
      theme(
      panel.grid.major = element_blank(),
      legend.position = "none",
       panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(),
       axis.ticks = element_blank(),
      plot.margin = unit(c(-0.5,1, 0, 0.5), "lines")) +
  xlab(NULL) + 
  ylab(NULL)+
  scale_x_date(labels=c(), breaks=date_breaks("month"), expand=c(0.05,0.05))+
  facet_grid(~Year, scales="free", space="free_x")+
  scale_y_discrete(limits=rev(levels(df$ServiceID)))+
  scale_color_manual(values=c("green4","blue4","red4","dodgerblue"))

png("graph.png", units="in", width=14, height=7, pointsize=20, res=300)
grid.arrange(arrangeGrob(p,p2, 
             nrow=2, heights=c(5,1)))
dev.off()

mypres <- PPT.AddBlankSlide(mypres)
mypres <- PPT.AddGraphicstoSlide(mypres,file="graph.png", size= c(10, 100, 700, 400))
}
