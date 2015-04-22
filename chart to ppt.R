require(rcom)
require(RDCOMClient)
require(R2PPT)

library(ggplot2)
p <- qplot(x=1,y=1)
print(p)

ggsave(my_temp_file<-paste(tempfile(),".wmf",sep=""), plot=p)

library(R2PPT)
mypres<-PPT.Init(method="RDCOMClient")
mypres <- PPT.AddBlankSlide(mypres)
mypres <- PPT.AddGraphicstoSlide(mypres,file=my_temp_file)
unlink(my_temp_file)


install.packages(c("rscproxy","rcom"),repos="http://rcom.univie.ac.at/download",lib=.Library)

library(rcom)

comRegisterRegistry()

installstatconnDCOM()