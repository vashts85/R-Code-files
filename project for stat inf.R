 # The Effect of Vitamin C on Tooth Growth in Guinea Pigs
# 
# Description
# 
# The response is the length of odontoblasts (teeth) in each of 10 guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid).


df=data.frame(ToothGrowth)
require(ggplot2)

str(df)

by(df$len, df$supp, mean)
by(df$len, df$dose, mean)

ggplot(df,aes(x= len, ymax=max(..count..)))  + 
  stat_bin(binwidth=5, aes(fill=..count..), colour="black") + 
  stat_bin(binwidth=5, geom="text", aes(label=..count..), vjust=-1.5) +
  facet_grid(.~supp)+
  ylim(c(0,15))+
  theme_bw()

ggplot(data=df, aes(x=dose, y=len, group=supp, colour=supp))+
  geom_point(shape=1,size=5)+
  facet_grid(.~supp,scales="free")+
  labs(x="Dosage level") +
  labs(y="Tooth length by supplements type") +
  labs(title="Tooth growth distribution by supplements type") +
  geom_smooth(method="lm")+
  theme_bw()


t.test(len ~ supp, data=df)

dose1<-subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,1.0))
dose2<-subset(ToothGrowth, ToothGrowth$dose %in% c(2.0,1.0))
dose3<-subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,2.0))

p1=t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose1)$p.value
p2=t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose2)$p.value
p3=t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose3)$p.value

table(c("test",p1,p2,p3))

t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose1)$statistic

smoke <- matrix(c(format(round(t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose1)$p.value,2), nsmall=2),
                  format(round(t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose2)$p.value,2), nsmall=2),
                  format(round(t.test(len ~ dose, paired=FALSE, var.equal=FALSE, data=dose3)$p.value,2), nsmall=2)
                                      ),ncol=3,byrow=TRUE)
colnames(smoke) <- c("Test 1: 0.5 vs 1.0","Test 2: 2.0 versus 1.0","Test 3 0.5 versus 2.0")
rownames(smoke) <- c("P-value")
smoke <- as.table(smoke)
smoke



