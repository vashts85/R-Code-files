x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp

sum(x*p)

sens=.75 #Test is positive and correct P(+|D)
spec=.52 #Test is false and correct  P(-|Dc)

#Want positive predictive value P(D|+)

(.75)*(.3)/(.75*.3+.48*.7)