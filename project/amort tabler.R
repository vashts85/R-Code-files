


amortization <- function(cost, downpayment, interest, term) {
  mortgage_table<<-data.frame()
  amort_value <<- (cost)*(1-downpayment/100)*(interest/1200)*((1+interest/1200)^(term*12))/((1+interest/1200)^(term*12)-1)
  print(sprintf("$%.2f", amort_value))         
  mortgage_table<<-data.frame(matrix(0, nrow = term*12, ncol = 3))
  
  
  principal<<-cost-downpayment/100*cost
  colnames(mortgage_table)<<-c("principal","pay_int","pay_p")
    mortgage_table[1,]<<-c(principal,
                         pay_int=principal*interest/1200,
                         pay_p=amort_value-principal*interest/1200)
  
  for (x in 1:((term*12)-1)) {
    y<<-x+1
    mortgage_table[y,]<<-c(principal = mortgage_table$principal[x]-mortgage_table$pay_p[x],
                           interest = (mortgage_table$principal[x]-mortgage_table$pay_p[x])*interest/1200, 
                           pay_p =amort_value-((mortgage_table$principal[x]-mortgage_table$pay_p[x])*interest/1200)) 
  }
  mortgage_table<<-cbind(mortgage_table,time=c(1:(15*12)))
  }
amortization(405000,20,3,15)

tail(mortgage_table)

library(ggplot2)
qplot(data=mortgage_table, x=time, y=pay_int)

ggplot(mortgage_table, aes(time)) + 
        geom_bar(stat="identity", aes(y = pay_p, colour = "red")) + 
        geom_bar(stat="identity", aes(y = pay_int, colour = "blue"))