--- 
title       : Mortgage Payment Calculator
subtitle    : A Shiny app
author      : Ricardo Carvalho
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : prettify  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides

--- bg:#81BEF7

## <font color="white"> App Overview </font>

* blaashdsad
* This app calculates mortage rates reactively by taking in user inputs
* These inputs include cost of the mortgage, downpayment percentage, interest rate and term of the mortgage loan
* It does not, however, calculate and provide estimates mortgage insurance, real estate taxes, or homeowner's insurance

--- bg:#81BEF7

## <font color="white"> Features </font>

* Slider for downpayment percentage
* Slider for term of loan payment
* Instant calculations and onscreen explanations (along with amortization equation)
* App is set in showcase mode so users can see code

--- bg:#81BEF7

## <font color="white"> The Math and Code behind it </font>

* The application is instantly calculating the monthly payment via the following formula:

$$(\text{Principal})\frac{(\text{Interest rate})(1+\text{interest rate})^\text{Term}}{(1+\text{interest rate})^\text{Term}-1}$$


* An amortization table is also created on the back end but is not displayed so the results can remain streamlined

--- bg:#81BEF7

## <font color="white"> Why you want this app </font>



* Very <ins><b>easy</ins></b> to use!
* Results are instant and non-cumbersome!
* Instructions are displayed onscreen!



---

slidify("index.rmd")  
browseURL("index.html")
