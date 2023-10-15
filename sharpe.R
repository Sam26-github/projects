library(readxl)
Main_Price_Data <- read_excel("C:/Users/91998/OneDrive/Documents/NMIMS/MSc ASA sem2/Project/Main Price Data.xlsx")
View(Main_Price_Data)
return1<-Main_Price_Data[,c(1,7,8,9,10,11)]
View(return1)

class(return1)
tr<-ts(return1)
View(tr)
class(tr)
library(PerformanceAnalytics)
portfolio_return<-Return.portfolio(tr[,-1],weights=c(0.0413,0.2761,0.7416,0.026,-0.085))
head(portfolio_return)

portfolio_excess_return<-Return.excess(portfolio_return)
head(portfolio_excess_return)

sharpe_ratio_manual <- round(mean(portfolio_excess_return) / StdDev(portfolio_excess_return), 4)
sharpe_ratio_manual

VaR(
  tr[,-1],
  Rf = 0,
  p = 0.95,
  weights = c(0.0413,0.2761,0.7416,0.026,-0.085))
  
  

SharpeRatio(
  tr[,-1],
  Rf = 0,
  p = 0.95,
  FUN = c("ES"),
  weights = c(0.0413,0.2761,0.7416,0.026,-0.085),
  annualize = FALSE,
  SE = FALSE,
  SE.control = NULL)


SharpeRatio(
  tr[,-1],
  Rf = 0,
  p = 0.95,
  FUN = c("StdDev"),
  weights = c(0.0413,0.2761,0.7416,0.026,-0.085),
  annualize = FALSE,
  SE = FALSE,
  SE.control = NULL)

y<-0.08986^2
?SharpeRatio

SharpeRatio.modified(tr,Rf = 0,
                     p = 0.95,
                     FUN = c("VaR"),
                     weights = c(0.0413,0.2761,0.7416,0.026,-0.085),
                     annualize = FALSE,
                     SE = FALSE,
                     SE.control = NULL)

