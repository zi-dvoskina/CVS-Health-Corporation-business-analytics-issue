# Data Source:
# CVS Health Corp.GuruFocus.com. 
# https://www.gurufocus.com/stock/CVS/guru-trades

library(readxl)
daily_10years <- read_excel("daily_10years.xlsx")
plot(daily_10years$stock_price~daily_10years$t, 
     xlab="t", ylab="CVS stock price", main="CVS stock price vs t")
plot(daily_10years$stock_price~daily_10years$year, 
     xlab="year", ylab="CVS stock price", main="CVS stock price vs year")
model_stock_price_1 <- lm(stock_price~t+year+quarter+month+Aetna+COVID,data=daily_10years)
summary(model_stock_price_1)
model_stock_price_2 <- lm(stock_price~t+year+quarter+Aetna+COVID,data=daily_10years)
summary(model_stock_price_2)

daily_newdates <- read_excel("daily_newdates.xlsx")
daily_newdates$stock_price <- predict(model_stock_price_2, newdata=daily_newdates)
daily_with_forecast <- rbind(daily_10years,daily_newdates)
plot(daily_with_forecast$stock_price~daily_with_forecast$t, 
     xlab="t", ylab="CVS stock price", main="CVS stock price vs t with forecast")
plot(daily_with_forecast$stock_price~daily_with_forecast$year, 
     xlab="year", ylab="CVS stock price", main="CVS stock price vs year with forecast")

library(readxl)
quarerly_10_years <- read_excel("quarerly_10 years.xlsx")
plot(quarerly_10_years$diluted_EPS~quarerly_10_years$t, 
     xlab="t", ylab="diluted EPS", main="diluted EPS vs t")
plot(quarerly_10_years$diluted_EPS~quarerly_10_years$year, 
     xlab="t", ylab="diluted EPS", main="diluted EPS vs year")
