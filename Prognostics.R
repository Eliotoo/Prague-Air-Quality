library(ggplot2)
library(skimr)
library(seasonal)
library(urca)

df <- data.frame(read.csv("**\\O3_monthly.csv", header = TRUE, sep = ","))
Pm10 <- ts(df[, 6], start = 2013, frequency = 12)
O3 <- ts(df[, 2], start = 2013, frequency = 12)
autoplot(O3) +
     ggtitle("Monthly Average O3 in Prague") +
       ylab("Concentration") +
       xlab("Year")

Pm10 %>% skim()
O3 %>% skim()

fit <- seas(Pm10, x11="")
autoplot(fit) +
      ggtitle("X11 decomposition of Pm10 in Prague")
fit <- seas(O3, x11="")
autoplot(fit) +
  ggtitle("X11 decomposition of O3 in Prague")

autoplot(Pm10, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("Concentration") +
  ggtitle("Pm10 in Prague in year 2013-2020") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
autoplot(O3, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("Concentration") +
  ggtitle("O3 in Prague in year 2013-2020") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

Pm10  %>% ur.kpss() %>% summary()
O3  %>% ur.kpss() %>% summary()
ndiffs(Pm10)
nsdiffs(Pm10)
ndiffs(O3)
nsdiffs(O3)

Pm10 %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
O3 %>% diff(lag=12) %>% ggtsdisplay()

O3 %>%
  auto.arima()%>%
  residuals() %>% ggtsdisplay()
Pm10 %>%
  Arima(order=c(1,0,1), seasonal=c(1,0,1))%>%
  residuals() %>% ggtsdisplay()
fit2 <- Arima(Pm10, order=c(1,0,1), seasonal=c(1,0,1))
fit3 <- auto.arima(O3)

fit2 %>% forecast(h=12) %>% autoplot()
fit3 %>% forecast(h=12) %>% autoplot()

