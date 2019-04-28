# file.edit("/Users/helenchang/Mirror/code/auxiliary/create.proj.R")

source("../auxiliary/mylib.R")
mylib(c("data.table", "magrittr", "summarytools", "ggplot2", "dplyr", "corrplot", 
        "tibbletime", "anomalize", "reshape2", "pracma"))
dt <- fread("data/raw_input/AirQualityUCI.csv", dec=",")
dt <- dt[, -c(16, 17)][Date!=""]
tm <- as.POSIXct(strptime(paste(dt$Date, dt$Time, sep=" "), format = "%d/%m/%Y %H.%M.%S", tz="GMT"))
dt[, datetime:=tm]
dt <- dt[, -c(1, 2)]
# difftime(max(tm, na.rm = T),min(tm, na.rm = T), "days", tz="CST")
setorder(dt, datetime)
setnames(dt, colnames(dt), c("CO_GT", "CO", "NMHC_GT", "C6H6_GT", "NMHC",
                             "NOx_GT", "NOx", "NO2_GT", "NO2", "O3", "T", "RH", 
                             "AH", "datetime"))
dt <- dt[, .(CO, CO_GT, NMHC, NMHC_GT, NO2, NO2_GT, NOx, NOx_GT, O3, C6H6_GT, T, RH, AH, datetime)]

for (j in seq_len(ncol(dt)))
  set(dt,which(dt[[j]]==-200),j,NA)
dfs.raw <- dfSummary(dt)
view(dfs.raw)
print(dfs.raw, file = sprintf("./%s.html", "dtSummary.raw"))

for (j in seq_len(ncol(dt)))
  set(dt,which(is.na(dt[[j]])),j,median(dt[[j]], na.rm=T))
# TBN_CIF (customer info)----
dfs <- dfSummary(dt)
# view(dfs)
print(dfs, file = sprintf("./%s.html", "dtSummary"))

dt.no.na <- select(dt, -NMHC_GT) %>% na.omit()
nrow(dt.no.na)
M <- select(dt.no.na, -datetime) %>% cor(.)
pdf(file = "./plots/corr.na.omit.pdf")
corrplot.mixed(M, lower.col = "black", tl.cex=0.5)
dev.off()

dtn <- as_tbl_time(dt, index=datetime)

ts1 <- time_decompose(dtn, target=CO, frequency = "monthly", trend = "monthly") %>% 
  anomalize(remainder) %>%
  time_recompose()
ts1 %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)

p1 <- ts1 %>%
  plot_anomaly_decomposition() +
  ggtitle("all")
p1

# write.csv(dt, "data/processed/dt.csv", row.names = F)

tmp <- round(movavg(dt$CO_GT*24.45/28, 8, "s"), 1)
aqi.intv <- c(0, 51, 101, 151, 201, 301, 401)
br <- c(0, 4.5, 9.5, 12.5, 15.5, 30.5, 40.5, 50.4)
target <- tmp
idx1 <- as.integer(cut(target, br))
co <- ((aqi.intv[idx1+1]-aqi.intv[idx1])*(target-br[idx1]))/(br[idx1+1]-br[idx1]) + aqi.intv[idx1]

tmp <- round(movavg(dt$NO2_GT*24.45/46, 8, "s"), 1)
br <- c(0, 54, 101, 361, 651, 1250, 1650)
target <- tmp
idx2 <- as.integer(cut(target, br))
no2 <- ((aqi.intv[idx2+1]-aqi.intv[idx2])*(target-br[idx2]))/(br[idx2+1]-br[idx2]) + aqi.intv[idx2]


# dt1 <- data.table(datetime=dt$datetime, co=co, no2=no2, T=dt$T, RH=dt$RH, AH=dt$AH)
# nrow(dt1)
# M1 <- select(dt1, -datetime) %>% cor(.)
# corrplot.mixed(M1, lower.col = "black", tl.cex=0.5)
dt1 <- data.table(datetime=dt$datetime, co=idx1, no2=idx2) %>% na.omit()
nrow(dt1)
M1 <- select(dt1, -datetime) %>% cor(.)
corrplot.mixed(M1, lower.col = "black", tl.cex=0.5)
table(dt1[, -1])

summary(dt$CO_GT)
plot(density(dt$CO_GT))


plot(density(dt$T))
plot(density(dt$RH))
plot(density(dt$AH))

plot(dt$CO_GT)
plot(dt$T)
plot(dt$RH)
plot(dt$AH)

plot(dt$CO_GT)
plot(dt$NMHC_GT)
plot(dt$NO2_GT)
plot(dt$NOx_GT)
plot(dt$C6H6_GT)
plot(dt$O3)


dt2 <- melt.data.table(dt[, .(datetime, O3, NOx, NMHC)], id.vars="datetime")
dt2[, value.smooth8:=movavg(value, 8, "s")]
target.tm <- as.Date("2005-02-21")
ggplot(dt2[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value.smooth8,colour=variable,group=variable)) + geom_line()
ggplot(dt2[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()

dt4 <- dt[datetime>=target.tm&datetime<=target.tm+3, .(datetime, O3, NOx, NMHC)] %>% na.omit()
M1 <- select(dt4, -datetime) %>% cor(.)
corrplot(M1)
corrplot.mixed(M1, lower.col = "black", tl.cex=0.5)

dt5 <- dt[, .(datetime, O3, NOx_GT)] %>% na.omit()
M1 <- select(dt5, -datetime) %>% cor(.)
corrplot(M1)
corrplot.mixed(M1, lower.col = "black", tl.cex=0.5)

dt5 <- dt[, .(datetime, CO_GT, NO2_GT, NOx_GT, C6H6_GT)] %>% na.omit()
dt5[, `:=` (NO2_GT=NO2_GT/100,
            NOx_GT=NOx_GT/100,
            C6H6_GT=C6H6_GT/20)]
# dt5[, `:=` (NO2_GT=as.numeric(NO2_GT),
#             NOx_GT=as.numeric(NOx_GT))]
nrow(dt5)
M1 <- select(dt5, -datetime) %>% cor(.)
corrplot.mixed(M1, lower.col = "black", tl.cex=0.8)
dt6 <- melt.data.table(dt5, id.vars="datetime")
target.tm <- as.Date("2004-06-20")
ggplot(dt6[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()


dt3 <- melt.data.table(dt[, .(datetime, NOx_GT, NMHC_GT)], id.vars="datetime")
dt3[, value.smooth8:=movavg(value, 8, "s")]
target.tm <- as.Date("2004-04-23")
ggplot(dt3[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value.smooth8,colour=variable,group=variable)) + geom_line()
ggplot(dt3[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()



dt[is.na(CO_GT)&!is.na(CO)]

dt7 <- dt[, .(datetime, CO, CO_GT)] %>% na.omit()
nrow(dt7)
M1 <- select(dt7, -datetime) %>% cor(.)
corrplot.mixed(M1, lower.col = "black", tl.cex=0.8)
dt6 <- melt.data.table(dt5, id.vars="datetime")
target.tm <- as.Date("2004-06-20")
ggplot(dt6[datetime>=target.tm&datetime<=target.tm+3],aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()

target.tm <- as.Date("2004-03-20")
dtn <- as_tbl_time(dt7[datetime>=target.tm&datetime<=target.tm+100], index=datetime)
ts1 <- time_decompose(dtn, target=CO_GT, 
                      frequency = "monthly", trend = "monthly") %>% 
  anomalize(remainder) %>%
  time_recompose()
ts1 %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)

p1 <- ts1 %>%
  plot_anomaly_decomposition() +
  ggtitle("all")
p1




plot((dt$CO^2), dt$CO_GT)
dt7 <- dt[, .(datetime, CO, CO_GT, NMHC, C6H6_GT)] %>% na.omit()
dt7[, CO_sq:=(CO/1000)^2]
linearMod <- lm(CO_GT ~ CO_sq + C6H6_GT, data=dt7)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
dt[, CO_sq:=(CO/1000)^2]
distPred <- predict(linearMod, dt[is.na(dt$CO_GT)&!is.na(dt$CO), .(CO_sq, C6H6_GT)])
dt[is.na(CO_GT)&!is.na(CO), CO_GT:=distPred]

nrow(dt.no.na)
dt.no.na <- select(dt, -NMHC_GT) %>% na.omit()
M <- select(dt.no.na, -datetime) %>% cor(.)
corrplot.mixed(M, lower.col = "black", tl.cex=0.5)






plot((dt$CO), dt$NO2_GT)
plot((dt$CO_GT), dt$NO2_GT)
plot((dt$CO_GT)^(1/2), dt$NO2_GT)
plot((dt$NO2), dt$NO2_GT)
plot((dt$NOx), dt$NO2_GT)
plot((dt$NOx)^(-1/2), dt$NO2_GT)
plot((dt$NOx_GT), (dt$NO2_GT))
plot((dt$NOx_GT)^(1/2), (dt$NO2_GT))
plot((dt$C6H6_GT), dt$NO2_GT)
plot((dt$C6H6_GT)^(1/2), dt$NO2_GT)
plot((dt$NMHC), dt$NO2_GT)
dt7 <- dt[, .(datetime, CO, CO_GT, NO2, NO2_GT, NOx, NOx_GT, C6H6_GT, NMHC)] %>% na.omit()
dt7[, `:=` (CO_GT=(CO_GT)^(1/2),
            NOx=NOx^(-1/2),
            NOx_GT=NOx_GT^(1/2),
            C6H6_GT=C6H6_GT^(1/2))]
linearMod <- lm(NO2_GT ~ NO2+NOx+NOx_GT+C6H6_GT+NMHC+CO_GT+CO, data=dt7)  # build linear regression model on full data
print(linearMod)
summary(linearMod)


nrow(dt.no.na)
dt.no.na <- select(dt, -NMHC_GT) %>% na.omit()
M <- select(dt.no.na, -datetime) %>% cor(.)
corrplot.mixed(M, lower.col = "black", tl.cex=0.5)



target.tm <- as.Date("2004-04-01")
dtt <- melt.data.table(dt[, .(datetime, CO_GT*10, NO2_GT, NOx_GT, C6H6_GT)], id.vars="datetime")
ggplot(dtt[datetime>=target.tm&datetime<=target.tm+3],
       aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()

aust <- ts(dt$CO.index[1:8736], frequency=24)
aust <- window(aust,start=1)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

fc <- hw(subset(aust,end=length(dt$CO.index)-1800),
         damped = TRUE, seasonal="multiplicative", h=1800)
autoplot(aust, include=1000) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))


x <- aust %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) 

aust %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()
fit <- stl(aust, t.window=24, s.window="periodic",
           robust=TRUE)
# fit %>% seasadj() %>% naive() %>%
#   autoplot(include=100) + ylab("New orders index") +
#   ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive", h=120) %>%
  autoplot(include=800) + ylab("New orders index") #+ geom_line(aes(x=370:(371+nrow(dt), y=ts(dt$CO.index[8737:nrow(dt)], start=8377)))
# fcast <- stlf(aust, method='naive')



# hold <- window(ts(t.data), start=8761)
# #, order = c(12, 0, 5)
# mod1 <- stl(ts(aust[-c(8761:9357)]))
# pred1 <- forecast(mod1,h=9357-8761)
# plot(pred1)
# lines(ts(t.data))



dt$CO.index %>% diff() %>% ggtsdisplay()
Box.test(diff(dt$CO.index), lag=1, type="Ljung-Box")
dt$CO_GT %>% diff() %>% ggtsdisplay()
Box.test(diff(dt$CO_GT), lag=24, type="Ljung-Box")

mylib("urca")
dt$CO.index %>% ur.kpss() %>% summary()
aust %>% diff() %>% ur.kpss() %>% summary()
ndiffs(aust)

dt$CO.index %>% ggtsdisplay()
dt$CO_GT %>% ggtsdisplay()


aust %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

fit1 <- auto.arima(aust, seasonal=FALSE,
                   stepwise=FALSE, approximation=FALSE)
fit1 # order = c(3,1,2)
checkresiduals(fit1)
fit1$aicc
fit2 <- Arima(aust, order=c(3,1,3))
fit2$aicc
fit3 <- Arima(aust, order=c(3,1,1))
fit3$aicc
fit4 <- Arima(aust, order=c(4,1,2))
fit4$aicc
fit5 <- Arima(aust, order=c(2,1,2))
fit5$aicc
# fit2 <- auto.arima(dt$CO_GT, seasonal=FALSE,
#                    stepwise=FALSE, approximation=FALSE)
# fit2

aust %>%
  Arima(order=c(3,1,2), seasonal=list(order=c(0,1,0), period=167)) %>%
  residuals() %>% ggtsdisplay()

ggAcf(dt$CO.index,main="", lag.max=200)
ggPacf(dt$CO.index,main="", lag.max=200)

ggAcf(dt$CO_GT,main="")
ggPacf(dt$CO_GT,main="")

dt$CO_GT %>%
  Arima(order=c(3,1,2), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

fit <- Arima(dt$CO.index, order=c(3,0,1), seasonal=c(0,1,2),
             lambda=0)
checkresiduals(fit, lag=24)

(fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,0),
              lambda=0))

dt$CO.index %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast(h=120) %>%
  autoplot(include = 1000) +
  ylab("H02 sales (million scripts)") + xlab("Year")


dt$CO_GT %>% diff(lag=24) %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally differenced H02 scripts")





t.data <- dt$CO.index[1:length(dt$CO.index)]

T = 1:length(t.data)
reg=lm(t.data~ T)
plot(T,t.data,type="l")
abline(reg,col="red")
Y=residuals(reg)
acf(Y,lag=200,lwd=3)

Z=diff(Y,24)
acf(Z,lag=500,lwd=3)

Z1=diff(Z,168)
acf(Z1,lag=500,lwd=3)

pacf(Z,lag=36,lwd=3)


hold <-window(ts(t.data), start=8761)

mod1<- stats::arima(ts(t.data[-c(8761:9357)]), order = c(2, 1, 0),
                    seasonal = list(order = c(0, 1, 0),
                                    period=167))
mod1
pred1 <- forecast(mod1,h=9357-8761)

plot(pred1)
# lines(ts(t.data))
E1=residuals(mod1)
acf(E1,lag=100,lwd=3)








ts <- ts(na.omit(dt$CO.index))
adf.test(ts, alternative = "stationary")
Acf(ts, main='')
Pacf(ts, main='')
mod <- auto.arima(ts, seasonal = F, test="adf", ic="aic")
mod
plot(forecast(mod, h=120))
mod2 <- Arima(ts, order=c(12, 0, 1))
plot(forecast(mod2, h=120))
mod3 <- Arima(ts, order=c(12, 0, 12))
plot(forecast(mod3, h=120))

tsdisplay(residuals(mod), lag.max=45, main='(4,0,0) Model Residuals')
Box.test(mod$residuals)
per <- periodogram(as.numeric(na.omit(dt$CO_GT)))
data.table(period=1/per$freq, spec=per$spec)[order(-spec)][1:5]
bestfit <- list(aic=mod$aic, p=0, q=0, fit=mod)
for (i in 1:3) {
  for (j in 1:3) {
    z1 <- fourier(ts(dt$CO_GT, frequency=24), K=i)
    z2 <- fourier(ts(dt$CO_GT, frequency=12), K=j)
    fit <- auto.arima(dt$CO_GT, xreg=cbind(z1, z2), seasonal = F)
    if (fit$aic < bestfit$aic) {
      bestfit = list(aic=fit$aic, p = i, q = j, fit=fit)
    }
    cat(c(z1[1], z2[1], "\n"))
  }
}
bestfit
# bestfit1 <- bestfit
bestfit$fit
plot(forecast(bestfit$fit, h=120))
