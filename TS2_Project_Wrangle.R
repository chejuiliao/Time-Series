library(tidyverse)
library(readxl)
library(lubridate)
library(imputeTS)
library(hms)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(haven)
library(utils)

# read in data and extract the hour portion of the hour
well0 <- read_excel("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/Well/G-2147_T.xlsx", sheet = "Well") %>%
  separate(time, into = c("a", "time"), sep = " ") %>%
  mutate(time = as.hms(time))

# paste the date and hour together and adjust for daylight savings 
well <- well0 %>%
  mutate(datetime = as.POSIXct(paste(date, time), tz = 'EST')) %>%
  mutate(datetime2 = if_else(tz_cd == 'EDT', datetime - 3600, datetime)) %>%
  mutate(datetime3 = floor_date(datetime2, "hour")) %>%
  group_by(datetime3) %>%
  summarise(Corrected = mean(Corrected)) %>%   # aggregating hourly
  mutate(hour = datetime3) %>%
  select(hour, Corrected)

# rain data ---------------------------------------------------------------

rain0 <- read_excel("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/well/G-2147_T.xlsx", sheet = "Rain")
# set the time zone as EST
attributes(rain0$Date)$tzone <- "EST"




View(rain0)
# assume the original data is EDT, need to adjust by 4 hour
rain0$Date <- rain0$Date + 4*60*60

rain0 <- rain0 %>%
  # retain the same period as well data
  filter(Date >= as.POSIXct('2007-10-09 22:59:59', tz='EST')) %>%
  #filter(Date >= as.POSIXct('2007-10-09 23:00.00', tz='UTC')) %>%
  # time needs to be adjusted by 1 minute
  mutate(Date = Date + 1)

# paste the date and hour together and adjust for daylight savings 
rain <- rain0 %>%
  mutate(datetime = floor_date(Date, "hour")) %>%
  group_by(datetime) %>%
  summarise(RAIN_FT = mean(RAIN_FT)) %>%   # aggregating hourly
  mutate(hour = datetime) %>%
  select(hour, RAIN_FT)


# tide data ---------------------------------------------------------------
# read in data and extract the hour portion of the hour
tide0 <- read_csv("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/Well Data/station_8722859.csv") %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), tz = 'EST'))

# assume the original data is EDT, need to adjust by 1 hour
tide0$datetime <- tide0$datetime - 1*60*60

# aggregate data by hour
tide <- tide0 %>%
  filter(datetime >= as.POSIXct('2007-10-09 22:59:59', tz='EST')) %>%
  mutate(datetime3 = floor_date(datetime, "hour")) %>%
  group_by(datetime3) %>%
  summarise(Prediction = mean(Prediction)) %>%   # aggregating hourly
  mutate(hour = datetime3) %>%
  select(hour, Prediction)

View(tide)
View(rain)
View(well)
# combine data ------------------------------------------------------------

# making a dataframe with all possible hours
allhours <- tibble(date = seq(
  from=as.POSIXct("2007-10-09 23:00.00", tz = 'EST'),
  to=as.POSIXct("2018-6-08 08:00.00", tz = 'EST'),
  by="hour"))
  
View(allhours)
# join with the well data to find missing hour values
well_m <- left_join(allhours, well, by = c("date" = "hour"))
sum(is.na(well_m$Corrected))

# join with rain data to find missing hour values
rain_m <- left_join(allhours, rain, by = c("date" = "hour"))
sum(is.na(rain_m$RAIN_FT))

# join with tide data to find missing hour values
tide_m <- left_join(allhours, tide, by = c("date" = "hour"))
sum(is.na(tide_m$Prediction))

# making time series objects and cleaning it
ts_well <- ts(well_m$Corrected, frequency = (365.25*24))
ts_well2 <- tsclean(ts_well)

ts_tide <- ts(tide_m$Prediction, frequency = (365.25*24))
ts_tide2 <- tsclean(ts_tide)

# building a data frame out of the clean time series and incorporate rain data
df_well_rt <- data.frame(date = seq(
  from=as.POSIXct("2007-10-09 23:00.00", tz = 'EST'),
  to=as.POSIXct("2018-06-08 08:00.00", tz = 'EST'),
  by="hour"),
  corrected = as.matrix(ts_well2), 
  rain = rain_m$RAIN_FT,
  tide = as.matrix(ts_tide2)
)

prediction_time <- df_well_rt$date[(nrow(df_well_rt)-167):nrow(df_well_rt)] + 60*60

df_well_rt$date <- df_well_rt$date - 60*60*4
View(df_well_rt)
# write out the data so I can use SAS to build models
write_csv(df_well_rt, 'C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/Well Data/df_well_rt.csv')


# model exploration -------------------------------------------------------

# take a look
ggplot(df_well_rt[5000:10000,], aes(date)) +
  geom_line(aes(y = corrected, colour = 'red')) +
  geom_line(aes(y = tide, colour = 'blue')) +
  geom_line(aes(y = rain*100, colour = 'yellow'))

df_reg <- df_well_rt

# regress corrected well_ft on rain_ft and tide
df_reg$r1 <- lag(df_well_rt$rain)
df_reg$corrected <- NULL
df_reg$date <- NULL
train <- df_reg[2:(nrow(df_reg)-168),]
y_train <- ts(df_well_rt$corrected[2:(nrow(df_well_rt)-168)], frequency = 24*365.25)

model1 <- Arima(y_train, order = c(3, 1 , 2), xreg = train, method = "ML")
summary(model1)
ndiffs(model1$residuals)

View(df_reg)

# take first order difference------------------------------------------------------
rain_dff <- diff(df_reg$rain)
tide_dff <- diff(df_reg$tide)
r1_dff <- diff(df_reg$r1)

df_reg_dff <- cbind(rain_dff, tide_dff, r1_dff)
train <- df_reg_dff[1:(nrow(df_reg)-168),]
y_train <- ts(df_well_rt$corrected[1:(nrow(df_well_rt)-168)], frequency = 24*365.25)

model1 <- Arima(y_train, order = c(3, 1 , 2), xreg = train, method = "ML")
summary(model1)

# ljung box test
p_val <- c()
for (i in 1:12){
  p_val <- c(p_val, Box.test(model1$residuals,lag=i,type='Ljung-Box')$p.value)
}
p_val

white_LB <- pmin(p_val, 0.2)
barplot(white_LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

Acf(model1$residuals, lag = 15)$acf
Pacf(model1$residuals, lag = 15)$acf

# p-value for the coefficients
(1-pnorm(abs(model1$coef)/sqrt(diag(model1$var.coef))))*2

# calculate MAPE
val <- df_reg[(nrow(df_reg) - 167):nrow(df_reg),]
y_val <- forecast(model1, xreg = val, h = 168)
pred <- y_val$mean
error <- df_well_rt$corrected[(nrow(df_reg) - 167):nrow(df_reg)] - pred
MAE <- mean(abs(error))
MAPE <- mean(abs(error) / abs(df_well_rt$corrected[(nrow(df_reg) - 167):nrow(df_reg)]))
MAPE

# store the actual and prediction value and date so we can use ggplot
df_prediction <- data.frame(cbind(hour = df_well_rt$date[(nrow(df_reg) - 167):nrow(df_reg)], predict = pred, actual = df_well_rt$corrected[(nrow(df_reg) - 167):nrow(df_reg)]))
df_prediction$hour <- as.POSIXct(df_prediction$hour, origin = "1960-01-01")

# plot the prediction
ggplot(df_prediction, aes(hour)) + 
  geom_point(aes(y = actual, colour = "Actual")) +
  geom_line(aes(y = predict, colour = "Prediction")) +
  xlab("Date") +
  ylab("Well ft") +
  labs(title = "Well ft Actual vs. Prediction", size = 15) +
  #ylim(c(min, max)) + 
  theme(legend.title = element_blank())





# use SAS data to plot ----------------------------------------------------

# read the training part of data
train_pred <- read_sas("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/Well Data/train.sas7bdat")

# read the validation part of data
df_val <- read_sas("C:/Users/Jerry/Documents/MSA18/Time_Series/HW/Fall2/Well Data/val.sas7bdat")
df_val$hour <- prediction_time

# white noise plot
p_val <- c()
for (i in 1:12){
  p_val <- c(p_val, Box.test(train_pred$RESIDUAL,lag=i,type='Ljung-Box')$p.value)
}
p_val

white_LB <- pmin(p_val, 0.2)
barplot(white_LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# plot the prediction
ggplot(df_val, aes(hour)) + 
  geom_line(aes(y = corrected, colour = "Actual")) +
  geom_line(aes(y = FORECAST, colour = "Prediction")) +
  xlab("Date") +
  ylab("Well ft") +
  labs(title = "Well ft Actual vs. Prediction", size = 15) +
  #ylim(c(min, max)) + 
  theme(legend.title = element_blank())