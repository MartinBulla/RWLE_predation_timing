# EXPLORATION and DATA Checking
require(here)
source(here::here('R/tools.R')) 
source(here::here('R/prepare_data.R'))

# date and temperature
    ggplot(xx, aes(y = date_num, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
    ggplot(xx, aes(x = date_num, y = midday_T)) + stat_smooth(method = 'lm') + geom_point()
    ggplot(xx, aes(x = date_num, y = midday_T, col = as.factor(year))) + stat_smooth(method = 'lm') + geom_point()

# (j) T during predation given midday temperature
    ggplot(xx, aes(y = temperature, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
    ggplot(xx, aes(y = temperature, x = date_num)) + stat_smooth(method = 'lm') + geom_point()

    m = lm(temperature ~ midday_T,data=xx)
    m = lm(temperature ~ date_num,data=xx)
    summary(m)


# checking final dataset
y=fread("Data/data_final.txt")

y[, first_egg := as.POSIXct(first_egg)]
y[, start_expo := as.POSIXct(start_expo)]
y[, last_ok := as.POSIXct(last_ok)]
y[, last_control := as.POSIXct(last_control)]
y[, end_expo := as.POSIXct(end_expo)]

# where start_expo same as last ok
nrow(y[start_expo==last_ok])
nrow(y[start_expo==last_ok & fate%in%c(0,1)])
nrow(y[start_expo!=last_ok & fate%in%c(0) & as.numeric(difftime(last_control,last_ok, units = 'days')) > 10])
nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_control,last_ok, units = 'days')) > 10])
nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_control,last_ok, units = 'days')) > 5])
nrow(y[start_expo!=last_ok & fate%in%c(0,1) & as.numeric(difftime(last_control,last_ok, units = 'days')) > 5])