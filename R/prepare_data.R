# logger based surface temperature data
a=fread("Data/temperatures.txt")
a[, year:=substring(date,1,4)]
a[, date_num:=as.numeric(strftime(date,format="%j"))]
a[, datetime_round := as.POSIXct(paste(date, hr), format = "%Y-%m-%d %H")]

y=fread("Data/data_final.txt")
y[, first_egg := as.POSIXct(first_egg)]
y[, start_expo := as.POSIXct(start_expo)]
y[, last_ok := as.POSIXct(last_ok)]
y[, last_control := as.POSIXct(last_control)]
y[, end_expo := as.POSIXct(end_expo)]
y[, exposure := as.numeric(difftime(end_expo, start_expo, units = 'days'))]
y[, mid_expo := mean(c(end_expo, start_expo)), by = nest]  
y[, mid_j :=as.numeric(strftime(mid_expo,format="%j"))]
y[, mid_age := as.numeric(difftime(mid_expo, first_egg, units = 'days'))]
y[, date := as.POSIXct(getDay(end_expo))]
y[, date_num := as.numeric(strftime(date,format="%j"))]

#y[fate == 0, fate := 7]
#y[fate == 1, fate := 0]
#y[fate == 7, fate := 1]

# dataset for timing of predation within a day
x = y[fate == 0 & logger_fate == 'yes']
x[, time := getime(end_expo)]
x$time_round=floor(x$time)
#x$temperature2 = a$median[match(as.character(round.POSIXt(x$end_expo,units="hours")),as.character(as.POSIXct(paste(a$date, a$hr), format = "%Y-%m-%d %H")))]
# add sunset and sunrise
    koord=SpatialPoints(cbind(55.36449,24.83803), proj4string=CRS("+proj=longlat +datum=WGS84")) # center of the study area
    x$sunrise=crepuscule(crds = koord,x$date,direction="dawn",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
    x$sunset=crepuscule(koord,x$date,direction="dusk",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
    x$sunrise_num=(as.numeric(x$sunrise) %% (24*3600))/3600 
    x$sunset_num=(as.numeric(x$sunset) %% (24*3600))/3600 

    # dataset with sunsets and sunrises for all dates in a season
        #x$date[x$date_num==min(x$date_num)]
        #x$date[x$date_num==max(x$date_num)]
       
        dats=seq.POSIXt(from=as.POSIXct("2019-03-08 UTC"),
                        to=as.POSIXct("2019-07-14 UTC"),by=24*3600)
        sunrs=crepuscule(koord,dats,direction="dawn",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
        sunss=crepuscule(koord,dats,direction="dusk",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
        sunrs=(as.numeric(sunrs) %% (24*3600))/3600 
        sunss=(as.numeric(sunss) %% (24*3600))/3600 

        ss = data.table(date = dats, sunrs = sunrs, sunss = sunss)
        ss[,date_num := as.numeric(strftime(date,format="%j"))]
        #ggplot(ss, aes(x = sunrs, y = date_num)) + geom_path()

# add whether nest was depredated during day/night   
    x$night=as.factor(ifelse(x$time>= x$sunrise_num & x$time <=x$sunset_num,"day","night" ))
    x$night_num = ifelse(x$night == 'day', 0, 1)

# add midday and midnight temperature
    am = a[hr == 12, list(date,mean)]
    setnames(am, 'mean', 'midday_T')
    am[, date := as.POSIXct(date)]
    x = merge(x,am, all.x = TRUE, by = 'date')
    
    # add missing temperature as average from the earliest previous and next day midday temperatures
    x[is.na(midday_T), midday_T := a[date_num %in% c(145,148) & hr == 12 & year == 2019, mean(mean)]]

# add time from sunrise and aggregate data
  x$time_from_sunrise=x$time-x$sunrise_num
  x$time_from_sunrise = ifelse(x$time_from_sunrise>12,x$time_from_sunrise-24, x$time_from_sunrise)
  
  x[, time_from_sunrise_r := round(time_from_sunrise)]
  ts = x[, .(cases = .N), by = 'time_from_sunrise_r']  
  td = x[, .(cases = .N), by = 'time_round'] 