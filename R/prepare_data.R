x=fread("Data/pred.csv")
    # column definitions
      # nest - unique nest ID
      # tnt:egg_temp - logger dostupnej pro dany hnizdo
      # date - date (day.month.year) of predation event
      # predation - hour (numeric) of predation event
      # quality - 1 = good, 2 = partial predation events, after which the nest was abandoned, 3 = poor (CO TO ZNAMENA); for the analyses 1 and 2 were used 
      # ..._time_correction - CO PRESNE TO ZNAMENA reseni nesrovnalosti s posunem casu - nutno k "predation" pricist maximum z nich

xx = x[grep("ok",x$remark)] # use only good data

# correct predation time where RFID was runninng on a wrong time 
    xx$time_corr=xx$predation+xx$rfid_time_correction
    xx$time_corr[xx$time_corr>24]=xx$time_corr[xx$time_corr>24]-24 # NEMELO BY SE TADY ZMENIT I DATUM?
    xx$time_corr_round=floor(xx$time_corr)
# add sunset and sunrise
    koord=SpatialPoints(cbind(55.36449,24.83803), proj4string=CRS("+proj=longlat +datum=WGS84")) # center of the study area

    xx$date=as.POSIXct(xx$date,format="%d.%m.%Y")
    xx$date_num=as.numeric(strftime(xx$date,format="%j"))
    xx$sunrise=crepuscule(koord,xx$date,direction="dawn",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
    xx$sunset=crepuscule(koord,xx$date,direction="dusk",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
    xx$sunrise_num=(as.numeric(xx$sunrise) %% (24*3600))/3600 
    xx$sunset_num=(as.numeric(xx$sunset) %% (24*3600))/3600 

    # dataset with sunsets and sunrises for all dates in a season
        xx$date[xx$date_num==min(xx$date_num)]
        xx$date[xx$date_num==max(xx$date_num)]
        max(xx$date)
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
    xx$night=as.factor(ifelse(xx$time_corr>= xx$sunrise_num & xx$time_corr <=xx$sunset_num,"day","night" ))
    xx$night_num = ifelse(xx$night == 'day', 0, 1)

# add time from sunrise and aggregate data
  xx$time_from_sunrise=xx$time_corr-xx$sunrise_num
  xx$time_from_sunrise = ifelse(xx$time_from_sunrise>12,xx$time_from_sunrise-24, xx$time_from_sunrise)
  
  xx[, time_from_sunrise_r := round(time_from_sunrise)]
  ts = xx[, .(cases = .N), by = 'time_from_sunrise_r']  
  td = xx[, .(cases = .N), by = 'time_corr_round'] 