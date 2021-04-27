# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor:  Martin Sladecek
# 📍 This script runs relative to the project's root directory, and 
# generates periods for which each nest was continuously recorded with one 
# or more devices (logger_lengths) and periods for which various logger
# combinations recorded at each nest (logger_bouts)
# ==========================================================================
#o=read.delim("logger_data.txt",sep=";")
#o =fread("logger_data.txt")

list.model.predictors=function(predictors,max_pred){
  x=rep(predictors,times=length(predictors)^(max_pred-1))
  xx=predictors
  for (pred in 2:max_pred) {
    xx=rep(predictors,each=length(xx))
    x=cbind(x,xx) 
  }
  unique_perm=unique(apply(t(apply(x,1,sort)), 1, paste,collapse=":"))# odstraneni replikativnich permutaci# odstraneni replikaci v ramci permutace
  unique_pred=unique(sapply(strsplit(unique_perm,split=":"),unique))# odstraneni replikaci v ramci permutace
  return(unique_pred)  
}

logger_days =read.delim("Data/logger_data.txt", sep = ' ')
logger_days$datetime_start=as.POSIXct(logger_days$datetime_start)
logger_days$datetime_end=as.POSIXct(logger_days$datetime_end)
#logger_days$logger=as.character(logger_days$logger)

# change time shift
logger_days$time_change=ifelse(substr(as.character(logger_days$datetime_start),1,4)=="2018",
                              as.numeric(strftime("2018-03-25 02:00:00",format="%j")),
                        ifelse(substr(as.character(logger_days$datetime_start),1,4)=="2019",
                              as.numeric(strftime("2019-03-31 02:00:00",format="%j")),
                              as.numeric(strftime("2020-03-29 02:00:00",format="%j"))))

logger_days$datetime_start=as.POSIXct(ifelse(as.numeric(strftime(logger_days$datetime_start,format="%j"))<logger_days$time_change,
                                logger_days$datetime_start-3600,
                                logger_days$datetime_start-7200),origin ="1970-01-01 00:00:00")
logger_days$datetime_end=as.POSIXct(ifelse(as.numeric(strftime(logger_days$datetime_end,format="%j"))<logger_days$time_change,
                                logger_days$datetime_end-3600,
                                logger_days$datetime_end-7200),origin ="1970-01-01 00:00:00")
logger_days$dat_end=y_$end_expo[match(logger_days$nest,y_$nest)]
logger_days$diff=difftime(logger_days$dat_end,logger_days$datetime_end,units = "secs")
sum(logger_days$diff[logger_days$diff<0])/(3600*24)
logger_days$datetime_end[logger_days$diff<0]=logger_days$datetime_end[logger_days$diff<0]+
                                              logger_days$diff[logger_days$diff<0]

#logger_days=logger_days[-which(logger_days$id_nest=="130_2019"& logger_days$datetime_start=="2019-06-09 07:30:00"),]
#logger_days=logger_days[logger_days$id_nest != "95_2019",]# logger pridan az do lihnouciho se hnizda

# intersections calculations 
  logger_lengths={}
  logger_bouts={}
  #nest="5_2018" 
  for (nest in unique(logger_days$nest)) {
    print(nest)
    x=logger_days[logger_days$nest==nest,]
    for (logger in unique(x$logger)) {
      xx=x[x$logger==logger,]
      #    assign(logger,sort(unlist(mapply(seq,from=as.numeric(xx$datetime_start) %/% 3600,
      #           to=as.numeric(xx$datetime_end) %/% 3600))))
      # zkouska s presnejsim zaokrouhlenim ale na hodiny
      ##    assign(logger,sort(unlist(mapply(seq,
      #                    from=as.numeric(round.POSIXt(xx$datetime_start,units = "hour"))%/% 3600,
      #           to=as.numeric(round.POSIXt(xx$datetime_end,units="hour")) %/% 3600))))
      # presne na vteriny/minty
      assign(logger,sort(unlist(mapply(seq,
                                       from=as.numeric(round.POSIXt(xx$datetime_start,units = "mins"))%/% 60,
                                       to=as.numeric(round.POSIXt(xx$datetime_end,units="mins")) %/% 60))))
    }
    if(length(unique(x$logger))==1){
      x3=data.frame(nest=nest,
                    name=unique(x$logger),
                    length=length(get(unique(x$logger))))
      logger_lengths=rbind(x3,logger_lengths)
      
      logger_bouts=rbind(logger_bouts,data.frame(nest=nest,
                    start=as.numeric(x$datetime_start)/60,
                    stop=as.numeric(x$datetime_end)/60))
      
      
    } else {
      x1=list.model.predictors(predictors = unique(x$logger),
                               max_pred = length(unique(x$logger)))
      x2={}
      for (i in 1:length(x1)) {
        if(length(unlist(x1[i]))==1){
          x2[[i]]=lapply(x1[i],get)
        }else {
          x2[[i]]=Reduce(intersect,lapply(unlist(x1[i]),get))
        }
      }
      
      logger_bouts=rbind(logger_bouts,data.frame(nest=nest,
                    start=tapply(unique(sort(unlist(x2))), 
                          cumsum(c(1,diff(unique(sort(unlist(x2)))))!=1), min),
                    stop=tapply(unique(sort(unlist(x2))), 
                          cumsum(c(1,diff(unique(sort(unlist(x2)))))!=1), max)))
      
      
      
      x2_2={}
      for(i in 1:length(x2)){
        x2_2[i]=length(setdiff(unlist(x2[i]),Reduce(union,x2[which(sapply(x1,length)>=length(x1[[i]]))[
          which(sapply(x1,length)>=length(x1[[i]]))!= i
          ]])))
        
      }
      
      #x2_2[which(sapply(x2,length)>0)]
      x3=data.frame(nest=nest,
                    name=unlist(sapply(x1[which(x2_2>0)],
                                       paste,collapse=",")),
                    length=x2_2[which(x2_2>0)])
      logger_lengths=rbind(x3,logger_lengths)
      
    }
    
    if(nrow(x)>1){print(nest)}
    
  }
  
  logger_lengths=logger_lengths[logger_lengths$length>0,]
  row.names(logger_lengths)=c(1:nrow(logger_lengths))
  logger_lengths$days=logger_lengths$length/(24*60)
  logger_lengths$nest=as.character(logger_lengths$nest)
  logger_lengths$year=substr(logger_lengths$nest,
                             nchar(logger_lengths$nest)-3,
                             nchar(logger_lengths$nest))
  


logger_bouts$start=as.POSIXct(logger_bouts$start*60,
                              origin = "1970-01-01 0:00:00")
logger_bouts$stop=as.POSIXct(logger_bouts$stop*60,
                              origin = "1970-01-01 0:00:00")
sum(logger_lengths$days)
sum(logger_lengths$length)/(24*60)
sum(as.numeric(logger_bouts$stop)-as.numeric(logger_bouts$start))/(24*3600)

#write.table(logger_lengths,"logger_lengths.txt")
#write.table(logger_bouts,"logger_bouts.txt")


#logger_bouts$dat_end=dat2$end_expo[match(logger_bouts$nest,dat2$nest)]
#logger_bouts$type_end=dat2$end_type[match(logger_bouts$nest,dat2$nest)]
#logger_bouts$diff=(logger_bouts$dat_end-logger_bouts$stop)
#sum(logger_bouts$diff[logger_bouts$diff<0])
#xx=ddply(logger_bouts,"nest",summarize,sum=sum(difftime(stop,start,units="days")))
#xx1=ddply(logger_lengths,"nest",summarize,sum=sum(days))
#xx$sum_len=xx1$sum[match(xx$nest,xx1$nest)]
#xx$diff=xx$sum_len-xx$sum
#head(xx)
#head(xx1)
#logger_days[logger_days$nest=="95_2019",]
#sum(xx$sum_len)

#logger_days[logger_days$nest=="144_2019",]
#logger_lengths[logger_lengths$nest=="144_2019",]
#logger_bouts[logger_bouts$nest=="144_2019",]
