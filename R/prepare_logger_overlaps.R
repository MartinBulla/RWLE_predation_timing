# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor:  Martin Sladecek
# 📍 This script runs relative to the project's root directory, and 
# generates periods for which each nest was continuously recorded with one 
# or more devices 
# ==========================================================================

logger_days=read.table("Data/logger_data.txt",sep=";",
                       header=T)

logger_days$datetime_start=as.POSIXct(logger_days$datetime_start)
logger_days$datetime_end=as.POSIXct(logger_days$datetime_end)
logger_days$logger=as.character(logger_days$logger)

# intersections calculations


logger_lengths={}
logger_bouts={}
#nest="5_2018" 
for (nest in unique(logger_days$nest)) {
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
#sum(logger_lengths$days)
#sum(logger_lengths$length)/(24*60)
#sum(as.numeric(logger_bouts$stop)-as.numeric(logger_bouts$start))/(24*3600)

write.table(logger_lengths,"logger_lengths.txt")
write.table(logger_bouts,"logger_bouts.txt")

