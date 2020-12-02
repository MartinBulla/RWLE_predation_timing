# DATA Checking, EXPLORATION & SAMPLE SIZES

# TOOLS and prepare DATA
    require(here)
    source(here::here('R/tools.R')) 
    source(here::here('R/prepare_data.R'))
    y[, first_j := as.numeric(strftime(first_egg,format="%j"))]
    y[, start_j := as.numeric(strftime(start_expo,format="%j"))]
    y[, end_j := as.numeric(strftime(end_expo,format="%j"))]

    o =fread("Data/logger_data.txt")
    o[, start_j := as.numeric(strftime(datetime_start,format="%j"))]
    o[, end_j := as.numeric(strftime(datetime_end,format="%j"))]

    source(here::here('R/prepare_logger_overlaps.R')) # runs for some time
    w = data.table(logger_lengths)

# CHECKING   
    nrow(y[start_expo==end_expo])
    xx = y[start_expo==end_expo]
    xtabs(~xx$fate+xx$end_type)

    nrow(y[start_expo==last_ok])
    nrow(y[start_expo==last_ok & fate%in%c(0,1)])

    nrow(y[start_expo!=last_ok & fate%in%c(0) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 10])
    nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 10])
    nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 5])
    nrow(y[start_expo!=last_ok & fate%in%c(0,1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 5])
# SAMPLE SIZES - Methods section of the paper
    nrow(y) # number of nests
    length(unique(o$nest)) # number of nests with some logger that continuously recorded

    nrow(y[fate == 1]) # number of hatched nests
    # ADD chick found on or around the nest
    nrow(y[end_type%in%c('hde')])# based on estimated hatching

    nrow(y[end_type%in%c('logger_min1day', 'visit_min1day')]) # hatched a day ago
    nrow(y[end_type%in%c('logger_minhalfday', 'visit_minhalfday')]) # hatched 0.5day ago
    nrow(y[end_type%in%c('hde')]) # hatched on estimated hatch date
    nrow(y[end_type%in%c('found_at_hatching')]) # hatched on estimated hatch date

    nrow(y[fate %in% c(2,5) & end_type%in%c('visit', 'logger')]) # exposure finished with last_ok visit

    nrow(y[fate == 0]) # number of predated nests
    nrow(y[fate == 0 & end_type%in%c('logger', 'logger_min1day', 'logger_minhalfday')]) # predated nests with precise logger based end
    # ADD nests found empty
    # ADD predate eggs
    # ADD some eggs missing and some abandoned
    nrow(y[end_type%in%c('half_rule')]) # predated at midpoint between last_ok and last_visit
    nrow(y[end_type%in%c('last_ok+1')]) # predated last_ok +1 day

    # # number of  followed nests in each day of the season 
        #max(y$end_j)-min(y$first_j, na.rm=T)
        max(y$end_j)-min(y$start_j, na.rm=T)
        l = list()
        for(i in min(y$start_j):max(y$end_j)){
            l[[i]] = data.table(day = i, recorded =  length(y[start_j<=i & end_j>=i, unique(nest)]))
        }
        yo = do.call(rbind, l)
        summary(yo)
        ggplot(yo, aes(x = recorded)) + geom_histogram()
        ggplot(yo, aes(x = day, y = recorded)) + geom_point()

    # number of continuously followed nests in each day of the season when nests were recorded
        l = list()
        for(i in 59:234){
            l[[i]] = data.table(day = i, recorded =  length(o[start_j<=i & end_j>=i, unique(nest)]))
        }
        co = do.call(rbind, l)
        summary(co)
        ggplot(co, aes(x = recorded)) + geom_histogram()
        ggplot(co, aes(x = day, y = recorded)) + geom_point()

# SAMPLE SIZES - Results section of the paper   
  # all
    nrow(y) # N nests
    sum(round(y$exposure))  # number of days followed
    round(median(y$exposure)); round(mean(y$exposure));  round(range(y$exposure))
    
 
    nrow(y[fate == 0]) # number of predated nests
    nrow(y[fate == 1]) # number of hatched nests
    nrow(y[fate == 2]) # number of failed nests        
    nrow(y[fate == 2]) # number of failed for other reason 
    nrow(y[fate == 5]) # number with unknown fate

  # continuously monitored
    length(unique(o$nest)) # N nests
    ww = w[  , .(days_followed = sum(days)), by = nest]
    summary(ww$days) # N days each nest was continuously monitored
    round(sum(ww$days)) # N days continuously monitored

    yl = y[nest%in%unique (o$nest)]
    nrow(y[fate == 0 & end_type == 'logger']) # number of predations while continuous monitoring running on the nest
      

    


  summary(factor(y$fate))
  summary(factor(y$end_type))
  xtabs(~y$fate+y$end_type)  

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

