# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Dial nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla, Martin Sladecek
# 📍 This script runs relative to the project's root directory, generates
# information presented in the text (about sample sizes, distributions,
# effects) for Abstract, Methods and Results
# and contains data checks and further explorations
# ==========================================================================

# TOOLS and prepare DATA
    require(here)
    source(here::here('R/tools.R')) 

    nsim = 5000
    
    source(here::here('R/prepare_data.R'))
    y[, first_j := as.numeric(strftime(first_egg,format="%j"))]
    y[, start_j := as.numeric(strftime(start_expo,format="%j"))]
    y[, end_j := as.numeric(strftime(end_expo,format="%j"))]

    yy = y[exposure>0]
    yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
    yy[is.na(fate_), fate_:= 0]    # 0 all other
    yy[ , success := round(exposure - fate_)]
    yy[fate_==1, failure := 1]
    yy[is.na(failure), failure := 0]

    source(here::here('R/prepare_logger_overlaps.R')) # runs for some time
    w = data.table(logger_lengths)

    o =fread("Data/logger_data.txt")
    o[, start_j := as.numeric(strftime(datetime_start,format="%j"))]
    o[, end_j := as.numeric(strftime(datetime_end,format="%j"))]

# Outputs - Abstract
    nrow(y) # number of nests
    length(unique(o$nest)) # number of nests with some logger that continuously recorded
    round(sum(w$days)) # N days nests were continuously monitored

    nrow(y[fate == 1]) # number of hatched nests
    nrow(y[fate == 0]) # number of predated nests      
    nrow(y[fate == 2]) # number of failed for other reason 
    nrow(y[fate == 5]) # number with unknown fate

    
    # daily predation rate
        yy = y[exposure>0]
        yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
        yy[is.na(fate_), fate_:= 0]    # 0 all other
        yy[ , success := round(exposure - fate_)]
        yy[fate_==1, failure := 1]
        yy[is.na(failure), failure := 0]
        
        
        ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
        bsim = sim(ma, n.sim=nsim)  
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI

        plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100*30 # total predation rate

# Outputs - Methods
    length(unique(o$nest)) # N nests with some logger that continuously recorded
    length(unique(o[logger =='dvr', nest])) # N nests with video recording system
    length(unique(o[logger %in% c('tnt','dht', 'zajda_thm'), nest])) # N nests with temperature and temperature/humidity recording system
    length(unique(o[logger %in% c('rfid'), nest])) # N nests with RFID system from Max Planck
    length(unique(o[logger %in% c('zajda_rfid'), nest])) # N nests with system containing RFID, temperature and humidity logging from CZU
    length(unique(o[logger %in% c('lup_egg'), nest])) # N nests with temperature probe within a fake egg

    
    nrow(y[fate == 1]) # number of hatched nests
    # ADD # N nests with chick found on or around the nest
    # ADD # N nests with hatching based on small eggshell pieces in the nest around estimated hatching

    xtabs(~y$fate+y$end_type) 
    summary(factor(y$fate))
    summary(factor(y$end_type)) 

    nrow(y[end_type%in%c('logger_min1day', 'visit_min1day')]) # N hatched a day ago
    nrow(y[end_type%in%c('logger_minhalfday', 'visit_minhalfday')]) # N hatched 0.5day ago
    nrow(y[end_type%in%c('hde')]) # hatched on estimated hatch date
    nrow(y[fate ==1 & end_type%in%c('last_ok+1')]) # hatched on estimated hatch date + 1 day (for cases where last ok visit was after the estimated hatch date)
    nrow(y[end_type%in%c('found_at_hatching')]) # hatched on estimated hatch date

    nrow(y[fate == 0]) # number of predated nests
    nrow(y[fate == 0 & end_type%in%c('logger', 'logger_min1day', 'logger_minhalfday')]) # predated nests with precise logger based end
    # ADD nests found empty
    # ADD predate eggs
    # ADD some eggs missing and some abandoned

    nrow(y[end_type%in%c('half_rule')]) # predated at midpoint between last_ok and last_visit
    nrow(y[fate == 0 & end_type%in%c('last_ok+1')]) # predated last_ok +1 day

    nrow(y[fate %in% c(2,5) & end_type%in%c('visit', 'logger')]) # exposure finished with last_ok visit
    
    # number of  followed nests in each day of the season 
        #max(y$end_j)-min(y$first_j, na.rm=T)
        max(y$end_j)-min(y$start_j, na.rm=T) # N days in breeding season
        l = list()
        for(i in min(y$start_j):max(y$end_j)){
            l[[i]] = data.table(day = i, recorded =  length(y[start_j<=i & end_j>=i, unique(nest)]))
        }
        yo = do.call(rbind, l)
        summary(yo)
        ggplot(yo, aes(x = recorded)) + geom_histogram()
        ggplot(yo, aes(x = day, y = recorded)) + geom_point()

    # correlation of season and mid-day temperature
        cor(x$mid_j, x$midday_T, method = 'pearson')
        ggplot(xx, aes(y = date_num, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
        ggplot(xx, aes(x = date_num, y = midday_T)) + stat_smooth(method = 'lm') + geom_point()
        ggplot(xx, aes(x = date_num, y = midday_T, col = as.factor(year))) + stat_smooth(method = 'lm') + geom_point()

# Outputs - Results  
  # all
    nrow(y) # N nests
    sum(round(y$exposure))  # number of days followed
    round(median(y$exposure)); round(mean(y$exposure));  round(range(y$exposure)) 
 
    nrow(y[fate == 0]) # number of predated nests
    nrow(y[fate == 1]) # number of hatched nests   
    nrow(y[fate == 2]) # number of failed for other reason 
    nrow(y[fate == 5]) # number with unknown fate

  # continuously monitored
    length(unique(o$nest)) # N nests

    ww = w[  , .(days_followed = sum(days)), by = nest]
    summary(ww$days) # N days each nest was continuously monitored
    round(sum(ww$days)) # N days continuously monitored

    nrow(y[fate == 0 & end_type == 'logger']) # number of predations while continuous monitoring running on the nest
    
    # daily predation rate
      ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
      bsim = sim(ma, n.sim=nsim)  
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100*30 # total predation rate

    # night predation
      summary(factor(x$night))   
# Explore how T at predation relates to season and mid-day T 
    ggplot(x, aes(y = temperature, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
    ggplot(x, aes(y = temperature, x = date_num)) + stat_smooth(method = 'lm') + geom_point()

    m = lm(temperature ~ midday_T,data=x)
    m = lm(temperature ~ date_num,data=x)
    summary(m)
# Data checking   
    nrow(y[start_expo==end_expo])
    xx = y[start_expo==end_expo]
    xtabs(~xx$fate+xx$end_type)

    nrow(y[start_expo==last_ok])
    nrow(y[start_expo==last_ok & fate%in%c(0,1)])

    nrow(y[start_expo!=last_ok & fate%in%c(0) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 10])
    nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 10])
    nrow(y[start_expo!=last_ok & fate%in%c(1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 5])
    nrow(y[start_expo!=last_ok & fate%in%c(0,1) & as.numeric(difftime(last_visit,last_ok, units = 'days')) > 5])

# sessionInfo()