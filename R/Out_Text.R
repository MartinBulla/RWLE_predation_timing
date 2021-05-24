# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla, Martin Sladecek
# 📍 This script runs relative to the project's root directory, generates
# information presented in the main text, i.e. info about sample sizes, 
# distributions, effects in Abstract, Methods and Results
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

    y_ = y
    y = y[end_type!='found_at_hatching']

    yy = y[exposure>0]
    yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
    yy[is.na(fate_), fate_:= 0]    # 0 all other
    yy[ , success := round(exposure - fate_)]
    yy[fate_==1, failure := 1]
    yy[is.na(failure), failure := 0]

    yyy = copy(yy)
    yyy[exposure>30, exposure:=30]
    yyy[ , success := round(exposure - fate_)]
    yyy[fate_==1, failure := 1]
    yyy[is.na(failure), failure := 0]

    o =fread("Data/logger_data.txt")
    o[, start_j := as.numeric(strftime(datetime_start,format="%j"))]
    o[, end_j := as.numeric(strftime(datetime_end,format="%j"))]
    
    source(here::here('R/prepare_logger_data_intersections.R')) # runs for some time
    w = data.table(logger_bouts) # shows for how long each nest was continuously monitored 
      w[, exposure := as.numeric(difftime(stop, start, units = 'days'))]
    u = data.table(logger_lengths) # shows for how long various combinations of loggers were on each nest

# Outputs - Abstract
    nrow(yy) # number of nests with exposure >0
    sum(yy$exposure)
    length(unique(o$nest)) # number of nests with some logger that continuously recorded
    round(sum(w$exposure)) # N days nests were continuously monitored

    nrow(yy[fate == 1]); round(100*nrow(yy[fate == 1])/nrow(yy)) # number & % of hatched nests
    nrow(yy[fate == 0]); round(100*nrow(yy[fate == 0])/nrow(yy))  # number & %  of predated nests      
    nrow(yy[fate == 2]); round(100*nrow(yy[fate == 2])/nrow(yy))  # number & %  of failed for other reason 
    nrow(yy[fate == 5]); round(100*nrow(yy[fate == 5])/nrow(yy))  # number & %  with unknown fate

    # daily and total predation rate
        ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
        bsim = sim(ma, n.sim=nsim)  
        #save(file = 'Data/DPR_sim.Rdata', bsim)
        load('Data/DPR_sim.Rdata')
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI

        (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate
        (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975))))^30)*100  # total predation rate

    #    
# Outputs - Introduction
    nrow(yy)  
    length(unique(o$nest)) # number of nests with some logger that continuously recorded
    round(sum(w$exposure))  
# Outputs - Methods
    length(unique(o$nest)) # N nests with some logger that continuously recorded
    length(unique(o[logger =='dvr', nest])) # N nests with video recording system
    length(unique(o[logger %in% c('tnt','dht', 'zajda_thm'), nest])) # N nests with temperature and temperature/humidity recording system
    length(unique(o[logger %in% c('rfid'), nest])) # N nests with RFID system from Max Planck
    length(unique(o[logger %in% c('zajda_rfid'), nest])) # N nests with system containing RFID, temperature and humidity logging from CZU
    length(unique(o[logger %in% c('lup_egg'), nest])) # N nests with temperature probe within a fake egg

    
    nrow(yy[start_type=='laying']) # nests found during egg laying

    nrow(yy[fate == 1]) # number of hatched nests
    nrow(yy[chicks_found=='final_nest_check']) # chicks found on the nest
    nrow(yy[chicks_found=='later']) # chicks found later
    nrow(yy[fate == 1 & chicks_found == 'no']) # nest empty (chicks out of the nest), but eggshell pieces that indicate hatching found in the nest
    641/233 # average number of chicks per found on/around a nest
    233/nrow(yy[fate == 1 & chicks_found == 'no']) # average number of chicks per nest

    nrow(yy[end_type=='logger_min1day']) # known time when chicks left based on visualized continuous data -  N hatched a day ago from the time the chicks left the nest - based on logger data
    nrow(yy[end_type=='visit_min1day']) # known time when chicks left based on visualized continuous data - N hatched a day ago from the time the chicks left the nest - based on age of chicks found away from the nest
    nrow(yy[end_type%in%c('logger_minhalfday', 'visit_minhalfday')]) # N hatched 0.5day ago
    nrow(yy[end_type%in%c('hde') & chicks_found == 'no', ]) # N  without chicks, but with signs of hatching
    nrow(yy[end_type%in%c('hde') & chicks_found == 'final_nest_check', ]) # N  older chicks were found around the nest during the final nest check 
    nrow(yy[end_type%in%c('hde') & chicks_found == 'later', ]) # N  parents found later with chicks
    nrow(yy[end_type%in%c('hde')]) # hatched on estimated hatch date
    nrow(y[fate ==1 & end_type%in%c('last_ok+1')]) # hatched on estimated hatch date + 1 day (for cases where last ok visit was after the estimated hatch date)
    nrow(y_[end_type%in%c('found_at_hatching')]) # found at or after hatching - zero exposure
    
    xtabs(~yy$fate+yy$end_type) 
    summary(factor(yy$fate))
    summary(factor(yy$end_type)) 

    nrow(y[fate == 0]) # number of predated nests
    nrow(y[fate == 0 & remarks==""]) # number of predated nests based on egg-shell pieces or adults without chicks
    nrow(y[fate == 0 & end_type%in%c('logger', 'logger_min1day', 'logger_minhalfday')]) # predated nests with precise logger based end
    y[fate == 0 & remarks!="", remarks] # number of partially predated and number of nests with egg remains

    nrow(y[end_type%in%c('half_rule')]) # predated at midpoint between last_ok and last_visit
    nrow(y[fate == 0 & end_type%in%c('last_ok+1')]) # predated last_ok +1 day
    
    nrow(y[fate == 0 & predation_logger == 'RFID_1']) # nests with RFID based timing of predation and only one parent tagged

    nrow(y[fate %in% c(5) & end_type%in%c('visit', 'logger') & exposure > 0])# unknown fate
    nrow(y[fate %in% c(2) & end_type%in%c('visit', 'logger') & exposure > 0])# abandoned or failed for other reasons

    nrow(y[fate == 2 & exposure == 0]) # failed for other reason, but missing visits and hence exposure
    nrow(y[fate == 5 & exposure == 0]) # unknown fate, but missing visits and hence exposure

    #nrow(yy[fate %in% c(2,5) & end_type%in%c('visit', 'logger')]) # exposure finished with last_ok visit

    #nrow(y[fate %in% c(2) & end_type%in%c('visit', 'logger')]) # exposure finished with last_ok - nests were abandoned or failed for other reason
    #nrow(y[fate %in% c(5) & end_type%in%c('visit', 'logger')]) # exposure finished with last_ok - nests with unknown fate
    
    # not in MS - number of  followed nests in each day of the season 
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
        cor(x_$mid_j, x_$midday_T, method = 'pearson')
        ggplot(x_, aes(y = date_num, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
        ggplot(x_, aes(x = date_num, y = midday_T, col = as.factor(year))) + stat_smooth(method = 'lm') + geom_point()

    # Explore multiple sampling per pair/bird
      x[M_ID =="", M_ID := paste0('Mn', nest)]
      x[F_ID =="", F_ID := paste0('Fn', nest)]
      x[, pair_ID:=paste0(M_ID, F_ID)]

      table(x$F_ID)
      length(unique(x$F_ID))
      length(unique(x$M_ID))
      length(unique(x$pair_ID))
      summary(factor(x$pair_ID))

      table(data.frame(table(x$pair_ID))$Freq)

      table(data.frame(table(x$F_ID))$Freq)

      table(data.frame(table(x$M_ID))$Freq)     

# Outputs - Results  
 
  # all
    nrow(yy) # N nests
    nrow(yy[fate == 0]); round(100*nrow(yy[fate == 0])/nrow(yy))  # number & %  of predated nests      
    nrow(yy[fate == 1]); round(100*nrow(yy[fate == 1])/nrow(yy)) # number & % of hatched nests
    nrow(yy[fate == 2]); round(100*nrow(yy[fate == 2])/nrow(yy))  # number & %  of failed for other reason 
    nrow(yy[fate == 5]); round(100*nrow(yy[fate == 5])/nrow(yy))  # number & %  with unknown fate
    
    sum(yy$exposure) # number of days followed
    round(median(yy$exposure),2); round(mean(yy$exposure),2);  round(range(yy$exposure),2) 
    y$exposure[y$exposure>35]
     
  # daily and total predation rate 
      ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
      bsim = sim(ma, n.sim=nsim)  
      load('Data/DPR_sim.Rdata') # loads one specific simulations that gave the results shown in the MS
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100 # total predation rate

  # not in the MS (used as a response to reviewers) daily and total with hatched based on eggshells as unknown
      yyx = copy(yy)
      yyx[fate == 1 & chicks_found == 'no', exposure := as.numeric(difftime(last_ok, start_expo, units = 'days'))]

      yyx = yyx[exposure>0]
      yyx[fate == 0, fate_:= 1]   # 1 predated works if swapped
      yyx[is.na(fate_), fate_:= 0]    # 0 all other
      yyx[ , success := round(exposure - fate_)]
      yyx[fate_==1, failure := 1]
      yyx[is.na(failure), failure := 0]

      ma=glm(cbind(failure,success)~1,family="binomial",data=yyx)
      bsim = sim(ma, n.sim=nsim)  
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100 # total predation rate

  # daily predation rate capping the extreme observation periods at 30 days
      nrow(yy[exposure>30]) # N nests with long observation periods  
      ma=glm(cbind(failure,success)~1,family="binomial",data=yyy)
      bsim = sim(ma, n.sim=nsim)  
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate
  # controlling for multiple clutching
      yy[M_ID =="", M_ID := paste0('Mn', nest)]
      yy[F_ID =="", F_ID := paste0('Fn', nest)]
      yy[, pair_ID:=paste0(M_ID, F_ID)]
      length(unique(yy$F_ID))
      length(unique(yy$M_ID))
      length(unique(yy$pair_ID))
      summary(factor(yy$pair_ID))

      table(data.frame(table(yy$pair_ID))$Freq)

      table(data.frame(table(yy$F_ID))$Freq)

      table(data.frame(table(yy$M_ID))$Freq)

      # multiple bird ids - response to reviewers
        bird = data.table(bird = c(yy$F_ID[yy$F_ID!=""],yy$M_ID[yy$M_ID!=""]))
        bird_N = bird[, .N, by = bird] 
        summary(bird_N)
        ggplot(bird_N, aes(x = N)) + geom_histogram()
        nrow(bird_N[N<2])/nrow(bird_N)
        nrow(bird_N[N==2])/nrow(bird_N)
        nrow(bird_N[N<3])/nrow(bird_N)
        nrow(bird_N[N==3])/nrow(bird_N)
        nrow(bird_N[N==4])/nrow(bird_N)
        nrow(bird_N[N>4])/nrow(bird_N)
  
      ma=glmer(cbind(failure,success)~1 + (1|M_ID) + (1|F_ID) + (1|pair_ID),family="binomial",data=yy)
      ma=glmer(cbind(failure,success)~1 + mid_j + (1|M_ID) + (1|F_ID) + (1|pair_ID),family="binomial",data=yy)
      ma=glmer(cbind(failure,success)~1 +  (1|pair_ID),family="binomial",data=yy)
      ma=glmer(cbind(failure,success)~1 +  mid_j + (1|pair_ID),family="binomial",data=yy)
      bsim = sim(ma, n.sim=nsim)  
      plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5)))*100 # estimate
      plogis(apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      (1-(1-plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100 # total predation rate

  # not in the MS, but as a response to the reviewer - >5 days of exposure
      yyyy = yy[exposure>5]
      yyyy[ , success := round(exposure - fate_)]
      yyyy[fate_==1, failure := 1]
      yyyy[is.na(failure), failure := 0] 
      ma=glm(cbind(failure,success)~1,family="binomial",data=yyyy)
      bsim = sim(ma, n.sim=nsim)  
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
      plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
      (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate 
  # not in the MS, daily predation rate without nests predated after estimated hatch date
      yyyy = yy[-which(fate == 0 & end_type%in%c('last_ok+1'))]
      yyyy[ , success := round(exposure - fate_)]
      yyyy[fate_==1, failure := 1]
      yyyy[is.na(failure), failure := 0] 

      ma=glm(cbind(failure,success)~1,family="binomial",data=yyyy)
      bsim = sim(ma, n.sim=nsim)  
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100,2) # estimate
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100,2) #95%CI
      round((1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100) # total predation rate
  # daily and total predation rate without unknown
      yyyy = yy[fate!=5]
      yyyy[ , success := round(exposure - fate_)]
      yyyy[fate_==1, failure := 1]
      yyyy[is.na(failure), failure := 0] 

      ma=glm(cbind(failure,success)~1,family="binomial",data=yyyy)
      bsim = sim(ma, n.sim=nsim)  
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100,2) # estimate
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100,2) #95%CI
      round((1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100) # total predation rate
  # not in the MS, predation rate for continuously monitored period only
        w[ ,fate := 0]
        w[ , max_stop :=max(stop), by = nest]
        w[ stop == max_stop & nest %in% y[fate == 0 & end_type == 'logger', nest], fate :=1]
        w[ , success := round(exposure - fate)]
        w[fate==1, failure := 1]
        w[is.na(failure), failure := 0]

        
        ma=glm(cbind(failure,success)~1,family="binomial",data=w)
        bsim = sim(ma, n.sim=nsim)  
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
        plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
        (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate
        (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.025, 0.975))))^30)*100  # total predation rate
        # predation rate for continuously monitored nests
        
        ma=glmer(cbind(failure,success)~1 + (1|nest),family="binomial",data=w) # gives nonsensical output
        bsim = sim(ma, n.sim=nsim)  
        plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
        plogis(apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
        (1-(1-plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate
        (1-(1-plogis(apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))))^30)*100  # total predation rate
  # not in the MS, predation rate for continuously monitored nests only
      yyyy = yy[nest %in%o$nest]
      yyyy[ , success := round(exposure - fate_)]
      yyyy[fate_==1, failure := 1]
      yyyy[is.na(failure), failure := 0] 

      ma=glm(cbind(failure,success)~1,family="binomial",data=yyyy)
      bsim = sim(ma, n.sim=nsim)  
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100,2) # estimate
      round(plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100,2) #95%CI
      round((1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5,0.025,0.975 ))))^30)*100) # total predation rate
         
  # continuously monitored
    length(unique(o$nest)) # N nests
    length(unique(o$nest))/nrow(yy) # % of nests
    summary(w$exposure) # N days each nest was continuously monitored
    min(w$exposure)*24
    round(sum(w$exposure)) # N days continuously monitored

    nrow(y[fate == 0 & end_type == 'logger']) # number of predations while continuous monitoring running on the nest

  # night predation
    summary(factor(x_$night))   
    sum(x_$night_num)/nrow(x_)

  # first part of the day predation
     nrow(x_[time<12])   
     nrow(x_[time<12])/nrow(x_)   

     x_[, exp_hatch := first_egg + 30*24*60*60]
     x_[, diff := as.numeric(difftime(exp_hatch, end_expo, units = 'days'))]
     x_[,.(nest,exposure, first_egg,exp_hatch,end_expo, diff)]

  # Explore how T at predation relates to season and mid-day T 
      ggplot(x_, aes(y = temperature, x = midday_T)) + stat_smooth(method = 'lm') + geom_point()
      ggplot(x_, aes(y = temperature, x = date_num)) + stat_smooth(method = 'lm') + geom_point()

      m = lm(temperature ~ midday_T,data=x_)
      m = lm(temperature ~ date_num,data=x_)
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
 #R version 4.0.2 (2020-06-22)
 #Platform: x86_64-apple-darwin17.0 (64-bit)
 #Running under: macOS Mojave 10.14.6
 #
 #Matrix products: default
 #BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
 #LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
 #
 #locale:
 #[1] C/UTF-8/C/C/C/C
 #
 #attached base packages:
 #[1] grid      stats     graphics  grDevices utils     datasets  methods  
 #[8] base     
 #
 #other attached packages:
 # [1] ggpubr_0.4.0      forcats_0.5.0     dplyr_1.0.1       purrr_0.3.4      
 # [5] readr_1.3.1       tidyr_1.1.1       tibble_3.0.3      tidyverse_1.3.0  
 # [9] gt_0.2.2          zoo_1.8-8         xlsx_0.6.3        stringr_1.4.0    
 #[13] reshape2_1.4.4    raster_3.3-13     plyr_1.8.6        performance_0.4.8
 #[17] multcomp_1.4-13   TH.data_1.0-10    survival_3.1-12   mvtnorm_1.1-1    
 #[21] maptools_1.0-1    sp_1.4-2          magrittr_1.5      lubridate_1.7.9  
 #[25] lattice_0.20-41   htmlTable_2.0.1   gridExtra_2.3     glue_1.4.2       
 #[29] ggthemes_4.2.0    ggplot2_3.3.2     ggExtra_0.9       effects_4.1-4    
 #[33] carData_3.0-4     data.table_1.13.0 arm_1.11-2        lme4_1.1-23      
 #[37] Matrix_1.2-18     MASS_7.3-51.6     here_0.1         
 #
 #loaded via a namespace (and not attached):
 # [1] minqa_1.2.4         colorspace_1.4-1    ggsignif_0.6.0     
 # [4] rio_0.5.16          ellipsis_0.3.1      rgdal_1.5-16       
 # [7] rprojroot_1.3-2     fs_1.5.0            base64enc_0.1-3    
 #[10] rstudioapi_0.11     fansi_0.4.1         xml2_1.3.2         
 #[13] codetools_0.2-16    splines_4.0.2       knitr_1.29         
 #[16] Formula_1.2-3       jsonlite_1.7.0      nloptr_1.2.2.2     
 #[19] rJava_0.9-13        broom_0.7.0         cluster_2.1.0      
 #[22] dbplyr_1.4.4        png_0.1-7           shiny_1.5.0        
 #[25] compiler_4.0.2      httr_1.4.2          backports_1.1.8    
 #[28] assertthat_0.2.1    fastmap_1.0.1       cli_2.0.2          
 #[31] survey_4.0          later_1.1.0.1       acepack_1.4.1      
 #[34] htmltools_0.5.0     tools_4.0.2         coda_0.19-3        
 #[37] gtable_0.3.0        Rcpp_1.0.5          cellranger_1.1.0   
 #[40] vctrs_0.3.2         nlme_3.1-148        insight_0.9.0      
 #[43] xfun_0.16           xlsxjars_0.6.1      openxlsx_4.1.5     
 #[46] rvest_0.3.6         mime_0.9            miniUI_0.1.1.1     
 #[49] lifecycle_0.2.0     rstatix_0.6.0       statmod_1.4.34     
 #[52] scales_1.1.1        hms_0.5.3           promises_1.1.1     
 #[55] sandwich_2.5-1      RColorBrewer_1.1-2  curl_4.3           
 #[58] rpart_4.1-15        latticeExtra_0.6-29 stringi_1.5.3      
 #[61] bayestestR_0.7.2    checkmate_2.0.0     zip_2.0.4          
 #[64] boot_1.3-25         rlang_0.4.7         pkgconfig_2.0.3    
 #[67] htmlwidgets_1.5.1   tidyselect_1.1.0    R6_2.4.1           
 #[70] generics_0.0.2      Hmisc_4.4-0         DBI_1.1.0          
 #[73] pillar_1.4.6        haven_2.3.1         foreign_0.8-80     
 #[76] withr_2.2.0         abind_1.4-5         nnet_7.3-14        
 #[79] car_3.0-8           modelr_0.1.8        crayon_1.3.4       
 #[82] jpeg_0.1-8.1        readxl_1.3.1        blob_1.2.1         
 #[85] reprex_0.3.0        digest_0.6.25       xtable_1.8-4       
 #[88] httpuv_1.5.4        munsell_0.5.0       mitools_2.4 