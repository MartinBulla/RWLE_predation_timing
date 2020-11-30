# LOAD TOOLS and DATA
  require(here)
  source(here::here('R/tools.R'))
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))

  require(directlabels)

# check
  ggplot(xx, aes(x = date_num, y = midday_T)) + geom_smooth() + geom_point()
  ggplot(xx, aes(x = date_num, y = midday_T)) + geom_smooth(method = 'lm') + geom_point()
  ggplot(xx, aes(x = date_num, y = midday_T)) + geom_smooth(method = 'lm', formula = y ~ poly(x,2), size = 1) + geom_point()
  cor(xx$date_num, xx$midday_T)
  

  ggplot(xx, aes(x = date_num, y = temperature)) + geom_smooth() + geom_point()
    ggplot(xx, aes(x = date_num, y = temperature)) + geom_smooth(method = 'lm', formula = y ~ poly(x,2), size = 1) + geom_point()

  cor(xx$date_num, xx$temperature)

  ggplot(y, aes(x = mid_age, y = mid_j)) + geom_smooth(method = 'lm') + geom_point()
  ym = y[!is.na(mid_age)]
  cor(ym$mid_age, ym$mid_j)

# daily predation rate according to Mayfield
  nrow(y[fate==1])/sum(y$exposure)

# 24h pattern
  # histogram - ggplot 
    gg = ggplot(xx, aes(x = time_corr, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0), lim = c(0,24), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + scale_y_continuous(expand = c(0, 0), name ="Predation events [count]")+
      scale_fill_manual(values=c(day = day_, night = night_)) + labs(tag = "(a)") +
      theme_MB +
      theme(legend.title = element_blank(),
            plot.tag.position = c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
    
    ggsave(file = 'Output/histogram_24h.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

    #hist(xx$time_corr,breaks=12)
    #hist(xx$time_corr,breaks=24,xlim=c(0,24))
    #summary(as.factor(xx$night))# check how many depredations in day/night
    #summary(xx$sunset_num-xx$sunrise_num)# check the distribution of night lengths
  
  # histogram - R-base plot
    x1=ddply(xx,"time_corr_round",summarize,n=length(time_corr))
    x1n=ddply(xx[xx$night=="night",],"time_corr_round",summarize,n=length(time_corr))
    x2=data.frame(hr=c(0:23),n=0)
    x2$n=x1$n[match(x2$hr,x1$time_corr_round)]
    x2$n_night=x1n$n[match(x2$hr,x1n$time_corr_round)]
    x2$n[is.na(x2$n)]=0
    x2$n_night[is.na(x2$n_night)]=0

    barplot(x2$n)# overall
    barplot(t(cbind(x2$n_night,x2$n-x2$n_night)),beside = F,
               col = c("grey30","grey70"))# with day/night
  
  # cor
    ggplot(td, aes(x = time_corr_round, y = cases)) + geom_point() + stat_smooth()    
  
  # model
    tx = data.table(time_corr_round = seq(0,23,by =1))
    tdx = merge(td,tx, all = TRUE)
    tdx[is.na(cases), cases := 0]
    tdx[,rad :=(2*pi*time_corr_round)/24]
    tdx[ time_corr_round<12, day_part := 1]
    tdx[ is.na(day_part), day_part := 2]

    m = lm(cases~sin(rad)+cos(rad), tdx)
    m = glm(cases~sin(rad)+cos(rad), family = 'poisson', tdx)
    m = glm(cases~day_part, family = 'poisson', tdx)
    summary(m)
    plot(allEffects(m))    

# sunrise pattern  
  
  # ggplot
    summary(xx$time_from_sunrise)
    gg = ggplot(xx, aes(x = time_from_sunrise, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
          scale_x_continuous(expand = c(0, 0), lim = c(-12,12), breaks = seq(-12,12, by = 1), labels = c("-12", "", "-10", "", "-8", "", "-6", "", "-4", "", "-2", "", 0,"","2","","4","","6","","8","","10","","12"), name = "Time after sunrise [hour]") + scale_y_continuous(expand = c(0, 0), name ="Predation events [count]")+
          scale_fill_manual(values=c(day = day_, night = night_)) + labs(tag = "(b)") +
      theme_MB +
      theme(legend.title = element_blank(),
            plot.tag.position = c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
  
        
    ggsave(file = 'Output/histogram_after-sunrise.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
  # cor
    ggplot(ts, aes(x = time_from_sunrise_r, y = cases)) + geom_point() + stat_smooth()
 
  # model
    tx = data.table(time_from_sunrise_r = seq(-11,11,by =1))
    tsx = merge(ts,tx, all = TRUE)
    tsx[is.na(cases), cases := 0]
    tsx[,rad :=(2*pi*time_from_sunrise_r)/24]

    m = lm(cases~abs(time_from_sunrise_r), tsx)
    m = glm(cases~abs(time_from_sunrise_r), family = 'poisson', tsx)
    summary(m)
    plot(allEffects(m))
    
    m = lm(cases~sin(rad)+cos(rad), tsx)
    m = glm(cases~sin(rad)+cos(rad), family = 'poisson', tsx)
    summary(m)
    plot(allEffects(m))    

    res_sunrise = m_out(name = "sunris_poisson", model = m, type = "glm", round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE, N = 23)

# temperature pattern
  # histogram - ggplot 
    gg = ggplot(xx, aes(x = temperature)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name ="Predation events [count]")+
      #scale_fill_manual(values=c(day = day_, night = night_)) + labs(tag = "(a)") +
      theme_MB +
      theme(legend.title = element_blank(),
            plot.tag.position = c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
    
    ggsave(file = 'Output/histogram_T.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

  # histogram day/night- ggplot 
      gg = ggplot(xx, aes(x = temperature, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), name ="Predation events [count]")+
        scale_fill_manual(values=c(day = day_, night = night_)) + #labs(tag = "(a)") +
        theme_MB +
        theme(legend.title = element_blank(),
              plot.tag.position = c(0.81, 0.98),
              plot.tag = element_text(size = 6, face = "bold") 
              )
      
      ggsave(file = 'Output/histogram_T_day-night.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

# seasonal pattern day/night
  # temperature general
    ggplot() + 
      geom_tile(aes(x = date, y = hr, fill = mean),a[year == 2019]) + 
      scale_fill_viridis_c(option = "plasma", name = "C°") +
      theme_MB 

  # temperature with predation data
    gg=  ggplot() + 
      geom_tile(aes(x = date_num, y = hr, fill = mean),a[year == 2019]) + 
      scale_fill_viridis_c(option = "plasma", name = "C°") +
      geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_point(aes(x = date_num, y = time_corr, col = night), xx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + 
      scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_color_manual(values=c(day = day_, night = night_)) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      theme_MB 

    ggsave(file = 'Output/season_time-vertical_night-temperature.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

  # temperature at predation simple 
    xx1 = xx[,.(date_num, time_corr, temperature)]
    xx2 = data.frame(date_num = c(1,1), time_corr = c(12,12), temperature = c(min(a[, mean]),max(a[, mean]))   ) # dummy variable, to get the C-scale we need
    xxx = rbind(xx1,xx2)

    isotherm50 = "#FA9E3BFF"
    isotherm60 = "#FDC926FF"
    isotherm70 = "#F0F921FF"

    gg = ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_smooth(aes(x = date_num, y = hr), data = a[mean>49 & mean <51 & hr>12 & date_num < 196], se = FALSE, col = isotherm50, lwd = 0.25) +
      geom_smooth(aes(x = date_num, y = hr), data = a[mean>49 & mean <51 & hr<12 & date_num < 196], se = FALSE, col = isotherm50, lwd = 0.25) +
      #geom_smooth(aes(x = date_num, y = hr), data = a[mean>59 & mean <61 & hr>12 & date_num < 196], se = FALSE, col = isotherm60, lwd = 0.25) +
      #geom_smooth(aes(x = date_num, y = hr), data = a[mean>59 & mean <61 & hr<12 & date_num < 196], se = FALSE, col = isotherm60, lwd = 0.25) +
      #geom_smooth(aes(x = date_num, y = hr), data = a[mean>69 & mean <71 & hr>12 & date_num < 196], se = FALSE, col = isotherm70, lwd = 0.25) +
      #geom_smooth(aes(x = date_num, y = hr), data = a[mean>69 & mean <71 & hr<12 & date_num < 196], se = FALSE, col = isotherm70, lwd = 0.25) +
      geom_point(aes(x = date_num, y = time_corr, col = temperature), xxx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + 
      scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_colour_viridis_c(option = "plasma", name = "C°")+ 
        #,begin = (min(xx$temperature)- min(a[, mean]))/(max(a[, mean])-min(a[, mean])),
        #end = (max(xx$temperature)- min(a[year == 2019, mean]))/(max(a[year == 2019, mean])-min(a[year == 2019, mean]))) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      annotate("text", x =125, y=14.5, label = "50C° isotherm", size =1.5, col = isotherm50)+
      theme_MB 

    ggsave(file = 'Output/season_time-vertical_night-area_TatPred_limitMinMAxAll.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
  # temperature at predation isotherms
    xx1 = xx[,.(date_num, time_corr, temperature)]
    xx2 = data.frame(date_num = c(1,1), time_corr = c(12,12), temperature = c(min(a[, mean]),max(a[, mean]))   ) # dummy variable, to get the C-scale we need
    xxx = rbind(xx1,xx2)

    vir_ = scales::viridis_pal(option = "plasma")(13)
    isotherm35 = scales::viridis_pal(option = "plasma")(13)[6]
    isotherm40 = scales::viridis_pal(option = "plasma")(13)[7]
    isotherm45 = scales::viridis_pal(option = "plasma")(13)[8]
    isotherm50 = "#FA9E3BFF"
    isotherm60 = "#FDC926FF"
    isotherm70 = "#F0F921FF"
    
    # dataset for isotherm
      iso_loess <- loess(mean ~ date_num * hr, data = a)
      
      # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
        xgrid <-  seq(min(a$date_num), 200, 1)
        ygrid <-  seq(min(a$hr), max(a$hr), 0.2)
      
      # Generate a dataframe with every possible combination of wt and hp
        newD <-  expand.grid(date_num = xgrid, hr = ygrid)
      # Feed the dataframe into the loess model and receive a matrix output with estimates of 
      # hour for each combination of date and mean temperature
        meanT <-  predict(iso_loess, newdata = newD)
        meanT[1:4, 1:4] #  # Abbreviated display of final matrix
        contour(x = xgrid, y = ygrid, z = meanT, xlab = 'day', ylab = 'hour')

      # Transform data to long form
        meanTL <- melt(meanT, id.vars = c('date_num', 'hr'), measure.vars = 'meanT')
        names(meanTL) <- c('date_num', 'hr', 'meanT')
      
      # Return data to numeric form
        meanTL$date_num <- as.numeric(str_sub(meanTL$date_num, str_locate(meanTL$date_num, '=')[1,1] + 1))
        meanTL$hr <- as.numeric(str_sub(meanTL$hr, str_locate(meanTL$hr, '=')[1,1] + 1))

        head(meanTL)  

        p = 
          ggplot()+
          stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = ..level..), 
                     breaks = 45, size = 1)
          direct.label(p, 'bottom.pieces')
        p = 
          ggplot()+
          stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = ..level..), 
                     breaks = c(15,20,25,30,35,40,45), size = 0.5)
          direct.label(p, 'bottom.pieces')

          p = 
          ggplot()+
          stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = as.factor(..level..)), 
                     breaks = c(35,40,45), size = 0.5) +
          scale_colour_manual(values = vir_[6:8])
          direct.label(p, 'bottom.pieces')
    # dataset for sunrise and sunset
        dats=seq.POSIXt(from=as.POSIXct("2019-02-19 UTC"), # day 50
                        to=as.POSIXct("2019-07-19 UTC"),by=24*3600)
        sunrs=crepuscule(koord,dats,direction="dawn",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
        sunss=crepuscule(koord,dats,direction="dusk",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
        sunrs=(as.numeric(sunrs) %% (24*3600))/3600 
        sunss=(as.numeric(sunss) %% (24*3600))/3600 
        ss2 = data.table(date = dats, sunrs = sunrs, sunss = sunss)
        ss2[,date_num := as.numeric(strftime(date,format="%j"))]
    
    # center use
      gg = 
      ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss2, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss2,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 45, size = 0.25, colour = isotherm45) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 40, size = 0.25, colour = isotherm40) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 35, size = 0.25, colour = isotherm35) +
      geom_point(aes(x = date_num, y = time_corr, col = temperature), xxx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + 
      scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_colour_viridis_c(option = "plasma", name = "C°")+ 
        #,begin = (min(xx$temperature)- min(a[, mean]))/(max(a[, mean])-min(a[, mean])),
        #end = (max(xx$temperature)- min(a[year == 2019, mean]))/(max(a[year == 2019, mean])-min(a[year == 2019, mean]))) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      annotate("text", x =104, y=10, label = "35C°", size =1.5, col = isotherm35)+
      annotate("text", x =128, y=10, label = "40C°", size =1.5, col = isotherm40)+
      annotate("text", x =165, y=10, label = "45C°", size =1.5, col = isotherm45)+
      theme_MB 

      ggsave(file = 'Output/season_time-vertical_night-area_TatPred_limitMinMAxAll_ISO_middle.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
    
    # center day/night rights use
      gg = 
      ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss2, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss2,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 45, size = 0.25, colour = isotherm45) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 40, size = 0.25, colour = isotherm40) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 35, size = 0.25, colour = isotherm35) +
      geom_point(aes(x = date_num, y = time_corr, col = temperature), xxx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + 
      scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_colour_viridis_c(option = "plasma", name = "C°")+ 
      annotate("text", x=55, y=3, label= "night", size =1.5, hjust = 0) + 
      annotate("text", x =55, y=12, label = "day", size =1.5, hjust = 0)+
      annotate("text", x =104, y=10, label = "35C°", size =1.5, col = isotherm35)+
      annotate("text", x =128, y=10, label = "40C°", size =1.5, col = isotherm40)+
      annotate("text", x =165, y=10, label = "45C°", size =1.5, col = isotherm45)+
      labs(tag = "(a)") +
      theme_MB +
      theme(  
              plot.tag.position = 'topleft', #c(0.025, 0.98), #plot.tag.position = c(0.42, 0.96),
              plot.tag = element_text(size = 6, face = "bold"), # size = 10)
              legend.text=element_text(size=5),
              legend.title=element_text(size=6, hjust = 0.5)
              
              )

      ggsave(file = 'Output/season_time-vertical_night-area_TatPred_limitMinMAxAll_ISO_middle_DMright.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

    # right
      gg = 
      ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 45, size = 0.25, colour = isotherm45) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 40, size = 0.25, colour = isotherm40) +
      stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 35, size = 0.25, colour = isotherm35) +
      geom_point(aes(x = date_num, y = time_corr, col = temperature), xxx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + 
      scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,210))+
      scale_colour_viridis_c(option = "plasma", name = "C°")+ 
        #,begin = (min(xx$temperature)- min(a[, mean]))/(max(a[, mean])-min(a[, mean])),
        #end = (max(xx$temperature)- min(a[year == 2019, mean]))/(max(a[year == 2019, mean])-min(a[year == 2019, mean]))) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      annotate("text", x =201, y=4.1, label = "35C°", size =1.5, col = isotherm35)+
      annotate("text", x =201, y=6.2, label = "40C°", size =1.5, col = isotherm40)+
      annotate("text", x =201, y=8.4, label = "45C°", size =1.5, col = isotherm45)+
      theme_MB 

      ggsave(file = 'Output/season_time-vertical_night-area_TatPred_limitMinMAxAll_ISO.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')


  # temperature at midday of predation event
    gg = ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_point(aes(x = date_num, y = time_corr, col = midday_T), xx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_colour_viridis_c(option = "plasma", name = "C°", 
        begin = (min(xx$temperature)- min(a[year == 2019, mean]))/(max(a[year == 2019, mean])-min(a[year == 2019, mean])),
        end = (max(xx$temperature)- min(a[year == 2019, mean]))/(max(a[year == 2019, mean])-min(a[year == 2019, mean]))
        ) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      theme_MB 

    ggsave(file = 'Output/season_time-vertical_night-area_Tmidday_limit2019.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')


  # histogram
    gg = ggplot(xx, aes(x = date_num, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") + scale_y_continuous(expand = c(0, 0), name ="Predation events [count]", lim = c(0,4))+
      scale_fill_manual(values=c(day = day_, night = night_)) + labs(tag = "(c)") +
      theme_MB +
      theme(legend.title = element_blank(),
            plot.tag.position = c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
    
    ggsave(file = 'Output/histogram_season.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
  # scatterplot horizontal
    gg = ggplot() + geom_point(aes(y = date_num, x = time_corr, col = night), xx)  +
      geom_path(aes(x = sunrs , y = date_num),ss,  size = 0.25, col = "grey") +
      geom_path(aes(x = sunss , y = date_num),ss,  size = 0.25, col = "grey") +
      scale_x_continuous(expand = c(0, 0), lim = c(0,24), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + scale_y_reverse(expand = c(0, 0), name ="Day in year", lim = c(200,50))+
      scale_color_manual(values=c(day = day_, night = night_)) +
      theme_MB +
      theme(legend.title = element_blank())
    
    ggsave(file = 'Output/season_time_horizontal.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
  # scatterplot - time vertical
    gg = ggplot() + 
      geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_area(aes(y =sunrs, x = date_num),ss,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_point(aes(x = date_num, y = time_corr, col = night), xx)  +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_color_manual(values=c(day = day_, night = night_)) +
      annotate("text", x=125, y=3, label= "night", size =1.5) + 
      annotate("text", x =125, y=12, label = "day", size =1.5)+
      theme_MB +
      theme(legend.title = element_blank())
    
    ggsave(file = 'Output/season_time-vertical_night-area.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
  # raw R-base plot
    plot(xx$time_corr~xx$date_num,cex=2,col=c("grey70","grey30")[xx$night],
         pch=20,xlab="day in year",ylab= "hour of the day")
    lines(x=seq(min(xx$date_num),max(xx$date_num)),y=sunss)
    lines(x=seq(min(xx$date_num),max(xx$date_num)),y=sunrs)

  # fit - glm
    g1 = ggplot(xx, aes(x = date_num, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") + scale_y_continuous(expand = c(0, 0), name ="Count", breaks = c(0,1,2), labels = c("1.00", '2.00', '3.00'))+
      scale_fill_manual(values=c(day = day_, night = night_))+
      theme_MB +
      theme(legend.position = "none",#legend.title = element_blank(),
            plot.tag.position = c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank()#, vjus
            )
    g2 = ggplot(xx, aes(y = night_num, x = date_num)) + geom_jitter(data = xx[night == 'day',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = day_) + geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = night_) + stat_smooth( method="glm", method.args=list(family="binomial"), formula = y ~ poly(x,2), size = 1), col = night_) + 
      theme_MB +
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") +scale_y_continuous(  name = "Probability of night vs day predation")

    #ggExtra::ggMarginal(gg, type = "histogram")#, groupColour = TRUE)

    grid.arrange(g1, g2, nrow=2, heights=c(1, 4))


    gg1 <- ggplotGrob(g1)
    gg2 <- ggplotGrob(g2)
    gg = arrangeGrob(
      grobs = list(gg1,gg2),
      heights = c(0.2, 0.8),
      layout_matrix = cbind(c(0.2, 0.8))
      )
    
    ggsave(file = 'Output/fit_season_withHist.png', gg, dpi = 300, width = 7, height = 7, units = 'cm')

  ggplot(xx, aes(y = night_num, x = date_num)) + 
    geom_jitter(data = xx[night == 'day',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = day_) + 
    geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = night_) + 
    stat_smooth( method="glm", method.args=list(family="binomial"), formula = y ~ poly(x,2), size = 1, col = night_) + 
      theme_MB +
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") +scale_y_continuous(  name = "Probability of night vs day predation")

  ggplot(xx[date_num>100], aes(y = night_num, x = date_num)) + 
    geom_jitter(data = xx[night == 'day',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = day_) + 
    geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = night_) + 
    stat_smooth( method="glm", method.args=list(family="gaussian"), size = 1, col = night_) + 
      theme_MB +
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") +scale_y_continuous(  name = "Probability of night vs day predation")  


  # fit - glm temperature
    ggplot(xx, aes(y = night_num, x = midday_T)) + 
      geom_jitter(data = xx[night == 'day',] , aes(y = night_num,x = midday_T), width = 0, height = 0.025, col = day_) + 
      geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = midday_T), width = 0, height = 0.025, col = night_) + 
      stat_smooth( method="glm", method.args=list(family="binomial"), col = night_) + 
      theme_MB +
      scale_x_continuous(expand = c(0, 0), lim = c(30,65),  name = "Midday temperature [C°]") +
      scale_y_continuous(  name = "Probability of night vs day predation")

  # fit - lm temperature
     ggplot(xx, aes(y = night_num, x = midday_T)) + 
      geom_jitter(data = xx[night == 'day',] , aes(y = night_num,x = midday_T), width = 0, height = 0.025, col = day_) + 
      geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = midday_T), width = 0, height = 0.025, col = night_) + 
      stat_smooth( method="lm", col = night_) + 
      theme_MB +
      scale_x_continuous(expand = c(0, 0), lim = c(30,65),  name = "Midday temperature [C°]") +
      scale_y_continuous(  name = "Probability of night vs day predation")
  
  # model glm temperature
    mbt=glm(night_num~midday_T,data=xx,family="binomial")
    summary(mbt)
    summary(glht(mbt))
    plot(allEffects(mbt))

    m_ass_s( name = 'ModelAss_night_given_temperature_binary',
               title = 'glm(night~midday_T,data=xx,family="binomial"',
               binomial = TRUE, mo = mbt, dat = xx, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")
  # model lm temperature
    mbt=lm(night_num~midday_T,data=xx)
    summary(mbt)
    summary(glht(mbt))
    plot(allEffects(mbt))

    m_ass_s( name = 'ModelAss_night_given_temperature_gauss',
               title = 'lm(night~midday_T,data=xx',
               binomial = TRUE, mo = mbt, dat = xx, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")

  # model glm
    mb=glm(night_num~date_num,data=xx,family="binomial")
    summary(mb)
    summary(glht(mb))
    plot(allEffects(mb))

    m_ass_s( name = 'ModelAss_night_given_season_binary',
               title = 'glm(night~date_num,data=xx,family="binomial"',
               binomial = TRUE, mo = mb, dat = xx, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")

    m = summary(mb)
    plot(exp(m$coefficients[1,1] + seq(min(xx$date_num),max(xx$date_num)) * m$coefficients[2,1])/
           (1+exp(m$coefficients[1,1] + seq(min(xx$date_num),max(xx$date_num)) * m$coefficients[2,1])) 
         ~ seq(min(xx$date_num),max(xx$date_num)),
         type="l",
         ylab="Probability of night predation",
         xlab="Day in year",
         ylim=c(0,1),
         bty="n")
    
    points((rnorm(nrow(xx),0,.01)+(as.numeric(xx$night)-1))~xx$date_num,
           col="grey30")
  # model lm (same results)
    mb=lm(night_num ~ date_num, data=xx)
      summary(mb)
      summary(glht(mb))
      plot(allEffects(mb))
      m_ass_s( name = 'ModelAss_night_given_season_Gaussian',
               title = 'lm(night_num ~ date_num, data=xx)',
               binomial = TRUE, mo = mb, dat = xx, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")

  # model glm temperature + date
    mbdt=glm(night_num~midday_T + date_num,data=xx,family="binomial")
    summary(mbdt)
    summary(glht(mbdt))
    plot(allEffects(mbdt))

    mx = lm(midday_T~ date_num,data=xx)
    xx$res_T = resid(mx)
    mbrd=glm(night_num~res_T + date_num,data=xx,family="binomial")
    summary(mbrd)
    summary(glht(mbrd))
    plot(allEffects(mbrd))

    m_ass_s( name = 'ModelAss_night_given_temperature_binary',
               title = 'glm(night~res_T + date_num,data=xx,family="binomial"',
               binomial = TRUE, mo = mbrd, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")

  # model lm temperature+ date
    mbt=lm(night_num~midday_T+ date_num,data=xx)
    summary(mbt)
    summary(glht(mbt))
    plot(allEffects(mbt))

    mx = lm(midday_T~ date_num,data=xx)
    xx$res_T = resid(mx)
    mbrd=lm(night_num~res_T+ date_num,data=xx)
    summary(mbrd)
    summary(glht(mbrd))
    plot(allEffects(mbrd))

    m_ass_s( name = 'ModelAss_night_given_temperature_gauss',
               title = 'lm(night~res_T + date_num,data=xx',
               binomial = TRUE, mo = mbt, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/")

# daily predation across season and nest age - all data
  
  #nrow(y[first_egg>start_expo])
  #nrow(y[start_expo>end_expo])
  #nrow(y[first_egg>mid_expo])
  #summary(y[first_egg>as.Date(start_expo), as.numeric(difftime(as.Date(first_egg), start_expo, units = 'days'))])
  
  y[fate == 1, fate_:= 0]   # works if swapped
  y[fate !=1, fate_:= 1]    # works if swapped
  y[, year_:= as.factor(year)] 
  y[ ,success := round(exposure - fate_)]
  y[fate==1, failure := 1]
  y[fate!=1, failure := 0]

  summary(as.factor(y$fate))
  summary(as.factor(y$fate_))

  #as.numeric(strftime(mean(c(end_expo, start_expo)),format="%j")), by = rowid]

  summary(y$exposure)
  

  # all data pred vs rest
    yy = y[exposure>0.5] 
    m = glm(fate_ ~ 1, family=binomial(logexp(days=round(yy$exposure))),data=yy)  
    1-(exp(m$coefficients)/(1+exp(m$coefficients))) #0.0108039
    1-plogis(m$coefficients)

    yy = y[exposure>0.5] 
    m2=glm(cbind(success,failure)~1,family="binomial",data=yy)
    1-(exp(m2$coefficients)/(1+exp(m2$coefficients))) #  0.01082444
    1-plogis(m2$coefficients)

  # predated vs hatched only
  yy = y[exposure>0.5 & fate<2] 
  
  # logistic exposure
  m = glm(fate_ ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)  
  1-(exp(m$coefficients)/(1+exp(m$coefficients))) #0.01284487
  
  ms = glm(fate ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)  
  (exp(ms$coefficients)/(1+exp(ms$coefficients))) 

  # binomial regression
  m2=glm(cbind(success,failure)~1,family="binomial",data=yy)
  1-(exp(m2$coefficients)/(1+exp(m2$coefficients))) 

  ms2=glm(cbind(failure,success)~1,family="binomial",data=yy)
  (exp(ms2$coefficients)/(1+exp(ms2$coefficients))) 

  m = glm(fate ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)  
  1-(exp(m$coefficients)/(1+exp(m$coefficients))) #0.01284487


  m = glm(fate_ ~ mid_j, family=binomial(logexp(days=yy$exposure)),data=yy)  
  summary(m) 
  plot(allEffects(m))

  # predated vs hatched only and known age
  yya = y[exposure>0.5 & fate<2 & !is.na(mid_age)] 

  m = glm(fate_ ~ mid_age + mid_j, family=binomial(logexp(days=yya$exposure)),data=yya)  
  summary(m) 
  plot(allEffects(m))

  # predated vs hatched and abandoned
  yha = y[exposure>0.5 & fate<5] 
  
  m = glm(fate_ ~ 1, family=binomial(logexp(days=yha$exposure)),data=yha)  
  1-exp(m$coefficients)/(1+exp(m$coefficients))

  m = glm(fate_ ~ mid_j, family=binomial(logexp(days=yha$exposure)),data=yha)  
  summary(m) 
  plot(allEffects(m))

  # predated vs hatched and abandoned with known age
  yhaa = y[exposure>0.5 & fate<2 & !is.na(mid_age)] 
  m = glm(fate ~ mid_j + mid_age, family=binomial(logexp(days=yhaa$exposure)),data=yhaa)  
  summary(m) 
  plot(allEffects(m))
  
  yx = y[exposure>0.5 & !is.na(mid_age)]
  m = glm(fate_ ~ mid_j + mid_age, family=binomial(logexp(days=yx$exposure)),data=yx) 
  m = glm(fate_ ~ mid_j*year_ + mid_age, family=binomial(logexp(days=yx$exposure)),data=yx) 

  ys = y[exposure>0.5] 
  m = glm(fate_ ~ mid_j, family=binomial(logexp(days=ys$exposure)),data=ys) 
  m = glm(fate_ ~ mid_j*year_, family=binomial(logexp(days=ys$exposure)),data=ys) 
  summary(m) 
  plot(allEffects(m))

