# TOOLS
  require(here)
  source(here::here('R/tools.R'))
  require(directlabels)
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  
  # colors
    day_ = 'lightgrey'
    night_ = 'grey30'
    point_out = 'grey30'
    point_out2 = 'grey40'

    vir_ = scales::viridis_pal(option = "plasma")(13)
    
    isotherm35 = scales::viridis_pal(option = "plasma")(13)[6]
    isotherm40 = scales::viridis_pal(option = "plasma")(13)[7]
    isotherm45 = scales::viridis_pal(option = "plasma")(13)[8]
    isotherm50 = "#FA9E3BFF"
    isotherm60 = "#FDC926FF"
    isotherm70 = "#F0F921FF"
    
# PREPARE DATA
  source(here::here('R/prepare_data.R'))

  # dataset with dummy variable, to get the temperature-legend for a full range of temperature data
    x1 = x[,.(midday_T, night_num, temperature)]
    x2 = data.frame(midday_T = c(20,20, 20, 20), night_num = c(1,1, 0,0), temperature = c(c(min(a[, mean]),max(a[, mean])),c(min(a[, mean]),max(a[, mean]))) )
    xx = rbind(x1,x2)

# Predictions
  m=glm(night_num ~ midday_T, family="binomial", data=x)
  bsim = sim(m, n.sim=nsim)  
    
  v = apply(bsim@coef, 2, quantile, prob=c(0.5)) # coefficients
  newD=data.frame(midday_T = seq(min(x$midday_T), max(x$midday_T), length.out = 300)) # values to predict for
  X <- model.matrix(~ midday_T,data=newD) # exactly the model which was used has to be specified here 
    
  # calculate predicted values and creditability intervals
    newD$pred <-plogis(X%*%v) 
    predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
    for(i in 1:nsim) {predmatrix[,i] <- plogis(X%*%bsim@coef[i,])}
                    predmatrix[predmatrix < 0] <- 0
                    newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                    newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                    #newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
                 
    pp=newD 
  


  # dataset with dummy variable, to get the temperature-legend for a full range of temperature data
    x1 = x[,.(date_num, time, temperature)]
    x2 = data.frame(date_num = c(1,1), time = c(12,12), temperature = c(min(a[, mean]),max(a[, mean]))   ) 
    xx = rbind(x1,x2)

  # dataset for isotherm
    iso_loess <- loess(mean ~ date_num * hr, data = a)
    
    # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
      xgrid <-  seq(min(a$date_num), 200, 1)
      ygrid <-  seq(min(a$hr), max(a$hr), 0.2)
    
    # Generate a dataframe with every possible combination of wt and hp
      newD <-  expand.grid(date_num = xgrid, hr = ygrid)
    # Feed the dataframe into the loess model and receive a matrix output with estimates of hour for each combination of date and mean temperature
      meanT <-  predict(iso_loess, newdata = newD)
      #meanT[1:4, 1:4] #  # Abbreviated display of final matrix
      #contour(x = xgrid, y = ygrid, z = meanT, xlab = 'day', ylab = 'hour')

    # Transform data to long form
      meanTL <- melt(meanT, id.vars = c('date_num', 'hr'), measure.vars = 'meanT')
      names(meanTL) <- c('date_num', 'hr', 'meanT')
    
    # Return data to numeric form
      meanTL$date_num <- as.numeric(str_sub(meanTL$date_num, str_locate(meanTL$date_num, '=')[1,1] + 1))
      meanTL$hr <- as.numeric(str_sub(meanTL$hr, str_locate(meanTL$hr, '=')[1,1] + 1))
      #head(meanTL)  

      #p = ggplot()+stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = ..level..), breaks = 45, size = 1); direct.label(p, 'bottom.pieces')
      #p =  ggplot()+stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = ..level..), breaks = c(15,20,25,30,35,40,45), size = 0.5)#direct.label(p, 'bottom.pieces')
      #p =  ggplot()+ stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT, colour = as.factor(..level..)), breaks = c(35,40,45), size = 0.5) +scale_colour_manual(values = vir_[6:8]);direct.label(p, 'bottom.pieces')
  # dataset for sunrise and sunset
      dats=seq.POSIXt(from=as.POSIXct("2019-02-19 UTC"), # day 50
                      to=as.POSIXct("2019-07-19 UTC"),by=24*3600)
      sunrs=crepuscule(koord,dats,direction="dawn",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
      sunss=crepuscule(koord,dats,direction="dusk",solarDep=6, POSIXct.out=TRUE)[,2]+4*3600
      sunrs=(as.numeric(sunrs) %% (24*3600))/3600 
      sunss=(as.numeric(sunss) %% (24*3600))/3600 
      ss2 = data.table(date = dats, sunrs = sunrs, sunss = sunss)
      ss2[,date_num := as.numeric(strftime(date,format="%j"))]
  # predicted relationships for (c)
    m=glm(night_num~date_num,data=x,family="binomial")
    bsim = sim(m, n.sim=nsim)  
    
    v = apply(bsim@coef, 2, quantile, prob=c(0.5)) # coefficients
    newD=data.frame(date_num = seq(min(x$date_nu), max(x$date_nu), length.out = 300)) # values to predict for
    X <- model.matrix(~ date_num,data=newD) # exactly the model which was used has to be specified here 
    
    # calculate predicted values and creditability intervals
    newD$pred <-plogis(X%*%v) 
    predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
    for(i in 1:nsim) {predmatrix[,i] <- plogis(X%*%bsim@coef[i,])}
                    predmatrix[predmatrix < 0] <- 0
                    newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
                    newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
                    #newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
                 
    pp=newD   
# Plot
   gg = 
    ggplot() + 
      #stat_smooth( method="glm", method.args=list(family="binomial"), col = night_, size=0.5) + 
      geom_ribbon(data = pp,aes(ymin=lwr, ymax=upr, x=midday_T), fill = night_, alpha = 0.2, show.legend = NA) +
      geom_line(data = pp,aes(x = midday_T, y = pred), col = night_) +
      geom_jitter(data = xx[night_num == 0] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025, , col = point_out, pch = 21) + 
      geom_jitter(data = xx[night_num == 1] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025,  col = point_out2, pch = 21) + 
      #geom_jitter(aes(x = midday_T, y = night_num,  fill = temperature), data = xx, shape=21, col = point_out) +
      scale_fill_viridis_c(option = "plasma", name = "C°\nwhen\npredated") +
      scale_x_continuous(expand = c(0, 0), lim = c(30,60),  name = "Mid-day T [°C]") +
      scale_y_continuous(  name = "Probability of night vs day predation") +
      coord_cartesian(xlim = c(30,61), clip = 'off') + 
      #labs(tag = "(c)") +
      theme_MB +
      theme(  
            legend.text=element_text(size=5),
            legend.title=element_text(size=6, hjust = 0.5)
            )

  ggsave(file = 'Output/Fig_3_7cm-width.png', gg, dpi = 300, width = 7, height = 12.5* 4.45/9.45, units = 'cm')
    