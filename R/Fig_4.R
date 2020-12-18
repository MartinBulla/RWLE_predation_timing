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
  
# Plot
   gg = 
    ggplot() + 
      #stat_smooth( method="glm", method.args=list(family="binomial"), col = night_, size=0.5) + 
      geom_ribbon(data = pp,aes(ymin=lwr, ymax=upr, x=midday_T), fill = night_, alpha = 0.2, show.legend = NA) +
      geom_line(data = pp,aes(x = midday_T, y = pred), col = night_) +
      geom_jitter(data = xx[night_num == 0] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025, , col = point_out, pch = 21) + 
      geom_jitter(data = xx[night_num == 1] , aes(y = night_num, x = midday_T,fill = temperature), width = 0, height = 0.025,  col = point_out2, pch = 21) + 
      #geom_jitter(aes(x = midday_T, y = night_num,  fill = temperature), data = xx, shape=21, col = point_out) +
      scale_fill_viridis_c(option = "plasma", name = "°C\nwhen\npredated") +
      scale_x_continuous(expand = c(0, 0), lim = c(30,60),  name = "Mid-day T [°C]") +
      scale_y_continuous(  name = "Probability of night vs day predation") +
      coord_cartesian(xlim = c(30,61), clip = 'off') + 
      #labs(tag = "(c)") +
      theme_MB +
      theme(  
            legend.text=element_text(size=5),
            legend.title=element_text(size=6, hjust = 0.5)
            )

   ggsave(file = 'Output/Fig_4_width-7cm.png', gg, dpi = 300, width = 7, height = 12.5* 4.45/9.45, units = 'cm')
    