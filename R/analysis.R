# LOAD TOOLS and DATA
  require(here)
  source(here::here('R/tools.R'))
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))

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
    m = lm(cases~poly(time_from_sunrise_r,2), ts)
    summary(m)
 
# seasonal pattern
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
      geom_point(aes(x = date_num, y = time_corr, col = night), xx)  +
      geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
      geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
      scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of day [hour]") + scale_x_continuous(expand = c(0, 0), name ="Day in year", lim = c(50,200))+
      scale_color_manual(values=c(day = day_, night = night_)) +
      theme_MB +
      theme(legend.title = element_blank())
    
    ggsave(file = 'Output/season_time-vertical.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')
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
    g2 = ggplot(xx, aes(y = night_num, x = date_num)) + geom_jitter(data = xx[night == 'day',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = day_) + geom_jitter(data = xx[night == 'night',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, col = night_) + stat_smooth( method="glm", method.args=list(family="binomial"), col = night_) + 
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
