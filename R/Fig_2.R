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

# (a) distribution of predation events across season
  ga = 
    ggplot(x, aes(x = date_num, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0), name = "Day in year") + scale_y_continuous(expand = c(0, 0), name ="Predation", breaks = c(0,1,2))+#, labels = c("1.00", '2.00', '3.00'))+
      scale_fill_manual(values=c(day = day_, night = night_))+
      annotate("text", x=32, y=1, label= "[count]", size =7*(1/72 * 25.4), hjust = 0.5, angle = 90, colour="grey30") + 
      coord_cartesian(xlim = c(50,200), clip = 'off') +                
      theme_MB +
      #labs(tag = "(a)") +
      theme(legend.position = c(0.5, 0.85),
            legend.title = element_blank(),
            legend.text=element_text(size=5, margin = margin(l = -4, unit = "pt")),
            legend.key.height= unit(0.25,"line"),
            legend.key.width = unit(0.15, "cm"),
            plot.tag.position = 'topleft', #c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold"), # size = 10)
            axis.title.x = element_blank(),
            axis.text.x = element_blank()#, vjus
            )
  
# (b) temperature at predation isotherms
  gb = 
    ggplot() + 
    geom_ribbon(aes(ymin = sunss , ymax = 24, x = date_num),ss2, fill = "grey95")+ #geom_path(aes(y = sunss , x = date_num),ss,  size = 0.25, col = "grey") +
    geom_area(aes(y =sunrs, x = date_num),ss2,fill = "grey95") + #geom_path(aes(y = sunrs , x = date_num),ss,  size = 0.25, col = "grey") +
    stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 45, size = 0.25, colour = isotherm45) +
    stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 40, size = 0.25, colour = isotherm40) +
    stat_contour(data = meanTL, aes(x = date_num, y = hr, z = meanT),  breaks = 35, size = 0.25, colour = isotherm35) +
    #geom_point(aes(x = date_num, y = time, col = temperature, fill = temperature), xx)  +
    geom_point(aes(x = date_num, y = time,  fill = temperature), data = xx, shape=21, col = point_out) +
    scale_y_reverse(expand = c(0, 0), lim = c(24,0), breaks = seq(0,24, by = 1), labels = c(0,"","2","","4","","6","","8","","10","","12","","14","","16","","18","","20","","22","","24"), name = "Time of predation") + 
    scale_x_continuous(expand = c(0, 0), name ="Day in year")+
    coord_cartesian(xlim = c(50,200), clip = 'off') + 
    #scale_colour_viridis_c(option = "plasma", name = "C°")+ 
    scale_fill_viridis_c(option = "plasma", name = "C°")+ 
    annotate("text", x=32, y=12, label= "[hour]", size =7*(1/72 * 25.4), hjust = 0.5, angle = 90, colour="grey30") + 
    annotate("text", x=55, y=3, label= "night", size =1.7, hjust = 0) + 
    annotate("text", x =55, y=12, label = "day", size =1.7, hjust = 0)+
    annotate("text", x =55, y=21, label = "night", size =1.7, hjust = 0)+
    annotate("text", x =104, y=10, label = "35C°", size =1.7, col = isotherm35)+
    annotate("text", x =128, y=10, label = "40C°", size =1.7, col = isotherm40)+
    annotate("text", x =165, y=10, label = "45C°", size =1.7, col = isotherm45)+
    #labs(tag = "(b)") +
    theme_MB +
    theme(  
            plot.tag.position = 'topleft', #c(0.025, 0.98), #plot.tag.position = c(0.42, 0.96),
            plot.tag = element_text(size = 6, face = "bold"), # size = 10)
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),  
            legend.text=element_text(size=5),
            legend.title=element_text(size=6, hjust = 0.5)
            )

    legend = get_legend(gb)
    gbb = gb + theme(legend.position="none")    

    #ggsave(file = 'Output/season_time-vertical_night-area_TatPred_limitMinMAxAll_ISO_middle_DMright.png', gg, dpi = 300, width = 7, height = 5, units = 'cm')

# (c) probability of night predation    
  gc = 
    ggplot() + 
      #stat_smooth( method="glm", method.args=list(family="binomial"), col = night_, size=0.5) + 
      geom_ribbon(data = pp,aes(ymin=lwr, ymax=upr, x=date_num), fill = night_, alpha = 0.2, show.legend = NA) +
      geom_line(data = pp,aes(x = date_num, y = pred), col = night_) +
      geom_jitter(data = x[night == 'day',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, fill = day_, col = point_out, pch = 21) + 
      geom_jitter(data = x[night == 'night',] , aes(y = night_num, x = date_num), width = 0, height = 0.025, fill = night_, col = point_out2, pch = 21) + 
      scale_x_continuous(expand = c(0, 0), lim = c(50,200),  name = "Day in year") +scale_y_continuous(  name = "Probability of night vs day predation") +
      #labs(tag = "(c)") +
      theme_MB +
      theme(plot.tag.position = 'topleft', #c(0.81, 0.98),
            plot.tag = element_text(size = 6, face = "bold") # size = 10)
            )
     
# arrange and export
  blank = ggplot() + theme_void() 
  ggA = ggarrange(
    ga + theme(plot.margin = unit(c(1.5,1.75,0.3,2), "mm")),  
    gbb + theme(plot.margin = unit(c(1.5,1.75,0.3,2), "mm")), 
    gc + theme(plot.margin = unit(c(1.5,1.75,0,2), "mm")),
    nrow=3, heights=c(1, 4, 4.45),  align = 'v', 
    labels = c('(a)','(b)', '(c)'), font.label =list(size = 6, face = "bold", color = "grey30"), hjust = c(-4.3, -4.1, -4.3), vjust = 2.5)
  ggL = ggarrange(blank, legend, blank, nrow=3, heights=c(1, 4, 4.45))
  ggAll = ggarrange(ggA, ggL, ncol=2, widths=c(4, 0.45))

  ggsave(file = 'Output/Fig_2_7cm-width.png', ggAll, dpi = 300, width = 7, height = 12.5, units = 'cm')
    #ggExtra::ggMarginal(gg, type = "histogram")#, groupColour = TRUE)

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
  #[1] grid      stats     graphics  grDevices utils     datasets  methods   base     
  #
  #other attached packages:
  # [1] directlabels_2020.6.17 ggpubr_0.4.0           forcats_0.5.0         
  # [4] dplyr_1.0.1            purrr_0.3.4            readr_1.3.1           
  # [7] tidyr_1.1.1            tibble_3.0.3           tidyverse_1.3.0       
  #[10] gt_0.2.2               zoo_1.8-8              xlsx_0.6.3            
  #[13] stringr_1.4.0          reshape2_1.4.4         raster_3.3-13         
  #[16] plyr_1.8.6             performance_0.4.8      multcomp_1.4-13       
  #[19] TH.data_1.0-10         survival_3.1-12        mvtnorm_1.1-1         
  #[22] maptools_1.0-1         sp_1.4-2               magrittr_1.5          
  #[25] lubridate_1.7.9        lattice_0.20-41        htmlTable_2.0.1       
  #[28] gridExtra_2.3          glue_1.4.2             ggthemes_4.2.0        
  #[31] ggplot2_3.3.2          ggExtra_0.9            effects_4.1-4         
  #[34] carData_3.0-4          data.table_1.13.0      arm_1.11-2            
  #[37] lme4_1.1-23            Matrix_1.2-18          MASS_7.3-51.6         
  #[40] here_0.1              
  #
  #loaded via a namespace (and not attached):
  # [1] readxl_1.3.1        backports_1.1.8     Hmisc_4.4-0         splines_4.0.2      
  # [5] digest_0.6.25       htmltools_0.5.0     fansi_0.4.1         checkmate_2.0.0    
  # [9] cluster_2.1.0       openxlsx_4.1.5      modelr_0.1.8        sandwich_2.5-1     
  #[13] jpeg_0.1-8.1        colorspace_1.4-1    blob_1.2.1          rvest_0.3.6        
  #[17] mitools_2.4         haven_2.3.1         xfun_0.16           rgdal_1.5-16       
  #[21] crayon_1.3.4        jsonlite_1.7.0      gtable_0.3.0        car_3.0-8          
  #[25] abind_1.4-5         scales_1.1.1        DBI_1.1.0           rstatix_0.6.0      
  #[29] miniUI_0.1.1.1      Rcpp_1.0.5          isoband_0.2.2       viridisLite_0.3.0  
  #[33] xtable_1.8-4        foreign_0.8-80      Formula_1.2-3       survey_4.0         
  #[37] htmlwidgets_1.5.1   httr_1.4.2          RColorBrewer_1.1-2  acepack_1.4.1      
  #[41] ellipsis_0.3.1      pkgconfig_2.0.3     rJava_0.9-13        farver_2.0.3       
  #[45] nnet_7.3-14         dbplyr_1.4.4        labeling_0.3        tidyselect_1.1.0   
  #[49] rlang_0.4.7         later_1.1.0.1       munsell_0.5.0       cellranger_1.1.0   
  #[53] tools_4.0.2         cli_2.0.2           generics_0.0.2      broom_0.7.0        
  #[57] fastmap_1.0.1       knitr_1.29          fs_1.5.0            zip_2.0.4          
  #[61] nlme_3.1-148        mime_0.9            xml2_1.3.2          compiler_4.0.2     
  #[65] rstudioapi_0.11     curl_4.3            png_0.1-7           ggsignif_0.6.0     
  #[69] reprex_0.3.0        statmod_1.4.34      stringi_1.5.3       nloptr_1.2.2.2     
  #[73] vctrs_0.3.2         pillar_1.4.6        lifecycle_0.2.0     cowplot_1.0.0      
  #[77] insight_0.9.0       httpuv_1.5.4        R6_2.4.1            latticeExtra_0.6-29
  #[81] promises_1.1.1      rio_0.5.16          codetools_0.2-16    boot_1.3-25        
  #[85] assertthat_0.2.1    xlsxjars_0.6.1      rprojroot_1.3-2     withr_2.2.0        
  #[89] bayestestR_0.7.2    hms_0.5.3           quadprog_1.5-8      rpart_4.1-15       
  #[93] coda_0.19-3         minqa_1.2.4         shiny_1.5.0         base64enc_0.1-3