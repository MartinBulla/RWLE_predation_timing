# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla
# 📍 This script runs relative to the project's root directory and generates
# Figure 4
# ==========================================================================

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
 # [1] directlabels_2020.6.17 ggpubr_0.4.0           forcats_0.5.0          dplyr_1.0.1           
 # [5] purrr_0.3.4            readr_1.3.1            tidyr_1.1.1            tibble_3.0.3          
 # [9] tidyverse_1.3.0        gt_0.2.2               zoo_1.8-8              xlsx_0.6.3            
 #[13] stringr_1.4.0          reshape2_1.4.4         raster_3.3-13          plyr_1.8.6            
 #[17] performance_0.4.8      multcomp_1.4-13        TH.data_1.0-10         survival_3.1-12       
 #[21] mvtnorm_1.1-1          maptools_1.0-1         sp_1.4-2               magrittr_1.5          
 #[25] lubridate_1.7.9        lattice_0.20-41        htmlTable_2.0.1        gridExtra_2.3         
 #[29] glue_1.4.2             ggthemes_4.2.0         ggplot2_3.3.2          ggExtra_0.9           
 #[33] effects_4.1-4          carData_3.0-4          data.table_1.13.0      arm_1.11-2            
 #[37] lme4_1.1-23            Matrix_1.2-18          MASS_7.3-51.6          here_0.1              
 #
 #loaded via a namespace (and not attached):
 # [1] minqa_1.2.4         colorspace_1.4-1    ggsignif_0.6.0      rio_0.5.16         
 # [5] ellipsis_0.3.1      rprojroot_1.3-2     fs_1.5.0            base64enc_0.1-3    
 # [9] rstudioapi_0.11     fansi_0.4.1         xml2_1.3.2          codetools_0.2-16   
 #[13] splines_4.0.2       knitr_1.29          Formula_1.2-3       jsonlite_1.7.0     
 #[17] nloptr_1.2.2.2      rJava_0.9-13        broom_0.7.0         cluster_2.1.0      
 #[21] dbplyr_1.4.4        png_0.1-7           shiny_1.5.0         compiler_4.0.2     
 #[25] httr_1.4.2          backports_1.1.8     assertthat_0.2.1    fastmap_1.0.1      
 #[29] cli_2.0.2           survey_4.0          later_1.1.0.1       acepack_1.4.1      
 #[33] htmltools_0.5.0     tools_4.0.2         coda_0.19-3         gtable_0.3.0       
 #[37] Rcpp_1.0.5          cellranger_1.1.0    vctrs_0.3.2         nlme_3.1-148       
 #[41] insight_0.9.0       xfun_0.16           xlsxjars_0.6.1      openxlsx_4.1.5     
 #[45] rvest_0.3.6         mime_0.9            miniUI_0.1.1.1      lifecycle_0.2.0    
 #[49] rstatix_0.6.0       statmod_1.4.34      scales_1.1.1        hms_0.5.3          
 #[53] promises_1.1.1      sandwich_2.5-1      RColorBrewer_1.1-2  curl_4.3           
 #[57] rpart_4.1-15        latticeExtra_0.6-29 stringi_1.5.3       bayestestR_0.7.2   
 #[61] checkmate_2.0.0     zip_2.0.4           boot_1.3-25         rlang_0.4.7        
 #[65] pkgconfig_2.0.3     htmlwidgets_1.5.1   tidyselect_1.1.0    R6_2.4.1           
 #[69] generics_0.0.2      Hmisc_4.4-0         DBI_1.1.0           pillar_1.4.6       
 #[73] haven_2.3.1         foreign_0.8-80      withr_2.2.0         abind_1.4-5        
 #[77] nnet_7.3-14         car_3.0-8           modelr_0.1.8        crayon_1.3.4       
 #[81] jpeg_0.1-8.1        readxl_1.3.1        blob_1.2.1          reprex_0.3.0       
 #[85] digest_0.6.25       xtable_1.8-4        httpuv_1.5.4        munsell_0.5.0      
 #[89] viridisLite_0.3.0   quadprog_1.5-8      mitools_2.4      