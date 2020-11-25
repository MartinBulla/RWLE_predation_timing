# TOOLS & DATA
  require(here)
  source(here::here('R/tools.R'))
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))

# 24h pattern
  ga = 
    ggplot(xx, aes(x = time_corr, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
      scale_x_continuous(expand = c(0, 0), lim = c(0,24), breaks = seq(0,24, by = 1), labels = c(0,"","","","4","","","","8","","","","12","","","","16","","","","20","","","","24"), name = "Time of day\n[hour]") + scale_y_continuous(expand = c(0, 0), name ="Predation\n[count]")+
      scale_fill_manual(values=c(day = day_, night = night_)) + 
      labs(tag = "(a)") +
      theme_MB +
      theme(legend.position = "none",
            legend.title = element_blank(),
            plot.tag.position = c(0.97, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
# sunrise pattern  
  gb = 
    ggplot(xx, aes(x = time_from_sunrise, fill = night)) + geom_histogram(binwidth = 1, center = 0.5) + 
          scale_x_continuous(expand = c(0, 0), lim = c(-12,12), breaks = seq(-12,12, by = 1), labels = c("-12", "", "", "", "-8", "", "", "", "-4", "", "", "", 0,"","","","4","","","","8","","","","12"), name = "Time relative to sunrise\n[hour]") + scale_y_continuous(expand = c(0, 0), name ="Predation\n[count]")+
          scale_fill_manual(values=c(day = day_, night = night_)) + labs(tag = "(b)") +
      theme_MB +
      theme(legend.title = element_blank(),
            legend.text=element_text(size=5, margin = margin(l = -4, unit = "pt")),
            legend.key.height= unit(0.25,"line"),
            legend.key.width = unit(0.15, "cm"),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),  
            plot.tag.position = c(0.97, 0.98),
            plot.tag = element_text(size = 6, face = "bold") 
            )
      legend = get_legend(gb)
      gbb = gb + theme(legend.position="none") 

# arrange and export
  gga <- ggplotGrob(ga)
  ggb <- ggplotGrob(gbb)
    
  g <- cbind(gga,ggb, size = "last") 

  grid.arrange(
          arrangeGrob(g),
          legend, ncol = 2, widths=c(5,0.5)
          )
  ggALL = arrangeGrob(
          arrangeGrob(g),
          legend, ncol = 2, widths=c(5,0.5)
          )

  ggsave(file = 'Output/Fig_1_8cm-width.png', ggALL, dpi = 300, width = 8, height = 4, units = 'cm')    

  # did not work well
  ggarrange(ga, 
            gbb + theme(plot.margin = unit(c(1.5,1.75,0,0), "mm")), 
            align = "hv")

  ggALL = ggarrange(ga, gbb, legend, ncol=3,  widths=c(5, 5, 1))#,labels = 'AUTO', font.label =list(size = 6, face = "bold"), hjust = -25, vjust = 2.5)#, common.legend = TRUE, legend = "right")
  
  ggsave(file = 'Output/Fig_1.png', ggALL, dpi = 300, width = 8, height = 4, units = 'cm')    


  # test example
  require(data.table); require(ggplot2); require(cowplot); require(ggpubr)
  dat1 = data.table(hour = sample(1:24, 40, replace=T), 
                    day_night = sample(c('night', 'day'), 40, replace = T))
  dat2 = data.table(hour = sample(1:24, 40, replace=T), 
                    day_night = sample(c('night', 'day'), 40, replace = T))
  
  g1 = ggplot(dat1, aes(x = hour, fill = day_night)) + geom_histogram() + ylim(0,5) + xlim(0,25) 
  g2 = ggplot(dat2, aes(x = hour, fill = day_night)) + geom_histogram() + ylim(0,5) + xlim(0,25)

  ggarrange(g1, g2, common.legend = TRUE, legend = "right", labels = "AUTO")

  #Works well, but the second plot has the y-axis.

  #If I suppress the y axis in B, B plot becomes larger. However, I want this plot to have the same plotting area (same length of x-axis as A)
  ggarrange(g1, g2 + theme(axis.title.y = element_blank(),axis.text.y = element_blank()), 
      common.legend = TRUE, legend = "right", labels = "AUTO")
  
  # horizontal align does not do the trick, but vertical align does. Yet there is a big gap between the two plots. Is there a way to bring them closer?
  ggarrange(g1, g2 + theme(axis.title.y = element_blank(),axis.text.y = element_blank()), 
      common.legend = TRUE, legend = "right", labels = "AUTO", align = "v")
  

  # I have attempted the same by adding the legend manually, but here again B is larger
  legend_ = get_legend(g2)
  g2b = g2 + theme(legend.position="none", axis.title.y = element_blank(),axis.text.y = element_blank()) 
  g1b = g1 + theme(legend.position="none") 
  ggarrange(g1b, g2b, legend_, ncol=3,  widths=c(5, 5, 3))

  # here, I can fix that with align thus attempted to add the legend manually, but here again B is larger
  ggarrange(ggarrange(g1b, g2b, align = "v"), legend_, ncol=2,  widths=c(7,2))


    gg1 <- ggplotGrob(g1b)
    gg2 <- ggplotGrob(g2b)
    
   g <- cbind(gg1,gg2, size = "last") 


  grid.arrange(
         arrangeGrob(g),
          legend_, ncol = 2, widths=c(5,1)
          )



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
  # [1] minqa_1.2.4         colorspace_1.4-1    ggsignif_0.6.0      rio_0.5.16         
  # [5] ellipsis_0.3.1      rgdal_1.5-16        rprojroot_1.3-2     fs_1.5.0           
  # [9] base64enc_0.1-3     rstudioapi_0.11     farver_2.0.3        fansi_0.4.1        
  #[13] xml2_1.3.2          codetools_0.2-16    splines_4.0.2       knitr_1.29         
  #[17] Formula_1.2-3       jsonlite_1.7.0      nloptr_1.2.2.2      rJava_0.9-13       
  #[21] broom_0.7.0         cluster_2.1.0       dbplyr_1.4.4        png_0.1-7          
  #[25] shiny_1.5.0         compiler_4.0.2      httr_1.4.2          backports_1.1.8    
  #[29] assertthat_0.2.1    fastmap_1.0.1       cli_2.0.2           survey_4.0         
  #[33] later_1.1.0.1       acepack_1.4.1       htmltools_0.5.0     tools_4.0.2        
  #[37] coda_0.19-3         gtable_0.3.0        Rcpp_1.0.5          cellranger_1.1.0   
  #[41] vctrs_0.3.2         nlme_3.1-148        insight_0.9.0       xfun_0.16          
  #[45] xlsxjars_0.6.1      openxlsx_4.1.5      rvest_0.3.6         mime_0.9           
  #[49] miniUI_0.1.1.1      lifecycle_0.2.0     rstatix_0.6.0       statmod_1.4.34     
  #[53] scales_1.1.1        hms_0.5.3           promises_1.1.1      sandwich_2.5-1     
  #[57] RColorBrewer_1.1-2  curl_4.3            rpart_4.1-15        latticeExtra_0.6-29
  #[61] stringi_1.5.3       bayestestR_0.7.2    checkmate_2.0.0     zip_2.0.4          
  #[65] boot_1.3-25         rlang_0.4.7         pkgconfig_2.0.3     labeling_0.3       
  #[69] htmlwidgets_1.5.1   cowplot_1.0.0       tidyselect_1.1.0    R6_2.4.1           
  #[73] generics_0.0.2      Hmisc_4.4-0         DBI_1.1.0           pillar_1.4.6       
  #[77] haven_2.3.1         foreign_0.8-80      withr_2.2.0         abind_1.4-5        
  #[81] nnet_7.3-14         car_3.0-8           modelr_0.1.8        crayon_1.3.4       
  #[85] jpeg_0.1-8.1        readxl_1.3.1        blob_1.2.1          reprex_0.3.0       
  #[89] digest_0.6.25       xtable_1.8-4        httpuv_1.5.4        munsell_0.5.0      
  #[93] mitools_2.4        
  #
  #
 