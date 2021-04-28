# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla
# 📍 This script runs relative to the project's root directory and generates
# Figure 1
# ==========================================================================

# LOAD TOOLS and DATA
  require(here)
  source(here::here('R/tools.R'))
  
  day_ = 'lightgrey'
  night_ = 'grey30'

  gtable_filter_remove <- function (x, name, trim = TRUE){
    matches <- !(x$layout$name %in% name)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    if (trim) 
      x <- gtable_trim(x)
    x
  }

  a=fread("Data/potential_predators_ebird.txt")
  names(a) = c('pk', 'name','scinam','n','country', 'site_ID','lat','lon','date','time','comments','day_j','month')
  summary(factor(a$name))
  unique(a$scinam)
  unique(a$name)

  a[, genus := sub("\\ .*", "", scinam)]
  summary(factor(a$genus))
  a = a[day_j<201 & day_j>49]
  a[n =='X', n :=1]
  a[,n := as.numeric(n)]

# PLOT
  g = ggplot(a, aes(x = day_j, fill = genus)) + geom_histogram() + 
    ylim(c(0,15)) +
    ylab('# of observation') + 
    xlab('Day of the year') + 
    facet_wrap(~genus, ncol = 7) + 
    theme_MB + theme(legend.position = "none") 
  g_tab <- ggplotGrob(g)
  #g_tab$layout$name
  g_filtered <- gtable_filter_remove(g_tab, name = paste0("axis-b-", c(2, 4, 6), "-3"), trim = FALSE)
  #grid.newpage()
  grid.draw(g_filtered)
  ggsave(file = 'Output/Fig_predators_hist.png',arrangeGrob(g_filtered), width = 12, height =6, units = 'cm')
  
# other
  ggplot(a, aes(x = day_j, col = name)) + geom_density()
  ggplot(a, aes(x = day_j, col = genus)) + geom_density()
  ggplot(a, aes(x = day_j, col = genus)) + geom_density() + facet_wrap(~genus, ncol = 7)
  ggplot(a, aes(x = day_j)) + geom_density() 
  ggplot(a, aes(x = day_j)) + geom_histogram() + ylab('# of observation')
  ggplot(a, aes(x = day_j, y = n)) + geom_point(pch = 16, alpha =0.5) + scale_y_log10() 
  ggplot(a, aes(x = as.factor(month), y = n)) + geom_boxplot() + scale_y_log10() 
  ggplot(a, aes(x = as.factor(month), y = n)) + geom_violin() + scale_y_log10() 
   
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