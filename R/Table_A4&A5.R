# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla, Martin Sladecek
# 📍 This script runs relative to the project's root directory and generates
# Table A4 and A5
# ==========================================================================

# TOOLS & DATA
  require(here)
  source(here::here('R/tools.R'))
  require(MuMIn)
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))

# GENERATE MODEL OUTPUTS
# (a) night predation over season
    mb=glm(night_num ~ date_num, family="binomial", data=x)
    #mb=glmer(night_num ~ date_num +(1|F_ID), family="binomial", data=x)
    mbp =glm(night_num ~ poly(date_num,2), family="binomial", data=x)
    a_night_bin = m_out(name = "(a) Season",  dep = "Night predation (0, 1)", fam = 'binomial', 
              N = nrow(x), type = "glm",  model = mb,
              round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_a_night-season_bin',
                 title = 'glm(night_num ~ date_num, family = "binomial", x)',
                 binomial = TRUE, mo = mb, dat = x, 
                 fixed = c('date_num'), categ = NULL, trans = c('none'), 
                 spatial = FALSE, temporal = TRUE, 
                 PNG = TRUE, outdir = "Output/ModelAss/")

    mg=lm(night_num ~ date_num, data=x)
    mgp = lm(night_num ~ poly(date_num,2), data=x)
    a_night_gaus = m_out(name = "(a) Season",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
              N = nrow(x), type = "lm",  model = mg,
              round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_a_night-season_gaus',
                 title = 'lm(night_num ~ date_num, x)',
                 binomial = TRUE, mo = mg, dat = x, 
                 fixed = c('date_num'), categ = NULL, trans = c('none'), 
                 spatial = FALSE, temporal = TRUE, 
                 PNG = TRUE, outdir = "Output/ModelAss/")
# (b) night predation given midday temperature
  tb=glm(night_num ~ midday_T, family="binomial", data=x)
  tbp  =glm(night_num ~ poly(midday_T,2), family="binomial", data=x)
  b_night_bin = m_out(name = "(b) Midday T",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(x), type = "glm",  model = tb,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_b_night-middayT_bin',
               title = 'glm(night_num ~ midday_T, family = "binomial", x)',
               binomial = TRUE, mo = tb, dat = x, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  tg=lm(night_num ~ midday_T, data=x)
  tgp = lm(night_num ~ poly(midday_T,2), data=x)
  b_night_gaus = m_out(name = "(b) Midday T",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = tg,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_b_night-middayT_gaus',
               title = 'lm(night_num ~ midday_T, x)',
               binomial = TRUE, mo = tg, dat = x, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")    
# (c) night predation given midday temperature and day in season
  dtb=glm(night_num ~ midday_T + date_num, family="binomial", data=x)
  c_night_bin = m_out(name = "(c) Midday T + date",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(x), type = "glm",  model = dtb,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_c_night-middayT-date_bin',
               title = 'glm(night_num ~ midday_T + date_num, family = "binomial", x)',
               binomial = TRUE, mo = dtb, dat = x, 
               fixed = c('midday_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  dtg=lm(night_num ~ midday_T + date_num, data=x)
  c_night_gaus = m_out(name = "(c) Midday T + date",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = dtg,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_c_night-middayT-date_gaus',
               title = 'lm(night_num ~ midday_T + date_num, x)',
               binomial = TRUE, mo = dtg, dat = x, 
               fixed = c('midday_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")        
# (d) night ~ reside temperature
  mx = lm(midday_T~ date_num,data=x)
  x$res_T = resid(mx)
  mbrd=glm(night_num ~ res_T + date_num, data = x, family="binomial")

  d_night_bin = m_out(name = "(d) res T + date",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(x), type = "glm",  model = mbrd,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_d_night-resT-date_bin',
               title = 'glm(night_num ~ res_T + date_num, data = x, family="binomial")',
               binomial = TRUE, mo = mbrd, dat = x, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  mgrd=lm(night_num ~ res_T + date_num, data = x)
  d_night_gaus = m_out(name = "(d) res T + date",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = mgrd,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_d_night-resT-date_gaus',
               title = 'lm(night_num ~ res_T + date_num, data = x)',
               binomial = TRUE, mo = mgrd, dat = x, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")        
# (e) night ~ reside temperature
  my = lm(date_num ~ midday_T,data=x)
  x$res_D = resid(my)
  mbrt=glm(night_num ~ res_D + midday_T, data = x, family="binomial")
  e_night_bin = m_out(name = "(e) res date + T",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(x), type = "glm",  model = mbrt,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_e_night-resDate-T_bin',
               title = 'glm(night_num ~ res_D + midday_T, data = x, family="binomial")',
               binomial = TRUE, mo = mbrt, dat = x, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  mgrt=glm(night_num ~ res_D + midday_T, data = x)
  e_night_gaus = m_out(name = "(e) res date + T",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = mgrt,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_e_night-resDate-T_gaus',
               title = 'glm(night_num ~ res_D + midday_T, data = x)',
               binomial = TRUE, mo = mgrt, dat = x, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")       

# (f) midday T ~ date
    m = lm(midday_T~ date_num,data=x)
    f_midTdate = m_out(name = "(f) Residual T",  dep = "Midday T", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = m,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_f_midTdate_gaus',
               title = 'lm(midday_T~ date_num,data=x)',
               binomial = TRUE, mo = m, dat = x, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")
# (g) date ~ middayT
    m = lm(date_num ~ midday_T,data=x)
    g_dateMidT = m_out(name = "(g) Residual date",  dep = "Date", fam = 'Gaussian', 
            N = nrow(x), type = "lm",  model = m,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_g_date_gaus',
               title = 'lm(date_num~ midday_T,data=x)',
               binomial = TRUE, mo = m, dat = x, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

# AICc 
 aic = data.table(model = rownames(AICc(mg,  tg,  dtg, mgrd, mgrt)), predictors = c('Season', 'Midday Temperature', 'Season & T', 'Season & residual T', 'T & residual season'), AICc(mb, tb, dtb, mbrd, mbrt))
 aic[, deltaAICc := AICc-min(AICc)]
 aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
 aic[, ER := round(max(prob)/prob, 2)]
 aicg = aic[order(deltaAICc)]

 aic = data.table(model = rownames(AICc(mb,  tb,  dtb, mbrd, mbrt)), predictors = c('Season', 'Midday Temperature', 'Season & T', 'Season & residual T', 'T & residual season'), AICc(mb, tb, dtb, mbrd, mbrt))
 aic[, deltaAICc := AICc-min(AICc)]
 aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
 aic[, ER := round(max(prob)/prob, 2)]
 aicb = aic[order(deltaAICc)]

 aic = rbind(aicb,aicg)

 aic[, AICc := round(AICc,2)]
 aic[, deltaAICc := round(deltaAICc,2)]
 aic[, prob := round(prob,2)]
 aic[, ER := round(ER,2)]

# COMBINE AND EXPORT
  o = rbind( 
            a_night_bin, a_night_gaus,
            b_night_bin, b_night_gaus,
            c_night_bin, c_night_gaus,
            d_night_bin, d_night_gaus,
            e_night_bin, e_night_gaus,
            f_midTdate, 
            g_dateMidT
          )  
  fwrite(file = "./Output/Table_A4.csv", o)

  fwrite(file = "./Output/Table_A5-AIC.csv", aic)

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
 # [1] MuMIn_1.43.17     ggpubr_0.4.0      forcats_0.5.0     dplyr_1.0.1       purrr_0.3.4      
 # [6] readr_1.3.1       tidyr_1.1.1       tibble_3.0.3      tidyverse_1.3.0   gt_0.2.2         
 #[11] zoo_1.8-8         xlsx_0.6.3        stringr_1.4.0     reshape2_1.4.4    raster_3.3-13    
 #[16] plyr_1.8.6        performance_0.4.8 multcomp_1.4-13   TH.data_1.0-10    survival_3.1-12  
 #[21] mvtnorm_1.1-1     maptools_1.0-1    sp_1.4-2          magrittr_1.5      lubridate_1.7.9  
 #[26] lattice_0.20-41   htmlTable_2.0.1   gridExtra_2.3     glue_1.4.2        ggthemes_4.2.0   
 #[31] ggplot2_3.3.2     ggExtra_0.9       effects_4.1-4     carData_3.0-4     data.table_1.13.0
 #[36] arm_1.11-2        lme4_1.1-23       Matrix_1.2-18     MASS_7.3-51.6     here_0.1         
 #
 #loaded via a namespace (and not attached):
 # [1] minqa_1.2.4         colorspace_1.4-1    ggsignif_0.6.0      rio_0.5.16         
 # [5] ellipsis_0.3.1      rgdal_1.5-16        rprojroot_1.3-2     fs_1.5.0           
 # [9] base64enc_0.1-3     rstudioapi_0.11     fansi_0.4.1         xml2_1.3.2         
 #[13] codetools_0.2-16    splines_4.0.2       knitr_1.29          Formula_1.2-3      
 #[17] jsonlite_1.7.0      nloptr_1.2.2.2      rJava_0.9-13        broom_0.7.0        
 #[21] cluster_2.1.0       dbplyr_1.4.4        png_0.1-7           shiny_1.5.0        
 #[25] compiler_4.0.2      httr_1.4.2          backports_1.1.8     assertthat_0.2.1   
 #[29] fastmap_1.0.1       cli_2.0.2           survey_4.0          later_1.1.0.1      
 #[33] acepack_1.4.1       htmltools_0.5.0     tools_4.0.2         coda_0.19-3        
 #[37] gtable_0.3.0        Rcpp_1.0.5          cellranger_1.1.0    vctrs_0.3.2        
 #[41] nlme_3.1-148        insight_0.9.0       xfun_0.16           xlsxjars_0.6.1     
 #[45] openxlsx_4.1.5      rvest_0.3.6         mime_0.9            miniUI_0.1.1.1     
 #[49] lifecycle_0.2.0     rstatix_0.6.0       statmod_1.4.34      scales_1.1.1       
 #[53] hms_0.5.3           promises_1.1.1      sandwich_2.5-1      RColorBrewer_1.1-2 
 #[57] curl_4.3            rpart_4.1-15        latticeExtra_0.6-29 stringi_1.5.3      
 #[61] bayestestR_0.7.2    checkmate_2.0.0     zip_2.0.4           boot_1.3-25        
 #[65] rlang_0.4.7         pkgconfig_2.0.3     htmlwidgets_1.5.1   tidyselect_1.1.0   
 #[69] R6_2.4.1            generics_0.0.2      Hmisc_4.4-0         DBI_1.1.0          
 #[73] pillar_1.4.6        haven_2.3.1         foreign_0.8-80      withr_2.2.0        
 #[77] abind_1.4-5         nnet_7.3-14         car_3.0-8           modelr_0.1.8       
 #[81] crayon_1.3.4        jpeg_0.1-8.1        readxl_1.3.1        blob_1.2.1         
 #[85] reprex_0.3.0        digest_0.6.25       xtable_1.8-4        httpuv_1.5.4       
 #[89] stats4_4.0.2        munsell_0.5.0       mitools_2.4 