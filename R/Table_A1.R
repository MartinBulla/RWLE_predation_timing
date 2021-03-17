# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla, Martin Sladecek
# 📍 This script runs relative to the project's root directory and generates
# Table A1
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
  yy = y[exposure>0]
  yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
  yy[is.na(fate_), fate_:= 0]    # 0 all other
  yy[, year_:= as.factor(year)] 
  yy[ , success := round(exposure - fate_)]
  yy[fate_==1, failure := 1]
  yy[is.na(failure), failure := 0]

# daily predation rate according to Aebischer - logistic regression
  ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
  mas=glm(cbind(failure,success)~mid_j,family="binomial",data=yy)

  Aebischer_b = m_out(name = "(a) Original scale",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = ma,
          round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = TRUE, perc_ = 100)
  
  Aebischer = m_out(name = "(a) Binomial scale",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = ma,
          round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE, perc_ = 1)

  Aebischer_Sb = m_out(name = "(b) Original scale",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = mas,
          round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = TRUE, perc_ = 100)

  Aebischer_S = m_out(name = "(b) Binomial scale",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = mas,
          round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE)

# ADD AICc
  aic = data.table(model = rownames(AICc(ma,  mas)), predictors = c('none', 'season'), AICc(ma, mas))
  aic[, deltaAICc := AICc-min(AICc)]
  aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
  aic[, ER := round(max(prob)/prob, 2)]
  aic[order(deltaAICc)]

  Aebischer[, AICc := aic[model =='ma', round(AICc,2)]]
  Aebischer[, deltaAICc := aic[model =='ma', round(deltaAICc,2)]]
  Aebischer[, prob := aic[model =='ma', round(prob,2)]]
  Aebischer[, ER := aic[model =='ma', round(ER,2)]]

  Aebischer_S$ER = Aebischer_S$prob = Aebischer_S$deltaAICc = Aebischer_S$AICc = ""
  Aebischer_S[1, AICc := aic[model =='mas', round(AICc,2)]]
  Aebischer_S[1, deltaAICc := aic[model =='mas', round(deltaAICc,2)]]
  Aebischer_S[1, prob := aic[model =='mas', round(prob,2)]]
  Aebischer_S[1, ER := aic[model =='mas', round(ER,2)]]

  Aebischer_b$ER = Aebischer_b$prob = Aebischer_b$deltaAICc = Aebischer_b$AICc = ""
  Aebischer_Sb$ER = Aebischer_Sb$prob = Aebischer_Sb$deltaAICc = Aebischer_Sb$AICc = ""

# COMBINE and EXPORT
  out = rbind(  
              Aebischer_b, Aebischer_Sb, 
               Aebischer, Aebischer_S
          )  
  fwrite(file = "./Output/Table_A1.csv", out)

# MODAL ASSUMPTIOMS
  ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
  mag=glmer(cbind(failure,success)~1 +(1|nest),family="binomial",data=yy)
  mas=glm(cbind(failure,success)~mid_j,family="binomial",data=yy)
  m_ass_s( name = 'Table_A1a',
               title = 'glm(cbind(failure,success)~1,family="binomial",data=yy)',
               binomial = TRUE, mo = ma, dat = yy, 
               fixed = NULL, categ = NULL, 
               spatial = TRUE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")
  bsim = sim(ma, n.sim=nsim)  
  plogis(apply(bsim@coef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
  plogis(apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
  (1-(1-plogis(apply(bsim@coef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate

  bsim = sim(mag, n.sim=nsim)
  plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5)))*100 # estimate - daily
  plogis(apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975)))*100 #95%CI
  (1-(1-plogis(apply(bsim@fixef, 2, quantile, prob=c(0.5))))^30)*100  # total predation rate

  ma$deviance/ma$df.residual
  library(AER)    
  dispersiontest(ma,trafo=1)

# OLD version (removed from the MS after reviews) with various methods that give similar results
  # daily predation rate according to Mayfield
    nrow(y[fate==0])/sum(y$exposure)
    mayfield = data.table(model = "(a) Mayfield",response = "", error_structure ="", N = 440, type ="", effect = "Daily predation rate", estimate_r = paste0(round(nrow(y[fate==0])/sum(y$exposure)*100,2), "%"), lwr_r = "", upr_r ="", AICc = "", deltaAICc = "", prob = "", ER = "")

  # daily predation rate according to Aebischer - logistic regression
    yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
    yy[is.na(fate_), fate_:= 0]    # 0 all other
    yy[, year_:= as.factor(year)] 
    yy[ , success := round(exposure - fate_)]
    yy[fate_==1, failure := 1]
    yy[is.na(failure), failure := 0]

    ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
    100*(exp(ma$coefficients)/(1+exp(ma$coefficients))) #  0.01082444
    100*plogis(ma$coefficients) * 30

    Aebischer_b = m_out(name = "(b) Logistic regression",  dep = "Predated vs success days", fam = 'binomial', 
            N = nrow(yy), type = "glm",  model = ma,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = TRUE, perc_ = 100)
    
    Aebischer = m_out(name = "(c) Logistic regression",  dep = "Predated vs success days", fam = 'binomial', 
            N = nrow(yy), type = "glm",  model = ma,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE, perc_ = 1)

    mas=glm(cbind(failure,success)~mid_j,family="binomial",data=yy)

    Aebischer_S = m_out(name = "(c) Logistic regression",  dep = "Predated vs success days", fam = 'binomial', 
            N = nrow(yy), type = "glm",  model = mas,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE)

  # daily predation rate according to Shaffer - logistic exposure regression
    yy[fate == 0, fate_e:= 0]   # 1 predated works if swapped
    yy[is.na(fate_e), fate_e:= 1]    # 0 all other
      
    ms = glm(fate_e ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)  
    1-(exp(ms$coefficients)/(1+exp(ms$coefficients))) #0.01284487
    100*(1-plogis(ms$coefficients))*30

    Shaffer_b = m_out(name = "(b) Logistic exposure regression",  dep = "Predated vs success days", fam = 'binomial_logExp', 
            N = nrow(yy), type = "glm",  model = ms,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = TRUE, perc_ = 100)

    Shaffer = m_out(name = "(d) Logistic exposure regression",  dep = "Predated vs success days", fam = 'binomial_logExp', 
            N = nrow(yy), type = "glm",  model = ms,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE)

      #m_ass_s( name = 'ModelAss_Table_A1bd_logisticExp_DPR_bin',
       #          title = 'glm(fate_e ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)',
        #         binomial = TRUE, mo = ms, dat = yy, 
         #        fixed = NULL, categ = NULL, trans = c('none'), 
          #       spatial = FALSE, temporal = TRUE, 
           #      PNG = TRUE, outdir = "Output/ModelAss/")

    mss = glm(fate_e ~ mid_j, family=binomial(logexp(days=yy$exposure)),data=yy)  
    
    Shaffer_S = m_out(name = "(d) Logistic exposure regression",  dep = "Predated vs success days", fam = 'binomial_logExp', 
            N = nrow(yy), type = "glm",  model = mss,
            round_ = 2, nsim = 5000, aic = FALSE, save_sim = FALSE, back_tran = FALSE)

  # ADD AICc
    aic = data.table(model = rownames(AICc(ma,  mas)), predictors = c('none', 'season'), AICc(ma, mas))
    aic[, deltaAICc := AICc-min(AICc)]
    aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
    aic[, ER := round(max(prob)/prob, 2)]
    aic[order(deltaAICc)]

    Aebischer[, AICc := aic[model =='ma', round(AICc,2)]]
    Aebischer[, deltaAICc := aic[model =='ma', round(deltaAICc,2)]]
    Aebischer[, prob := aic[model =='ma', round(prob,2)]]
    Aebischer[, ER := aic[model =='ma', round(ER,2)]]

    Aebischer_S$ER = Aebischer_S$prob = Aebischer_S$deltaAICc = Aebischer_S$AICc = ""
    Aebischer_S[1, AICc := aic[model =='mas', round(AICc,2)]]
    Aebischer_S[1, deltaAICc := aic[model =='mas', round(deltaAICc,2)]]
    Aebischer_S[1, prob := aic[model =='mas', round(prob,2)]]
    Aebischer_S[1, ER := aic[model =='mas', round(ER,2)]]

    aic = data.table(model = rownames(AICc(ms,  mss)), predictors = c('none', 'season'), AICc(ms, mss))
    aic[, deltaAICc := AICc-min(AICc)]
    aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
    aic[, ER := round(max(prob)/prob, 2)]
    aic[order(deltaAICc)]

    Shaffer[, AICc := aic[model =='ms', round(AICc,2)]]
    Shaffer[, deltaAICc := aic[model =='ms', round(deltaAICc,2)]]
    Shaffer[, prob := aic[model =='ms', round(prob,2)]]
    Shaffer[, ER := aic[model =='ms', round(ER,2)]]

    Shaffer_S$ER = Shaffer_S$prob = Shaffer_S$deltaAICc = Shaffer_S$AICc = ""
    Shaffer_S[1, AICc := aic[model =='mss', round(AICc,2)]]
    Shaffer_S[1, deltaAICc := aic[model =='mss', round(deltaAICc,2)]]
    Shaffer_S[1, prob := aic[model =='mss', round(prob,2)]]
    Shaffer_S[1, ER := aic[model =='mss', round(ER,2)]]

  # COMBINE AND EXPORT
    Aebischer_b$ER = Aebischer_b$prob = Aebischer_b$deltaAICc = Aebischer_b$AICc = ""
    Shaffer_b$ER = Shaffer_b$prob = Shaffer_b$deltaAICc = Shaffer_b$AICc = ""

    out = rbind(  
                mayfield,
                Aebischer_b, Shaffer_b,
                Aebischer, Aebischer_S, 
                Shaffer, Shaffer_S
            )  
    fwrite(file = "./Output/Table_A1.csv", out)

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
 # [1] MuMIn_1.43.17     ggpubr_0.4.0      forcats_0.5.0     dplyr_1.0.1      
 # [5] purrr_0.3.4       readr_1.3.1       tidyr_1.1.1       tibble_3.0.3     
 # [9] tidyverse_1.3.0   gt_0.2.2          zoo_1.8-8         xlsx_0.6.3       
 #[13] stringr_1.4.0     reshape2_1.4.4    raster_3.3-13     plyr_1.8.6       
 #[17] performance_0.4.8 multcomp_1.4-13   TH.data_1.0-10    survival_3.1-12  
 #[21] mvtnorm_1.1-1     maptools_1.0-1    sp_1.4-2          magrittr_1.5     
 #[25] lubridate_1.7.9   lattice_0.20-41   htmlTable_2.0.1   gridExtra_2.3    
 #[29] glue_1.4.2        ggthemes_4.2.0    ggplot2_3.3.2     ggExtra_0.9      
 #[33] effects_4.1-4     carData_3.0-4     data.table_1.13.0 arm_1.11-2       
 #[37] lme4_1.1-23       Matrix_1.2-18     MASS_7.3-51.6     here_0.1         
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
 #[88] httpuv_1.5.4        stats4_4.0.2        munsell_0.5.0      
 #[91] mitools_2.4 