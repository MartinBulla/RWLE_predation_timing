# TOOLS & DATA
  require(here)
  source(here::here('R/tools.R'))
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))

# GENERATE MODEL OUTPUTS
# (a) 24h circadian pattern
    tx = data.table(time_corr_round = seq(0,23,by =1))
    tdx = merge(td,tx, all = TRUE)
    tdx[is.na(cases), cases := 0]
    tdx[,rad :=(2*pi*time_corr_round)/24]

    m = glm(cases~sin(rad)+cos(rad), family = 'poisson', tdx)
    a_24h_pois = m_out(name = "(a) Time of day",  dep = "Predation per hour", fam = 'Poisson', 
            N = nrow(tdx), type = "glm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_a_24h_pois',
               title = 'glm(cases~sin(rad)+cos(rad), family = "poisson", tdx)',
               binomial = TRUE, mo = m, dat = tdx, 
               fixed = c('rad', 'rad'), categ = NULL, trans = c('cos', 'sin'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

    m = lm(cases~sin(rad)+cos(rad), tdx)
    a_24h_gaus = m_out(name = "(a) Time of day",  dep = "Predation per hour", fam = 'Gaussian', 
            N = nrow(tdx), type = "lm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_a_24h_gaus',
               title = 'lm(cases~sin(rad)+cos(rad), tdx)',
               binomial = TRUE, mo = m, dat = tdx, 
               fixed = c('rad', 'rad'), categ = NULL, trans = c('cos', 'sin'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

    #summary(m)
    #plot(allEffects(m))  
# (b) 24h sunrise pattern    
    tx = data.table(time_from_sunrise_r = seq(-11,11,by =1))
    tsx = merge(ts,tx, all = TRUE)
    tsx[is.na(cases), cases := 0]
    tsx[,rad :=(2*pi*time_from_sunrise_r)/24]

   
    m = glm(cases~abs(time_from_sunrise_r), family = 'poisson', tsx)
    b_sun_pois = m_out(name = "(b) Time relative to sunrise",  dep = "Predation per hour", fam = 'Poisson', 
            N = nrow(tsx), type = "glm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_b_Sun_pois',
               title = 'glm(cases~abs(time_from_sunrise_r), family = "poisson", tsx)',
               binomial = TRUE, mo = m, dat = tsx, 
               fixed = c('time_from_sunrise_r'), categ = NULL, trans = c('abs'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

    m = lm(cases~abs(time_from_sunrise_r), tsx)
    b_sun_gaus = m_out(name = "(b) Time relative to sunrise", dep = "Predation per hour", fam = 'Gaussian',
            N = nrow(tsx), type = "lm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_b_Sun_gaus',
               title = 'lm(cases~abs(time_from_sunrise_r), tsx)',
               binomial = TRUE, mo = m, dat = tsx, 
               fixed = c('time_from_sunrise_r'), categ = NULL, trans = c('abs'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

    #summary(m)
    #plot(allEffects(m))
# (c) night predation over season
  mb=glm(night_num ~ date_num, family="binomial", data=xx)
  c_night_bin = m_out(name = "(c) Season",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(xx), type = "glm",  model = mb,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_c_night-season_bin',
               title = 'glm(night_num ~ date_num, family = "binomial", xx)',
               binomial = TRUE, mo = mb, dat = xx, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  mg=lm(night_num ~ date_num, data=xx)
  c_night_gaus = m_out(name = "(c) Season",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = mg,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_c_night-season_gaus',
               title = 'lm(night_num ~ date_num, xx)',
               binomial = TRUE, mo = mg, dat = xx, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")
# (d) night predation given midday temperature
  tb=glm(night_num ~ midday_T, family="binomial", data=xx)
  d_night_bin = m_out(name = "(d) Midday T",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(xx), type = "glm",  model = tb,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_d_night-middayT_bin',
               title = 'glm(night_num ~ midday_T, family = "binomial", xx)',
               binomial = TRUE, mo = tb, dat = xx, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  tg=lm(night_num ~ midday_T, data=xx)
  d_night_gaus = m_out(name = "(d) Midday T",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = tg,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_d_night-middayT_gaus',
               title = 'lm(night_num ~ midday_T, xx)',
               binomial = TRUE, mo = tg, dat = xx, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")    
# (e) night predation given midday temperature and day in season
  dtb=glm(night_num ~ midday_T + date_num, family="binomial", data=xx)
  e_night_bin = m_out(name = "(e) Midday T + date",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(xx), type = "glm",  model = dtb,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_d_night-middayT-date_bin',
               title = 'glm(night_num ~ midday_T + date_num, family = "binomial", xx)',
               binomial = TRUE, mo = dtb, dat = xx, 
               fixed = c('midday_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  dtg=lm(night_num ~ midday_T + date_num, data=xx)
  e_night_gaus = m_out(name = "(e) Midday T + date",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = dtg,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_e_night-middayT-date_gaus',
               title = 'lm(night_num ~ midday_T + date_num, xx)',
               binomial = TRUE, mo = dtg, dat = xx, 
               fixed = c('midday_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")        
# (f) midday T ~ date
    m = lm(midday_T~ date_num,data=xx)
    f_midTdate = m_out(name = "(f) Residual T",  dep = "Midday T", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_f_midTdate_gaus',
               title = 'lm(midday_T~ date_num,data=xx)',
               binomial = TRUE, mo = m, dat = xx, 
               fixed = c('date_num'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")
# (g) night ~ reside temperature
  mx = lm(midday_T~ date_num,data=xx)
  xx$res_T = resid(mx)
  mbrd=glm(night_num ~ res_T + date_num, data = xx, family="binomial")

  g_night_bin = m_out(name = "(g) res T + date",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(xx), type = "glm",  model = mbrd,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_g_night-resT-date_bin',
               title = 'glm(night_num ~ res_T + date_num, data = xx, family="binomial")',
               binomial = TRUE, mo = mbrd, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  mgrd=lm(night_num ~ res_T + date_num, data = xx)
  g_night_gaus = m_out(name = "(g) Midday T + date",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = mgrd,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_g_night-resT-date_gaus',
               title = 'lm(night_num ~ res_T + date_num, data = xx)',
               binomial = TRUE, mo = mgrd, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")        
# (h) date ~ middayT
    m = lm(date_num ~ midday_T,data=xx)
    h_dateMidT = m_out(name = "(h) Residual date",  dep = "Date", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = m,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
    m_ass_s( name = 'ModelAss_h_date_gaus',
               title = 'lm(date_num~ midday_T,data=xx)',
               binomial = TRUE, mo = m, dat = xx, 
               fixed = c('midday_T'), categ = NULL, trans = c('none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")
# (i) night ~ reside temperature
  my = lm(date_num ~ midday_T,data=xx)
  xx$res_D = resid(my)
  mbrt=glm(night_num ~ res_D + midday_T, data = xx, family="binomial")
  i_night_bin = m_out(name = "(i) res date + T",  dep = "Night predation (0, 1)", fam = 'binomial', 
            N = nrow(xx), type = "glm",  model = mbrt,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_i_night-resDate-T_bin',
               title = 'glm(night_num ~ res_D + midday_T, data = xx, family="binomial")',
               binomial = TRUE, mo = mbrt, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")

  mgrt=glm(night_num ~ res_D + midday_T, data = xx)
  i_night_gaus = m_out(name = "(i) res date + T",  dep = "Night predation (0, 1)", fam = 'Gaussian', 
            N = nrow(xx), type = "lm",  model = mgrt,
            round_ = 3, nsim = 5000, aic = FALSE, save_sim = FALSE)
  m_ass_s( name = 'ModelAss_i_night-resDate-T_gaus',
               title = 'glm(night_num ~ res_D + midday_T, data = xx)',
               binomial = TRUE, mo = mgrt, dat = xx, 
               fixed = c('res_T', 'date_num'), categ = NULL, trans = c('none','none'), 
               spatial = FALSE, temporal = TRUE, 
               PNG = TRUE, outdir = "Output/ModelAss/")       

# COMBINE AND EXPORT
  o = rbind(  a_24h_pois, a_24h_gaus,
            b_sun_pois, b_sun_gaus,
            c_night_bin, c_night_gaus,
            d_night_bin, d_night_gaus,
            e_night_bin, e_night_gaus,
            f_midTdate, 
            g_night_bin, g_night_gaus,
            h_dateMidT,
            i_night_bin, i_night_gaus
          )  
  fwrite(file = "./Output/Table.csv", o)