# TOOLS & DATA
  require(here)
  source(here::here('R/tools.R'))
  
  reml = TRUE # models fitted with REML (TRUE) or ML (FALSE)
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))
  yy = y[exposure>0]

# daily predation rate according to Mayfield
  nrow(y[fate==0])/sum(y$exposure)

# daily predation rate according to Aebischer - logistic regression
  yy[fate == 0, fate_:= 1]   # 1 predated works if swapped
  yy[is.na(fate_), fate_:= 0]    # 0 all other
  #y[, year_:= as.factor(year)] 
  yy[ , success := round(exposure - fate_)]
  yy[fate_==1, failure := 1]
  yy[is.na(failure), failure := 0]

  ma=glm(cbind(failure,success)~1,family="binomial",data=yy)
  100*(exp(ma$coefficients)/(1+exp(ma$coefficients))) #  0.01082444
  100*plogis(ma$coefficients) * 30

  Aebischer = m_out(name = "(a) Logistic regression",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = ma,
          round_ = 4, nsim = 5000, aic = FALSE, save_sim = FALSE)


  mas=glm(cbind(failure,success)~mid_j,family="binomial",data=yy)

  Aebischer_S = m_out(name = "(c) Logistic regression",  dep = "Predated vs success days", fam = 'binomial', 
          N = nrow(yy), type = "glm",  model = mas,
          round_ = 4, nsim = 5000, aic = FALSE, save_sim = FALSE)

# daily predation rate according to Shaffer - logistic exposure regression
  yy[fate == 0, fate_e:= 0]   # 1 predated works if swapped
  yy[is.na(fate_e), fate_e:= 1]    # 0 all other
    
  ms = glm(fate_e ~ 1, family=binomial(logexp(days=yy$exposure)),data=yy)  
  1-(exp(ms$coefficients)/(1+exp(ms$coefficients))) #0.01284487
  100*(1-plogis(ms$coefficients))*30

  Shaffer = m_out(name = "(a) Logistic exposure regression",  dep = "Predated vs success days", fam = 'binomial_logExp', 
          N = nrow(yy), type = "glm",  model = ms,
          round_ = 4, nsim = 5000, aic = FALSE, save_sim = FALSE)

  mss = glm(fate_e ~ mid_j, family=binomial(logexp(days=yy$exposure)),data=yy)  
  
  Shaffer_S = m_out(name = "(d) Logistic exposure regression",  dep = "Predated vs success days", fam = 'binomial_logExp', 
          N = nrow(yy), type = "glm",  model = mss,
          round_ = 4, nsim = 5000, aic = FALSE, save_sim = FALSE)

# COMBINE AND EXPORT
  o = rbind(  Aebischer, Shaffer,
            Aebischer_S, Shaffer_S
          )  
  fwrite(file = "./Output/Table_Rate.csv", o)

# AIC
  require(MuMIn)
  aic = data.table(model = rownames(AICc(ma,  mas)), predictors = c('none', 'season'), AICc(ma, mas))
  aic[, deltaAICc := AICc-min(AICc)]
  aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
  aic[, ER := round(max(prob)/prob, 2)]
  aic[order(deltaAICc)]


# sessionInfo()

# COMBINE AND EXPORT
  o = rbind(  Aebischer, Shaffer,
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

# AIC
  require(MuMIn)
  aic = data.table(model = rownames(AICc(ma,  mas), predictors = c('none', 'season'), AICc(mb, mbp, tb, tbp, dtb, mbrd, mbrt))
 aic[, deltaAICc := AICc-min(AICc)]
 aic[, prob := round(exp(-0.5*deltaAICc)/sum(exp(-0.5*deltaAICc)),2)]
 aic[, ER := round(max(prob)/prob, 2)]
 aic[order(deltaAICc)]

# sessionInfo()