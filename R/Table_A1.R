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

  