# ==========================================================================
# Supporting information for "Sladecek et al. (2020) Diel nest predation 
# pattern changes across season in a subtropical shorebird
# Contributor: Martin Bulla
# 📍 This script runs relative to the project's root directory, and loads
# functions and packages used in the analyses
# ==========================================================================

#' Loads packages and installs those that are not in the library
#' @param  vector of package names
#' @export

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))#, quietly  = TRUE
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}

#' Extract time as numeric from POSIXct
#' @param  x (POSIXct)
#' @export
getime = function (x) {ifelse(is.na(x), as.numeric(NA), as.numeric(difftime(x, trunc(x,"day"), units = "hours")))}

#' Extract DATE from POSIXct
#' @param  x (POSIXct)
#' @export
getDay = function (x) {as.Date(trunc(x, "day"))}

# load/install packages
  packages = c('arm','data.table', 'effects',  'ggExtra', 'ggplot2', 'ggthemes', 'glue',  'grid','gridExtra', 'here', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'maptools', 'multcomp', 'performance','plyr','raster','reshape2','stringr','xlsx','zoo', 'gt', 'tidyverse', 'ggpubr')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )

# Set system time
   Sys.setenv(TZ="UTC")

# Customized ggplot theme
    theme_MB = theme(  
              axis.line = element_blank(),
              #axis.line = element_line(colour="grey70", size=0.25),
              axis.title = element_text(size=7, colour="grey30"),
              axis.title.y = element_text(vjust=3.5),
              axis.title.x = element_text(vjust=1),
              axis.text = element_text(size=6),#, vjust = 0.5, hjust=1),# margin=units(0.5,"mm")),
              axis.ticks.length=unit(0.5,"mm"),
              axis.ticks = element_line(colour = "grey70", size = 0.1),
              #axis.ticks.margin,
              
              strip.text.x = element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
              strip.background = element_rect(fill="grey99",colour="grey70", size=0.25),
                #strip.background = element_blank(), 
                #strip.text = element_blank(),
              panel.spacing = unit(0, "mm"),
              panel.background=element_blank(),
              panel.border = element_rect(colour="grey70", size=0.1, fill = NA), #panel.border=element_blank(),
              panel.grid = element_blank(),

              legend.text=element_text(size=6),
              legend.title=element_text(size=6),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.height= unit(0.5,"line"),
              legend.key.width = unit(0.25, "cm"),
              legend.margin = margin(0,0,0,0, unit="cm"),
              legend.box.margin = margin(l = -6), #legend.justification = c(-1,0),
              legend.background = element_blank()
              )


# model output function
  m_out = function(model = m, type = "mixed", 
    name = "define", dep = "define", fam = 'Gaussian',
    round_ = 3, nsim = 5000, aic = TRUE, save_sim = FALSE, N = NA, back_tran = FALSE, perc_ = 1){
      # perc_ 1 = proportion or 100%
    bsim = sim(model, n.sim=nsim)  
    
    if(save_sim!=FALSE){save(bsim, file = paste0(save_sim, name,'.RData'))}
   
    if(type != "mixed"){
     v = apply(bsim@coef, 2, quantile, prob=c(0.5))
     ci = apply(bsim@coef, 2, quantile, prob=c(0.025,0.975)) 

     if(back_tran == TRUE & fam == "binomial"){
      v = perc_*plogis(v)
      ci = perc_*plogis(ci)
     }
    if(back_tran == TRUE & fam == "binomial_logExp"){
          v = perc_*(1-plogis(v))
          ci = perc_*(1-plogis(ci))
          ci = rbind(ci[2,],ci[1,])
         }

     if(back_tran == TRUE & fam == "Poisson"){
      v = exp(v)
      ci = exp(ci)
     }

     oi=data.frame(type='fixed',effect=rownames(coef(summary(model))),estimate=v, lwr=ci[1,], upr=ci[2,])
      rownames(oi) = NULL
      oi$estimate_r=round(oi$estimate,round_)
      oi$lwr_r=round(oi$lwr,round_)
      oi$upr_r=round(oi$upr,round_)
      if(perc_ == 100){
       oi$estimate_r = paste0(oi$estimate_r,"%")
       oi$lwr_r = paste0(oi$lwr_r,"%")
       oi$upr_r = paste0(oi$upr_r,"%")
      }
     x=data.table(oi[c('type',"effect", "estimate_r","lwr_r",'upr_r')]) 
   
    }else{
     v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
     ci = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975)) 

     if(back_tran == TRUE & fam == "binomial"){
      v = perc_*plogis(v)
      ci = perc_*plogis(ci)
     }
    if(back_tran == TRUE & fam == "binomial_logExp"){
          v = perc_*(1-plogis(v))
          ci = perc_*(1-plogis(ci))
          ci = rbind(ci[2,],ci[1,])
         }

     if(back_tran == TRUE & fam == "Poisson"){
      v = exp(v)
      ci = exp(ci)
     }

     oi=data.frame(type='fixed',effect=rownames(coef(summary(model))),estimate=v, lwr=ci[1,], upr=ci[2,])
        rownames(oi) = NULL
        oi$estimate_r=round(oi$estimate,round_)
        oi$lwr_r=round(oi$lwr,round_)
        oi$upr_r=round(oi$upr,round_)
        if(perc_ == 100){
         oi$estimate_r = paste0(oi$estimate_r,"%")
         oi$lwr_r = paste0(oi$lwr_r,"%")
         oi$upr_r = paste0(oi$upr_r,"%")
        }
     oii=oi[c('type',"effect", "estimate_r","lwr_r",'upr_r')] 
    
     l=data.frame(summary(model)$varcor)
     l = l[is.na(l$var2),]
     l$var1 = ifelse(is.na(l$var1),"",l$var1)
     l$pred = paste(l$grp,l$var1)

     q050={}
     q025={}
     q975={}
     pred={}
     
     # variance of random effects
     for (ran in names(bsim@ranef)) {
       ran_type = l$var1[l$grp == ran]
       for(i in ran_type){
        q050=c(q050,quantile(apply(bsim@ranef[[ran]][,,ran_type], 1, var), prob=c(0.5)))
        q025=c(q025,quantile(apply(bsim@ranef[[ran]][,,ran_type], 1, var), prob=c(0.025)))
        q975=c(q975,quantile(apply(bsim@ranef[[ran]][,,ran_type], 1, var), prob=c(0.975)))
        pred= c(pred,paste(ran, i))
        }
       }
     # residual variance
     q050=c(q050,quantile(bsim@sigma^2, prob=c(0.5)))
     q025=c(q025,quantile(bsim@sigma^2, prob=c(0.025)))
     q975=c(q975,quantile(bsim@sigma^2, prob=c(0.975)))
     pred= c(pred,'Residual')

     ri=data.frame(model = name,type='random %',effect=pred, estimate_r=round(100*q050/sum(q050)), lwr_r=round(100*q025/sum(q025)), upr_r=round(100*q975/sum(q975)))
       rx = ri[ri$effect == 'Residual',]
       if(rx$lwr_r>rx$upr_r){ri$lwr_r[ri$effect == 'Residual'] = rx$upr_r; ri$upr_r[ri$effect == 'Residual'] = rx$lwr_r}
       ri$estimate_r = paste0(ri$estimate_r,'%')
       ri$lwr_r = paste0(ri$lwr_r,'%')
       ri$upr_r = paste0(ri$upr_r,'%')
    
    x = data.table(rbind(oii,ri))
    }
    
    x[1, model := name]                                                                
    x[1, response := dep]                                                                
    x[1, error_structure := fam]                                                                
    x[1, N := N]                                                                

    x=x[ , c('model', 'response', 'error_structure', 'N', 'type',"effect", "estimate_r","lwr_r",'upr_r')] 

    if (aic == TRUE){   
        x[1, AIC := AIC(update(model,REML = FALSE))] 
        }
    if (aic == "AICc"){
        aicc = AICc(model)
        x[1, AICc := aicc] 
    }
    if(type == "mixed"){
      x[1, R2_mar := invisible({capture.output({r2_nakagawa(model)$R2_marginal})})]
      x[1, R2_con := invisible({capture.output({r2_nakagawa(model)$R2_conditional})})]
     }
    x[is.na(x)] = ""
    return(x)
  } 

  #summary(m)$r.squared
  #summary(m)$adj.r.squared
# model assumption functions
  # mixed models
  m_ass = function(name = 'define', mo = m0, dat = d, fixed = NULL, categ = NULL, trans = "none", spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir'){
   l=data.frame(summary(mo)$varcor)
   l = l[is.na(l$var2),]
   if(PNG == TRUE){
    png(paste(outdir,name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = nrow(l)-1+length(fixed)+length(categ) + 4 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
   #unique(l$grp[l$grp!="Residual"])
   for(i in unique(l$grp[l$grp!="Residual"])){
    #i = "mean_year"
    ll=ranef(mo)[names(ranef(mo))==i][[1]]
    if(ncol(ll)==1){
     qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
     }else{
      qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
      qqnorm(ll[,2], main = paste(i,names(ll)[2]),col='grey');qqline(ll[,2], col ='red')
     }
    }
    
   # variables
   scatter={} 
   for (i in rownames(summary(mo)$coef)) {
        #i = "lat_abs"
      j=sub("\\).*", "", sub(".*\\(", "",i)) 
      scatter[length(scatter)+1]=j
    }
    x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                    log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
    for (i in 1:length(fixed)){
        jj =fixed[i]
        variable=dat[, ..jj][[1]]
        if(trans[i]=='log'){
        scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }else if(trans[i]=='abs'){
        scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }else{
        scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
      }
     }
    
    if(length(categ)>0){
      for(i in categ){
         variable=dat[, ..i][[1]]
          boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
         }
    }     
          
    if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
        }
    if(spatial == TRUE){    
    spdata=data.frame(resid=resid(mo), x=dat$Longitude, y=dat$Latitude)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
      plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
        legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
      plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
      plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
        }
   
   mtext(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''), side = 3, line = -1, cex=0.7,outer = TRUE)
  if(PNG==TRUE){dev.off()}
  }
  
  # simple models
  m_ass_s = function(name = 'define', title = 'define', binomial = FALSE, mo = m0, dat = d, fixed = NULL, categ = NULL, trans = NULL, spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir'){
    # binomial - shall a plot visualizing response means per sequence of fitted data be visualized?
    # trans - vector containing transformation function used to transform each predictor
   if(PNG == TRUE){
    png(paste(outdir,name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = length(fixed)+length(categ) + 4 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   if(binomial == TRUE){
      plot(fitted(mo), jitter(mo$model[,1], amount=0.05), xlab="Fitted values", ylab="Original values", las=1, cex.lab=1, cex=0.8,  main=list(paste("Probability of", names(mo$model)[1]),cex=0.8) )
      abline(0,1, lty=3)
      t.breaks <- cut(fitted(mo), quantile(fitted(mo)))
      means <- tapply(mo$model[,1], t.breaks, mean)
      semean <- function(x) sd(x)/sqrt(length(x))
      means.se <- tapply(mo$model[,1], t.breaks, semean)
      points(quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
      segments(quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(mo),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
   }

   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
  
   # variables
     scatter={} 
     for (i in rownames(summary(mo)$coef)) {
          #i = "lat_abs"
        j=sub("\\).*", "", sub(".*\\(", "",i)) 
        scatter[length(scatter)+1]=j
      }
      x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                      log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
      for (i in 1:length(fixed)){
          jj =fixed[i]
          variable=dat[, ..jj][[1]]
          if(trans[i]=='log'){
          scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='abs'){
          scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='sin'){
            scatter.smooth(resid(mo)~sin(variable),xlab=paste('sin(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          }else if(trans[i]=='cos'){
            scatter.smooth(resid(mo)~cos(variable),xlab=paste('cos(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
          } else {  
          scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }
       }
      
      if(length(categ)>0){
        for(i in categ){
           variable=dat[, ..i][[1]]
            boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
           }
      }     
          
   if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
        }
   if(spatial == TRUE){    
      spdata=data.frame(resid=resid(mo), x=dat$Longitude, y=dat$Latitude)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
      plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
        legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
      plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
      plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
        }
   
   mtext(title, side = 3, line = -1, cex=0.7,outer = TRUE)
   
   if(PNG==TRUE){dev.off()}
  }  

# Logistic exposure link function
  logexp <- function(days = 1)
  {
      linkfun <- function(mu) qlogis(mu^(1/days))
      linkinv <- function(eta) plogis(eta)^days
      mu.eta <- function(eta) days * plogis(eta)^(days-1) *
        .Call(stats:::C_logit_mu_eta, eta, PACKAGE = "stats")
      valideta <- function(eta) TRUE
      link <- paste("logexp(", days, ")", sep="")
      structure(list(linkfun = linkfun, linkinv = linkinv,
                     mu.eta = mu.eta, valideta = valideta, name = link),
                class = "link-glm")
  }

# intersection of logger data on the nest
  list.model.predictors=function(predictors,max_pred){
    x=rep(predictors,times=length(predictors)^(max_pred-1))
    xx=predictors
    for (pred in 2:max_pred) {
      xx=rep(predictors,each=length(xx))
      x=cbind(x,xx) 
    }
    unique_perm=unique(apply(t(apply(x,1,sort)), 1, paste,collapse=":"))# odstraneni replikativnich permutaci# odstraneni replikaci v ramci permutace
    unique_pred=unique(sapply(strsplit(unique_perm,split=":"),unique))# odstraneni replikaci v ramci permutace
    return(unique_pred)  
  }

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
 #[33] carData_3.0-4     arm_1.11-2        lme4_1.1-23       Matrix_1.2-18    
 #[37] MASS_7.3-51.6     here_0.1          data.table_1.13.0
 #
 #loaded via a namespace (and not attached):
 # [1] minqa_1.2.4         colorspace_1.4-1    ggsignif_0.6.0     
 # [4] rio_0.5.16          ellipsis_0.3.1      rprojroot_1.3-2    
 # [7] fs_1.5.0            base64enc_0.1-3     rstudioapi_0.11    
 #[10] fansi_0.4.1         xml2_1.3.2          codetools_0.2-16   
 #[13] splines_4.0.2       knitr_1.29          Formula_1.2-3      
 #[16] jsonlite_1.7.0      nloptr_1.2.2.2      rJava_0.9-13       
 #[19] broom_0.7.0         cluster_2.1.0       dbplyr_1.4.4       
 #[22] png_0.1-7           shiny_1.5.0         compiler_4.0.2     
 #[25] httr_1.4.2          backports_1.1.8     assertthat_0.2.1   
 #[28] fastmap_1.0.1       cli_2.0.2           survey_4.0         
 #[31] later_1.1.0.1       acepack_1.4.1       htmltools_0.5.0    
 #[34] tools_4.0.2         coda_0.19-3         gtable_0.3.0       
 #[37] Rcpp_1.0.5          cellranger_1.1.0    vctrs_0.3.2        
 #[40] nlme_3.1-148        insight_0.9.0       xfun_0.16          
 #[43] xlsxjars_0.6.1      openxlsx_4.1.5      rvest_0.3.6        
 #[46] mime_0.9            miniUI_0.1.1.1      lifecycle_0.2.0    
 #[49] rstatix_0.6.0       statmod_1.4.34      scales_1.1.1       
 #[52] hms_0.5.3           promises_1.1.1      sandwich_2.5-1     
 #[55] RColorBrewer_1.1-2  curl_4.3            rpart_4.1-15       
 #[58] latticeExtra_0.6-29 stringi_1.5.3       bayestestR_0.7.2   
 #[61] checkmate_2.0.0     zip_2.0.4           boot_1.3-25        
 #[64] rlang_0.4.7         pkgconfig_2.0.3     htmlwidgets_1.5.1  
 #[67] tidyselect_1.1.0    R6_2.4.1            generics_0.0.2     
 #[70] Hmisc_4.4-0         DBI_1.1.0           pillar_1.4.6       
 #[73] haven_2.3.1         foreign_0.8-80      withr_2.2.0        
 #[76] abind_1.4-5         nnet_7.3-14         car_3.0-8          
 #[79] modelr_0.1.8        crayon_1.3.4        jpeg_0.1-8.1       
 #[82] readxl_1.3.1        blob_1.2.1          reprex_0.3.0       
 #[85] digest_0.6.25       xtable_1.8-4        httpuv_1.5.4       
 #[88] munsell_0.5.0       mitools_2.4   
