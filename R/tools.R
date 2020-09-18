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

# load/install packages
  packages = c('arm','data.table', 'effects',  'ggplot2', 'ggthemes', 'glue',  'grid','gridExtra', 'here', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'maptools', 'multcomp', 'performance','plyr','raster','stringr','xlsx','zoo', 'gt', 'tidyverse', 'ggpubr')
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
  m_out = function(name = "define", model = m, round_ = 3, nsim = 5000, aic = TRUE, save_sim = FALSE, N = NA){
  bsim <- sim(model, n.sim=nsim)  
    if(save_sim!=FALSE){save(bsim, file = paste0(save_sim, name,'.RData'))}
   v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
   ci = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975)) 
   oi=data.frame(model = name,type='fixed',effect=rownames(coef(summary(model))),estimate=v, lwr=ci[1,], upr=ci[2,])
      rownames(oi) = NULL
      oi$estimate_r=round(oi$estimate,round_)
      oi$lwr_r=round(oi$lwr,round_)
      oi$upr_r=round(oi$upr,round_)
  oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')] 
  
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
  
  x = rbind(oii,ri)
    x$N = ""
    x$N[1] = N
    if (aic == TRUE){   
        x$AIC = ""
        x$AIC[1]=AIC(update(model,REML = FALSE))
        x$delta = ""
        x$prob = ""
        x$ER = ""
        }
     x$R2_mar = ""
     x$R2_con = ""
     x$R2_mar [1]= invisible({capture.output({r2_nakagawa(model)$R2_marginal})})
     x$R2_con [1]= invisible({capture.output({r2_nakagawa(model)$R2_conditional})})
     #x$R2_mar [1]= r2_nakagawa(model)$R2_marginal
     #x$R2_con [1]= r2_nakagawa(model)$R2_conditional
    return(x)
  } 
# model assumption functions
  # mixed models
  m_ass = function(name = 'define', mo = m0, dat = d, fixed = NULL, categ = NULL, trans = NULL, spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir'){
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
   
   if(PNG == TRUE){
    png(paste(outdir,name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = length(fixed)-1+length(categ) + 4 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   if(binomial == TRUE){
      plot(fitted(mo), jitter(mo$model[,1], amount=0.05), xlab="Fitted values", ylab="Original values", las=1, cex.lab=1, cex=0.8,  main=list(paste("Probability of", names(mo$model)[1]),cex=0.8) )
      abline(0,1, lty=3)
      t.breaks <- cut(fitted(m), quantile(fitted(m)))
      means <- tapply(mo$model[,1], t.breaks, mean)
      semean <- function(x) sd(x)/sqrt(length(x))
      means.se <- tapply(mo$model[,1], t.breaks, semean)
      points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
      segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
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
   
   mtext(title, side = 3, line = -1, cex=0.7,outer = TRUE)
  if(PNG==TRUE){dev.off()}
  }  

