
library(MASS)
library(ggplot2)
library(ResourceSelection) #for the 'hoslem.test' function
library(arm)               #for the 'standardize' function
library(MuMIn)             #for the 'dredge' function
library(AICcmodavg)        #for the 'aictab' function
library(here)       

# Additional function called 'logexp' 
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

# Additional function called 'vif.mer' 
vif.mer <- function (fit) {
    ## adapted from rms::vif

    v <- vcov(fit)
    nam <- names(coefficients(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}


# Read-in input data
#nest.data <- read.csv(file=here::here("Data/log expos CAHA input_sample.csv"), header = TRUE, sep = ",")
nest.data <- read.csv(file="log expos CAHA input_sample.csv", header = TRUE, sep = ",")

# Variables in the dataset:
# 1. nestID
# 2. day1 : first day of the interval
# 3. end.int : last day of the interval
# 4. expos : number of days in the interval
# 5. middate: value of day at the interval midpoint
# 6. al: age at the interval midpoint (i.e., linear age term)
# 7. xstage: stage of nesting at interval midpoint (lay, incub, or brood)
# 8. survive: 1 if nest survived the interval, 0 if it did not
# 9. trials: all as 1

### And some covariates:
# 10. year: linear effect, continuous variable
# 11. district: three subsites at CAHA (HI, OI, or BI)
# 12. exclosure: 1 if the nest was exclosed at the beginning of the interval
# 13. crab: 1 if a ghost crab was detected at the beginning of the interval
# 14. inidate.centered: date of clutch initiation for the given nest centered to the annual mean date of clutch initiation
# 15. tmax: daily maximum temperature at the beginning of the interval
# 16. prcp: daily total precipitation at the beginning of the interval
# 17. wsf2: daily fastest 2-min wind speed at the beginning of the interval
# 18. year.f: year as a categorical variable
# 19. clutch : clutch size

### Somethings to remember when creating the input data:
### 'middate' can have .5 values
### 'al' must be equal or greater than 0
### 'expos' should not be NA or less than 0



# Set categorical variables as factors
nest.data$year.f <- as.factor(as.character(nest.data$year.f))
nest.data$exclosure <- as.factor(as.character(nest.data$exclosure))
nest.data$crab <- as.factor(as.character(nest.data$crab))

nest.data$survive/nest.data$trials
# Global model without a random effect
glm.logexp<-glm(survive/trials~xstage + al + middate + crab + inidate.centered + exclosure + 
							   year.f + district + tmax + prcp + wsf2 + clutch +
							   #and some interaction terms:
							   al:xstage + crab:tmax + crab:exclosure + crab:clutch + crab:inidate.centered,
							   family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
dat = y
summary(glm.logexp)	
dat$fate_=ifelse(dat$fate==0,0,1)
dd=dat[!is.na(dat$mid_age),]
m=glm(fate_/1~mid_j, # + mid_age,
      family=binomial(logexp(days=as.numeric(dd$expo_days)+dd$fate_mayf)),data=dd)
m=glm(fate_/1~scale(mid_j) + scale(mid_age),
      family=binomial(logexp(days=as.numeric(dd$expo_days)+dd$fate_mayf)),data=dd)
m=glm(fate_/1~1 ,
      family=binomial(logexp(days=as.numeric(dd$expo_days)+dd$fate_mayf)),data=dd)

x1=predict(m)
summary(m)

1-expit(5.20448)
1-expit(4.8127)

1-expit(5.2188 )
1-expit(4.8515 )
m2=glm(cbind(expo_days,fate_mayf)~mid_j + mid_age,
      family="binomial",data=dd)


m2=glm(cbind(expo_days,fate_mayf)~scale(mid_j) + scale(mid_age),
      family="binomial",data=dd)
m2=glm(cbind(expo_days,fate_mayf)~1,
      family="binomial",data=dd)
x2=predict(m2)
summary(m2)
plot(x2~x1)
lines(x=c(0,12),y=c(0,12))

plogis(x2)  # jako expit

plogis(x1)^as.numeric(dd$expo_days)

plogis(x1)-plogis(x2)

plot(sort(plogis(x1))~sort(dd$mid_age),type="l")
lines(sort(plogis(x2))~sort(dd$mid_age))


hist(as.numeric((dd$end_expo-dd$first_egg)/24)[dd$fate==0],breaks=20)
hist(dd$mid_age)

tab=t(xtabs(~dd$mid_age + dd$fate)/rowSums(xtabs(~dd$mid_age + dd$fate)))
barplot(tab)


ddd=dd[dd$expo_days<45,]
m=glm(fate_/1~mid_j + mid_age,
      family=binomial(logexp(days=as.numeric(ddd$expo_days)+ddd$fate_mayf)),data=ddd)

summary(m)
sample()

exp(0.19)

# Test the goodness of fit of the global model
hoslem.test(glm.logexp$y, glm.logexp$fitted)	
# You want the p-value to be greater than 0.05

# Let's check for potential collinearity among explanatory variables
library(GGally)
ggpairs(nest.data[,c(5,6,15,16)])
					
### Create a set of all possible submodels from our global model,
### and compare models using AIC
								
### We don't want collinear variables to appear in the same model.
### You can create a small matrix to assign which combinations of variables
### you want to exclude from your models.
### To use a matrix, find how to use 'subset' in 'dredge' function of R package MuMIn.
### Or, you could specify the combinations in your model as below.

options(na.action = "na.fail")

vif.mer(glm.logexp)

# Standardize variables in the model by
# subtracting the mean and divided by 2 Standard deviation
stdz.model <- standardize(glm.logexp, standardize.y = FALSE)

# Create a model set with all possible submodels,
# excluding the three combinations of variables that are collinear.
# This could take few minutes depending on how many submodels there can be.
model.set <- dredge(stdz.model, subset = expression(!( (z.middate && z.inidate.centered) ||  (z.middate && z.tmax) || (z.tmax && z.inidate.centered))))   
 
# Subset compatitive models with delta AICc less than 2
top.models <- get.models(model.set, subset= delta<2)  

# For a AIC table
aictab(top.models)


# from Arnold (2010):
# "For n/K > 40, AIC-based model selection will support additional variables whose
# approximately 85% confidence intervals exclude zero (i.e., if likelihood-ratio x^2 > 2
# on 1 degree of freedom, then P < 0.157). It makes little sense to select variables at
# P < 0.157 using AIC and then turn around and dismiss them at P > 0.05 using 95% confidence
# intervals." 

# So, here, I re-ran the top 8 models outside of 'dredge' function
# to check for uninformative variable

glm.1<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + crab:clutch,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.2<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.3<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + crab:clutch + crab:tmax,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.4<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + crab:tmax,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.5<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + prcp + crab:clutch,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.6<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + exclosure + crab:clutch,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.7<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + crab:clutch + al:xstage,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	
glm.8<-glm(survive/trials~xstage + al + crab + year.f + district + tmax + wsf2 + clutch + prcp,
						  family=binomial(logexp(days=nest.data$expos)),data=nest.data)	

cand.mods <- list(glm.1,glm.2,glm.3,glm.4,glm.5,glm.6,glm.7,glm.8)

mod.names <- c("glm.1","glm.2","glm.3","glm.4","glm.5","glm.6","glm.7","glm.8")

# AIC table again
aictab(cand.set=cand.mods, modnames=mod.names, second.ord = FALSE)
						  
# For each model, check the 85% confidence interval for covariates						  
confint.default(glm.1, level=0.85)





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
###    Predicting DSR over a range of covariate values     ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###


### To do this, we need to create a new dataset where all
### covariates are set to the mean value except for the 
### covariate of interest that we want to predict the DSR over. 

### Say, I want to predict DSR  
### at nest age of 19 (i.e., the middle of incubation)
### over a range of observed daily maximum temperature (13 - 34 'C)
### for two groups of nests, with and without ghost crabs detected (0 or 1) 

pred5 <- nest.data

# For continuous variables in the dataset, 
# this for loop sets the values to the column mean
for (i in 1:ncol(pred5)){
	
	if(is.numeric(pred5[,i])==TRUE){
	pred5[i] <- colMeans(pred5[i])
	} 
}	  

# I only need 44 rows in this new dataset 
# 22 rows to assign temperature 13 to 34'C
# for each of two groups (crab 0 or 1)
pred5 <- pred5[1:44,]

# set nest age at 19
pred5$al <- 19

# set crab to 0 or 1
pred5$crab <- as.factor(as.character(c(rep(0,22),rep(1,22))))

# set reproductive stage as 'incubation'
pred5$xstage <- "incub"	  

# set exclosure to be '1' which codes 'yes'
pred5$exclosure <- as.factor(as.character('1'))

# fill in the range of temperature
pred5$tmax <- c(rep(seq(13,34,by=1),2))

# Then, estimate the DSR again using this new dataset	  		  
pred5.glm <- predict(glm.2, newdata = pred5, type = "link", se.fit = TRUE)

critval <- 1.96 ## approx 95% CI
upr <- pred5.glm$fit + (critval * pred5.glm$se.fit)
lwr <- pred5.glm$fit - (critval * pred5.glm$se.fit)
fit <- pred5.glm$fit

fit2 <- plogis(fit)
upr2 <- plogis(upr)
lwr2 <- plogis(lwr)

pred5 <- cbind(pred5, fit2, upr2, lwr2)

# Change labels for the factor levels for plotting
pred5$crab <- factor(pred5$crab, levels=c(0,1), labels=c("Ghost crab absent","Ghost crab present"))

write.csv(pred5, file="C:/Users/ebkwon/Google Drive/CAHA/Projects/Daily nest survival/dsr pred with tmax and crab.csv", row.names=FALSE)

# Plotting the predicted DSR 
p5 <-   ggplot() + 
        theme_bw() +
        geom_line(data=pred5,aes(x=tmax,y=fit2,lty=crab)) +  
        geom_line(data=pred5,aes(x=tmax,y=lwr2,lty=crab)) +  
        geom_line(data=pred5,aes(x=tmax,y=upr2,lty=crab)) +  
        geom_point(data=pred5,aes(x=tmax,y=fit2,shape=crab),size=3) +
		    scale_shape_manual(values=c(1,16)) +
		    scale_linetype_manual(values=c('dotted','dashed')) +
		    ggtitle("a.") +
	      xlab(expression("Daily maximum temperature" *~degree*C)) +
		    ylab("Daily nest survival rate with 95% CI") +
		    theme(plot.title = element_text(hjust=0, size = 14)) +
		    theme(panel.grid.minor = element_blank()) + 
	      theme(panel.grid.major = element_blank()) +
	      theme(axis.title.x = element_text(size = 14, vjust=-.5)) +
	      theme(axis.title.y = element_text(size = 14, vjust=1.5)) +
	      theme(axis.text.x = element_text(size=12)) +
	      theme(axis.text.y = element_text(size=12)) +
		    theme(panel.grid.minor = element_blank()) + 
		    theme(panel.grid.major = element_blank()) + 
		    theme(legend.title = element_blank())	+
        theme(legend.text = element_text(size=12)) +
        theme(legend.key = element_blank()) +
        theme(legend.position = c(0.75, 0.15), legend.justification = c(0.5, 0.5))

# Set working directory to the folder you want to save the plot				  	
setwd("C:/Users/ebkwon/Google Drive/CAHA/Projects/Daily nest survival")

# Save the plot as a .png file					  	
png(filename = "dsr_tmax_crab.png", width = 480 * 7, height = 480 * 7,
  pointsize = 12 * 1.5, res = 600)
p5
dev.off()


