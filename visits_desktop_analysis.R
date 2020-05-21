library(e1071)
library(plotly)
library(factoextra)
library(pca3d)
library(MASS)
library(nnet)
library(randomForest)
library(randomForestExplainer)

source("read_data.R")
source("Classification_news_visits.R")
source("ISCED_coding.R")
source("data_prep.R")
source("visits_prob.R")

all_cat <- read.csv("predicted_cats/Number_of_visits_all_Category.csv")

head(all_cat)

all_cat_p <- all_cat
all_cat_p$routine <- all_cat_p$routine/all_cat_p$total_news_visit
all_cat_p$search <- all_cat_p$search/all_cat_p$total_news_visit
all_cat_p$social <- all_cat_p$social/all_cat_p$total_news_visit
all_cat_p$unknown <- all_cat_p$unknown/all_cat_p$total_news_visit

all_cat_p_escaled <- as.data.frame(cbind("panelist_id" = all_cat_p$panelist_id, scale(all_cat_p[, c("total_news_visit", "routine", "search", "social", "unknown")])))
head(all_cat_p_escaled)

nrow(all_cat_p_escaled[which(all_cat_p_escaled$total_news_visit > 5),])
all_cat_p_escaled <- all_cat_p_escaled[-which(all_cat_p_escaled$total_news_visit > 5),]

# Determine number of clusters for proportionate variables
set.seed(1)
wss <- (nrow(all_cat_p_escaled)-1)*sum(apply(all_cat_p_escaled[,-which(colnames(all_cat_p_escaled) == "panelist_id")],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(all_cat_p_escaled[,-which(colnames(all_cat_p_escaled) == "panelist_id")],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of clusters",
     ylab="Within groups sum of squares")

n_cluster_p <- 4

# K-Means cluster Analysis
fit_p <- kmeans(all_cat_p_escaled[,-which(colnames(all_cat_p_escaled) == "panelist_id")], n_cluster_p) # We can choose more or less groups. We can decide it together

all_cat_p_escaled <- as.data.frame(cbind(all_cat_p_escaled, fit_p$cluster))
colnames(all_cat_p_escaled)[ncol(all_cat_p_escaled)] <- "cluster"

# Number of participants in clusters
#1   2   3   4 
#804 35 656 720 

# # get cluster means
groups_p <- aggregate(all_cat_p_escaled,by=list(fit_p$cluster),FUN=mean)
groups_p

# Recode clusters to be similar to the clusters based on desktop data
# 1) People who consume lots of news, they tend to consume through routine and little through search and social (4. cluster here)
# 2) People who tend to consume few news and mostly through search (1. cluster here)
# 3) People who tend to consume few news and mostly through social (2. cluster here)
# 4) People who consume news moderately, mostly from unknown sources (3. cluster here)

recoded_cl <- NULL
recoded_cl[all_cat_p_escaled$cluster == 1] <- 2
recoded_cl[all_cat_p_escaled$cluster == 2] <- 3
recoded_cl[all_cat_p_escaled$cluster == 3] <- 4
recoded_cl[all_cat_p_escaled$cluster == 4] <- 1
all_cat_p_escaled$cluster <- recoded_cl
groups_p <- aggregate(all_cat_p_escaled,by=list(fit_p$cluster),FUN=mean)
groups_p

all_cat_p <- merge(all_cat_p, all_cat_p_escaled[,c("panelist_id", "cluster")], by = "panelist_id")

table(all_cat_p$cluster)

### Mulinominal logistic regression

full_dat <- merge(all_cat_p, exp_dat, by = "panelist_id", all.x = TRUE)
full_dat[,c("cluster", "undecided", "polinterest")] <- as.data.frame(apply(full_dat[,c("cluster", "undecided", "polinterest")], 2, factor))
full_dat[,which(sapply(full_dat, is.character))] <- as.data.frame(apply(full_dat[,which(sapply(full_dat, is.character))], 2, factor))
str(full_dat$cluster)

full_dat$cluster <- relevel(full_dat$cluster, ref = "4") # Set the reference cluster category to ‘unknown’ news consumption

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany


# Get result statistics and output tables

log_reg_res <- function(reg_model)
{
  res <- list()
  
  res[[1]] <-summary(reg_model) # To explore the beta coefficients of the model
  
  z <- summary(reg_model)$coefficients/summary(reg_model)$standard.errors #z calculation for the regression coefficients
  res[[2]] <- z
  
  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
  res[[3]] <- p
  
  risk_ratio <- exp(coef(reg_model)) #To get the relative risk IE odds ratio, we need to exponentiate the coefficient
  res[[4]] <- risk_ratio
  
  hess <- reg_model$Hessian # Hessian observed/expected information matrix
  res[[5]] <- hess
  
  cluster1 <- rbind(risk_ratio[1,], summary(reg_model)$coefficients[1,], summary(reg_model)$standard.errors[1,],z[1,],p[1,]) #Get a table
  #of coefficients, standard errors, z stats, and p values for cluster = 1 (cluster 4 'unknown' is the reference category)
  rownames(cluster1) <- c("Risk ratio", "Coefficient","Std. Errors","z stat","p value")
  res[[6]] <- knitr::kable(cluster1)
  
  cluster2 <- rbind(risk_ratio[2,],summary(reg_model)$coefficients[2,],summary(reg_model)$standard.errors[2,],z[2,],p[2,]) #Get a table
  #of coefficients, standard errors, z stats, and p values for cluster = 2
  rownames(cluster2) <- c("Risk ratio","Coefficient","Std. Errors","z stat","p value")
  res[[7]] <- knitr::kable(cluster2)
  
  cluster3 <- rbind(risk_ratio[3,], summary(reg_model)$coefficients[3,],summary(reg_model)$standard.errors[3,],z[3,],p[3,]) #Get a table
  #of coefficients, standard errors, z stats, and p values for cluster = 3
  rownames(cluster3) <- c("Risk ratio", "Coefficient","Std. Errors","z stat","p value")
  res[[8]] <- knitr::kable(cluster3)
  
  
  prob <-summary(reg_model)$fitted.values # We can also use probabilities to understand our model:
  res[[9]] <- prob
  
  
  # Check the model accuracy by building classification table:  
  full_dat$predicted <- predict(reg_model, newdata = full_dat, "class") #Predicting the values for the dataset
  ctable <- table(full_dat$cluster, full_dat$predicted)  # Building classification table
  acc <- round((sum(diag(ctable))/sum(ctable))*100,2) # Calculating accuracy - sum of diagonal elements divided by total obs
  res[[10]] <- acc
  
  names(res) <- c("summary", "z", "p", "risk_ratio", "Hessian", "cluster1", "cluster2", "cluster3", "prob", "acc")
  res
}

### Political explanatory variables
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num +
                            trust.EP + trust.nat.pol)

mod_pol <- multinom(formula_pol, data = full_dat,
                    na.action = na.omit, # omit missing observations
                    Hess = TRUE) #get Hessian observed/expected information matrix)
res_pol <- log_reg_res(mod_pol)
res_pol


# Political explanatory variables per countries
mod_pol_France <- multinom(formula_pol, data = France)
mod_pol_UK <- multinom(formula_pol, data = UK)
mod_pol_Germany <- multinom(formula_pol, data = Germany)

res_pol_France <- log_reg_res(mod_pol_France)
res_pol_UK <- log_reg_res(mod_pol_UK)
res_pol_Germany <- log_reg_res(mod_pol_Germany)
res_pol_France
res_pol_UK
res_pol_Germany

### Political and sociodemographical explanatory variables
formula_pol_socio <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num + trust.EP +
                                  trust.nat.pol + gender + age_num + children + income + family + ISCED)

mod_pol_socio <- multinom(formula_pol_socio, data = full_dat,
                          na.action = na.omit, # omit missing observations
                          Hess = TRUE) #get Hessian observed/expected information matrix
res_pol_socio <- log_reg_res(mod_pol_socio)
res_pol_socio

# Political and sociodemographical explanatory variables per countries
mod_pol_socio_France <- multinom(formula_pol_socio, data = France)
mod_pol_socio_UK <- multinom(formula_pol_socio, data = UK)
mod_pol_socio_Germany <- multinom(formula_pol_socio, data = Germany)

res_pol_socio_France <- log_reg_res(mod_pol_socio_France)
res_pol_socio_UK <- log_reg_res(mod_pol_socio_UK)
res_pol_socio_Germany <- log_reg_res(mod_pol_socio_Germany)
res_pol_socio_France
res_pol_socio_UK
res_pol_socio_Germany

## Model description
print(res_pol$summary$convergence) #0 means converged
print(res_pol$summary$AIC) 
print(res_pol$acc) #accuracy


#Prepare model description tables
selection_res <- function(sel_crit)
{
  sel_crit <- c(sel_crit$summary$convergence, sel_crit$summary$AIC, sel_crit$acc) #model selection criteria
  mod_sel <- rbind(sel_crit)
  colnames(mod_sel) <- c("convergence","AIC","Accuracy")
  mod_sel
}

#For political variables
total_pol <- as.data.frame(selection_res(res_pol))
str(total_pol)
rownames(total_pol) <-"pol"

pol_France <- as.data.frame(selection_res(res_pol_France))
rownames(pol_France) <-"pol_France"

pol_UK <- as.data.frame(selection_res(res_pol_UK))
rownames(pol_UK) <-"pol_UK"

pol_Germany <- as.data.frame(selection_res(res_pol_Germany))
rownames(pol_Germany) <-"pol_Germany"


pol <- rbind(total_pol, pol_France, pol_Germany, pol_UK)
str(pol)

#For politics and sociodemographics
total_pol_socio <- as.data.frame(selection_res(res_pol_socio))
str(total_pol_socio)
rownames(total_pol_socio) <-"pol_socio"

pol_socio_France <- as.data.frame(selection_res(res_pol_socio_France))
rownames(pol_socio_France) <-"pol_socio_France"

pol_socio_UK <- as.data.frame(selection_res(res_pol_socio_UK))
rownames(pol_socio_UK) <-"pol_socio_UK"

pol_socio_Germany <- as.data.frame(selection_res(res_pol_socio_Germany))
rownames(pol_socio_Germany) <-"pol_socio_Germany"


pol_socio <- rbind(total_pol_socio, pol_socio_France, pol_socio_Germany, pol_socio_UK)
str(pol_socio)


# Effect stars
library(nnet)
library(EffectStars2)
library(graphics)

dir.create("effectstars_extended", showWarnings = FALSE)

#Function to prepare political effectstars

effectstars_pol <- function(model, file)
{
  filename <- paste("effectstars_extended/", file, ".png", sep = "")
  risk_ratio <- exp(coef(model)) #Get the Beta coefficients (risk ratios)   
  e_star_risk_ratio <- t(risk_ratio) #Transpose the risk ratio matrix in order to get the right format to feed into Effectstars2
  
  z <- summary(model)$coefficients/summary(model)$standard.errors #z calculation for the regression coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
  p_values <- formatC(p, format="f", digits=3) #format p
  labels <- matrix(paste0(rep(c("Search", "Social", "Unknown"), nrow(e_star_risk_ratio)), "\n(", p_values, ")"),
                   byrow = T, ncol = 3) #create labels containing the response categories and all p-values
  ctrl <- star.ctrl(lwd.circle = 3, col.circle = "lightblue", #graphical formatting
                    lty.circle = 5, col.fill = "lightgrey", lwd.star = 1.8,
                    cex.main = 1.5, cex.labels = 1.2, col.main = "black",
                    col.labels = "black", col.star = "black", dist.labels = 1.1, 
                    font.labels = 1, radius = 1)
  png(filename = filename, width=800, height=600)
  effectstars(e_star_risk_ratio,
              names = c("Intercept", "Registered voters", "Voted", rep("Changed mind", 3), 
                        "Undecided", "Polinterest", "Leftmidright", "Trust in EP", "Trust in NP"), #name of the star
              subs = c("", rep("(yes)", 2),  "(did not change)", "(did not vote)", "(doesn't remember)", "(yes)" , rep("", 4)), 
              #category labels of the predictors 
              labels = labels, #dependent variable categories
              control = ctrl) #graphic above
  dev.off()
  
  
}

#Plot political effectstars
effectstars_pol(mod_pol, "effectstars_pol")
effectstars_pol(mod_pol_France, "effectstars_pol_Fr")
effectstars_pol(mod_pol_Germany, "effectstars_po_Ger")
effectstars_pol(mod_pol_UK, "effectstars_UK")

#Function to prepare political and sociodemographic effectstars

effectstars_pol_socio <- function(model, file)
{
  filename <- paste("effectstars_extended/", file, ".png", sep = "")
  risk_ratio <- exp(coef(model)) #Get the Beta coefficients (risk ratios)   
  e_star_risk_ratio <- t(risk_ratio) #Transpose the risk ratio matrix in order to get the right format to feed into Effectstars2
  
  z <- summary(model)$coefficients/summary(model)$standard.errors #z calculation for the regression coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #2-tailed z test
  p_values <- formatC(p, format="f", digits=3) #format p
  labels <- matrix(paste0(rep(c("Search", "Social", "Unknown"), nrow(e_star_risk_ratio)), "\n(", p_values, ")"),
                   byrow = T, ncol = 3) #create labels containing the response categories and all p-values
  ctrl <- star.ctrl(lwd.circle = 3, col.circle = "lightblue", #graphical formatting
                    lty.circle = 5, col.fill = "lightgrey", lwd.star = 1.8,
                    cex.main = 1, cex.labels = 1, col.main = "black",
                    col.labels = "black", col.star = "black", dist.labels = 1.1, 
                    font.labels = 1, radius = 1)
  png(filename = filename, width=800, height=600)
  effectstars(e_star_risk_ratio,
              names = c("Intercept",   #name of the star 
                        "Registered voters", 
                        "Voted", 
                        rep("Changed mind", 3), 
                        "Undecided", 
                        "Polinterest", 
                        "Leftmidright", 
                        "Trust in EP", 
                        "Trust in NP", 
                        "Gender", 
                        "Age", 
                        rep("Chidren", 3), 
                        rep("Income", 7), 
                        rep("Family", 5), 
                        rep("ISCED", 3)),
              subs = c("", rep("(yes)", 2),  "(did not change)", "(did not vote)", "(doesn't remember)", "(yes)" , rep("", 4), "(male)",
                       "", "(2)", "(3+)", "(No)","(500-1000)", "(1000-1500)", "(1500-2000)", "(2000-2500)", "(2500+)" ,"(no income)", "(NA)",
                       "(divorced with partner)", "(divorced w/o partner)", "(married)", "(single with partner)", "(single w/o partner)", "ISCED3",
                       "ISCED4", "ISCED8"),
              #category labels of the predictors 
              labels = labels, #dependent variable categories
              
              control = ctrl) #graphic above
  dev.off()
}


#Plot political and sociodemographic effectstars
effectstars_pol_socio(mod_pol_socio, "effectstars_pol_socio")
effectstars_pol_socio(mod_pol_socio_France, "effectstars_pol_socio_Fr")
effectstars_pol_socio(mod_pol_socio_Germany, "effectstars_pol_socio_Ger")
effectstars_pol_socio(mod_pol_socio_UK, "effectstars_pol_socio_UK")
