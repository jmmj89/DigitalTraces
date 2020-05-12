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

#all_cat <- read.csv("predicted_cats/Number_of_visits_all_Category.csv")

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

full_dat$cluster <- relevel(full_dat$cluster, ref = "1") # The reference category is routine.

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany


#Diagnostics
summary(full_dat)
#1 checking missings: have missingsin sociodemographics, we should omit them in the multinom regr.
colSums(is.na(full_dat)) 

log_reg_res <- function(reg_model) #Function to produce measures of the model and produce output tables:
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
  
  cluster2 <- rbind(risk_ratio[1,], summary(reg_model)$coefficients[1,], summary(reg_model)$standard.errors[1,],z[1,],p[1,]) #Get a table 
  #of coefficients, standard errors, z stats, and p values for cluster = 2 (cluster 1 'routine' is the reference category)
  rownames(cluster2) <- c("Risk ratio", "Coefficient","Std. Errors","z stat","p value")
  res[[6]] <- knitr::kable(cluster2)
  
  cluster3 <- rbind(risk_ratio[2,],summary(reg_model)$coefficients[2,],summary(reg_model)$standard.errors[2,],z[2,],p[2,]) #Get a table 
  #of coefficients, standard errors, z stats, and p values for cluster = 3
  rownames(cluster3) <- c("Risk ratio","Coefficient","Std. Errors","z stat","p value")
  res[[7]] <- knitr::kable(cluster3)
  
  cluster4 <- rbind(risk_ratio[3,], summary(reg_model)$coefficients[3,],summary(reg_model)$standard.errors[3,],z[3,],p[3,]) #Get a table 
  #of coefficients, standard errors, z stats, and p values for cluster = 4 
  rownames(cluster4) <- c("Risk ratio", "Coefficient","Std. Errors","z stat","p value")
  res[[8]] <- knitr::kable(cluster4)
  
  
  prob <-summary(reg_model)$fitted.values # We can also use probabilities to understand our model: 
  res[[9]] <- prob
  
  
  # Check the model accuracy by building classification table:  
  full_dat$predicted <- predict(reg_model, newdata = full_dat, "class") #Predicting the values for the dataset
  ctable <- table(full_dat$cluster, full_dat$predicted)  # Building classification table
  acc <- round((sum(diag(ctable))/sum(ctable))*100,2) # Calculating accuracy - sum of diagonal elements divided by total obs
  res[[10]] <- acc
  
  names(res) <- c("summary", "z", "p", "risk_ratio", "Hessian", "cluster2", "cluster3", "cluster4", "prob", "acc")
  res
}


### Political explanatory variables

#Here are the variables I used. These should be used in the random forest as well to be able to compare the results.
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num +
                            trust.EP + trust.nat.pol)

mod_pol <- multinom(formula_pol, data = full_dat,
                    na.action = na.omit, # omit missing observations
                    Hess = TRUE) #get Hessian observed/expected information matrix
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

### Political and sociodemographical explanatory variables together
#Here are the variables I used. These should be used in the random forest as well to be able to compare the results.
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

## Model evaluation

#function to get convergence, AIC and accuracy for evaluation table.
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


#Model evaluation data for total data and per countries for the political variables model.
pol <- rbind(total_pol, pol_France, pol_Germany, pol_UK)
str(pol)

#For politics and sociodemographics together.
total_pol_socio <- as.data.frame(selection_res(res_pol_socio))
str(total_pol_socio)
rownames(total_pol_socio) <-"pol_socio"

pol_socio_France <- as.data.frame(selection_res(res_pol_socio_France))
rownames(pol_socio_France) <-"pol_socio_France"

pol_socio_UK <- as.data.frame(selection_res(res_pol_socio_UK))
rownames(pol_socio_UK) <-"pol_socio_UK"

pol_socio_Germany <- as.data.frame(selection_res(res_pol_socio_Germany))
rownames(pol_socio_Germany) <-"pol_socio_Germany"

#Model evaluation data for total data and per countries for the political and sociodemographic variables model.
pol_socio <- rbind(total_pol_socio, pol_socio_France, pol_socio_Germany, pol_socio_UK)
str(pol_socio)

### Random Forest
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

# Splitting the data per countries

dir.create("random_forest_combined_res", showWarnings = FALSE)
rand_forest_res <- function(frml, data, dir, file_min_depth, file_multi_importance, file_multi_way_gini,
                            file_interactions, file_interactions_relevant)
{
  res <- list()
  forest <- randomForest(frml, data = data, localImp = TRUE, na.action = na.omit)
  res[[1]] <- forest
  
  #Distribution of minimal depth
  min_depth_frame <- min_depth_distribution(forest)
  min_depth <- plot_min_depth_distribution(min_depth_frame)
  png(filename = paste(dir, file_min_depth, sep = "/"))
  plot(min_depth)
  dev.off()
  
  # Various variable importance measures
  importance_frame <- measure_importance(forest)
  res[[2]] <- importance_frame
  multi_way_importance <- plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
  png(filename = paste(dir, file_multi_importance, sep = "/"))
  plot(multi_way_importance)
  dev.off()
  
  # 
  png(filename = paste(dir, file_multi_way_gini, sep = "/"))
  multi_way_importance_gini <- plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease",
                                                         y_measure = "gini_decrease", size_measure = "p_value")
  plot(multi_way_importance_gini)
  dev.off()
  
  # Variable interactions
  # 5 most important variables (minimal depth and number of trees)
  top_vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees"))
  res[[3]] <- top_vars
  interactions_frame <- min_depth_interactions(forest, top_vars)
  res[[4]] <- interactions_frame
  png(filename = paste(dir, file_interactions, sep = "/"))
  interactions <- plot_min_depth_interactions(interactions_frame)
  plot(interactions)
  dev.off()
  
  # Interactions in relevant trees
  interactions_frame <- min_depth_interactions(forest, top_vars,
                                               mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
  png(filename = paste(dir, file_interactions_relevant, sep = "/"))
  interactions_relevant <- plot_min_depth_interactions(interactions_frame)
  plot(interactions_relevant)
  dev.off()
  
  names(res) <- c("forest", "importance_frame", "top_vars", "interactions_frame")
  res
}

### Political explanatory variables
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num + trust.EP + trust.nat.pol)

set.seed(53)
forest_pol <- rand_forest_res(formula_pol, full_dat, dir ="random_forest_res", "min_depth_pol.png", "multi_importance_pol.png",
                              "multi_importance_gini_pol.png", "interactions_pol.png", "interactions_relevant_pol.png")
forest_pol

# Political explanatory variables per countries
forest_pol_France <- rand_forest_res(formula_pol, France, dir ="random_forest_res", "min_depth_pol_fr.png", "multi_importance_pol_fr.png",
                                     "multi_importance_gini_pol_fr.png", "interactions_pol_fr.png", "interactions_relevant_pol_fr.png")
forest_pol_UK <- rand_forest_res(formula_pol, UK, dir ="random_forest_res", "min_depth_pol_uk.png", "multi_importance_pol_uk.png",
                                 "multi_importance_gini_pol_uk.png", "interactions_pol_uk.png", "interactions_relevant_pol_uk.png")
forest_pol_Germany <- rand_forest_res(formula_pol, Germany, dir ="random_forest_res", "min_depth_pol_de.png", "multi_importance_pol_de.png",
                                      "multi_importance_gini_pol_de.png", "interactions_pol_de.png", "interactions_relevant_pol_de.png")
forest_pol_France
forest_pol_UK 
forest_pol_Germany

### Political and sociodemographical explanatory variables
# There are some missing values in sociodemographical variables:
# Number of cases with missing values:
length(which(is.na(full_dat$children)))
# These will be omitted

formula_pol_socio <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num + trust.EP +
                                  trust.nat.pol + age_num + children + income + family + ISCED)

forest_pol_socio <- rand_forest_res(formula_pol_socio, full_dat, dir ="random_forest_res", "min_depth_pol_socio.png", "multi_importance_pol_socio.png",
                                    "multi_importance_gini_pol_socio.png", "interactions_pol_socio.png", "interactions_relevant_pol_socio.png")
forest_pol_socio

# Political and sociodemographical explanatory variables per countries
forest_pol_socio_France <- rand_forest_res(formula_pol_socio, France, dir ="random_forest_res", "min_depth_pol_socio_fr.png", "multi_importance_pol_socio_fr.png",
                                           "multi_importance_gini_pol_socio_fr.png", "interactions_pol_socio_fr.png", "interactions_relevant_pol_socio_fr.png")
forest_pol_socio_UK <- rand_forest_res(formula_pol_socio, UK, dir ="random_forest_res", "min_depth_pol_socio_uk.png", "multi_importance_pol_socio_uk.png",
                                       "multi_importance_gini_pol_socio_uk.png", "interactions_pol_socio_uk.png", "interactions_relevant_pol_socio_uk.png")
forest_pol_socio_Germany <- rand_forest_res(formula_pol_socio, Germany, dir ="random_forest_res", "min_depth_pol_socio_de.png", "multi_importance_pol_socio_de.png",
                                            "multi_importance_gini_pol_socio_de.png", "interactions_pol_socio_de.png", "interactions_relevant_pol_socio_de.png")
forest_pol_socio_France
forest_pol_socio_UK 
forest_pol_socio_Germany
