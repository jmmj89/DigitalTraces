library(e1071)
library(plotly)
library(factoextra)
library(pca3d)
library(MASS)
library(nnet)

# setwd("C:\\DesktopPCGamer\\D\\Data\\IPSDS\\Disciplinas\\Project Consulting\\Digital Traces\\DigitalTraces-master\\")

source("read_data.R")
source("Classification_news_visits.R")
source("ISCED_coding.R")
source("data_prep.R")

set.seed(1)

head(per_part)

head(df.url1)

per_part_p <- per_part
per_part_p$routine <- per_part_p$routine/per_part_p$total_news_visit
per_part_p$search <- per_part_p$search/per_part_p$total_news_visit
per_part_p$social <- per_part_p$social/per_part_p$total_news_visit
per_part_p$unknown <- per_part_p$unknown/per_part_p$total_news_visit

per_part_p_escaled <- scale(per_part_p[, c("total_news_visit", "routine", "search", "social", "unknown")])
head(per_part_p_escaled)

# Determine number of clusters for proportionate variables
set.seed(1)
wss <- (nrow(per_part_p_escaled)-1)*sum(apply(per_part_p_escaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(per_part_p_escaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of clusters",
     ylab="Within groups sum of squares")

n_cluster_p <- 4

# K-Means cluster Analysis
fit_p <- kmeans(per_part_p_escaled, n_cluster_p) # We can choose more or less groups. We can decide it together

per_part_p_escaled <- as.data.frame(cbind(per_part_p_escaled,fit_p$cluster))
colnames(per_part_p_escaled)[ncol(per_part_p_escaled)] <- "cluster"

per_part_p$cluster <- per_part_p_escaled$cluster

table(per_part_p$cluster)

# Number of participants in clusters
#1   2   3   4 
#443 545 203 415 

#Function to print 7 graphics of each country with X axis = total_news_visit in a pdf file
GraphicByCountry <- function(df){
  pdf("teste.pdf")
  for(country in unique(df$Country)[1:3]){
    cat("Country: ", country, "\n")
    per_part_aux <- na.omit(per_part[per_part$Country == country,])
    
    print(table(per_part_aux$cluster))
    
    print(table(per_part_aux$Changing_Vote))
    
    graph1<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$routine, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph1)
    
    graph2<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$search, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph2)
    
    
    graph3<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$social, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph3)
    
    
    graph4<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$unknown, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph4)
    
    
    graph5<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$total_active_time, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph5)
    
    
    graph6<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$n, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph6)
    
    
    graph7<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$average_time, 
                                     color=per_part_aux$cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph7)
    
    
  }
  dev.off()
  
  invisible(NULL)
}

#GraphicByCountry(per_part_p)

# # get cluster means
groups_p <- aggregate(per_part_p_escaled,by=list(fit_p$cluster),FUN=mean)
groups_p

#Looking at the same time to the groups and graphics we can identify that we have clearly at least 4 groups:
# 1) People who consume lots of news, they tend to consume through routine and little through search and social
# 2) People who tend to consume few news and mostly through search
# 3) People who tend to consume few news and mostly through social
# 4) People who consume news moderately, mostly from unknown sources


# PCA
#1
# set.seed(1)
# pca <- prcomp(per_part[,2:totalvars], scale=T)
# summary(pca)
# 
# plot(pca$x, pch=20, col="blue", type="n") # To plot dots, drop type="n"
# text(pca$x, rownames(pca$x), cex=0.8)


#2
colnums <- which(colnames(per_part_p) %in% colnames(per_part_p_escaled))

set.seed(1)
res.pca <- prcomp(per_part_p[,colnums], scale = TRUE)

#Scree plot PCA
fviz_eig(res.pca)

# fviz_pca_ind(res.pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

fig_var <- fviz_pca_var(res.pca, axes = c(1, 2),
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE     # Avoid text overlapping
)
fig_var

# fviz_pca_biplot(res.pca, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )



set.seed(1)
km.res <- kmeans(per_part_p[,colnums], 4, nstart = 1)
# Visualize
fviz_cluster(km.res, data = per_part[,2:10],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())



#Adding the cluster variable after the PCA be computated
res.pca$x <- cbind(res.pca$x, per_part_p$cluster)
colnames(res.pca$x)[which(colnames(res.pca$x) == "")] <- "cluster"

fig <- plot_ly((as.data.frame(res.pca$x)), x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, colors = c('#AF382A', '#0C4B8E', '#DC4B8E', '#3A4E22') )
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig

# The most important variables to the PCs are total_news_visit and total_active_time. Pretty obvious, but it is good to confirm!
# The PCA also Confirm that the Knn is consistent in the clusters. Since there is no much intersection in the groups. At least outside the border!



# Didnt work
# fig_var_3d <- plot_ly((as.data.frame(res.pca$rotation)), x = ~PC1, y = ~PC2, z = ~PC3, type="scatter3d")
#, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
# fig_var_3d <- fig_var_3d %>%  add_annotations(text = rownames(res.pca$rotation))
# fig_var_3d <- fig_var_3d %>% layout(scene = list(xaxis = list(title = 'PC1'),
#                                                  yaxis = list(title = 'PC2'),
#                                                  zaxis = list(title = 'PC3')))
# fig_var_3d

pca3d(res.pca, group=factor(res.pca$x[,"cluster"]), fancy=FALSE, biplot=TRUE, palette = c('#AF382A', '#0C4B8E', '#DC4B8E', '#3A4E22'))

####

### Mulinominal logistic regression
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

full_dat <- merge(per_part_p, exp_dat, by = "panelist_id", all.x = TRUE)
full_dat[,c("cluster", "undecided", "polinterest")] <- as.data.frame(apply(full_dat[,c("cluster", "undecided", "polinterest")], 2, factor))
full_dat[,which(sapply(full_dat, is.character))] <- as.data.frame(apply(full_dat[,which(sapply(full_dat, is.character))], 2, factor))

full_dat$cluster <- relevel(full_dat$cluster, ref = "1") # We can later change the reference cluster (now it is routine)

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany

log_reg_res <- function(reg_model)
{
  res <- list()
  
  res[[1]] <-summary(reg_model)
  
  z <- summary(reg_model)$coefficients/summary(reg_model)$standard.errors
  res[[2]] <- z
  
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  res[[3]] <- p
  
  risk_ratio <- exp(coef(reg_model))
  res[[4]] <- risk_ratio
  
  names(res) <- c("summary", "z", "p", "risk_ratio")
  res
}

### Political explanatory variables
formula_pol <- as.formula(cluster ~ reg_vote + voted + change + undecided + polinterest.num + leftmidright.num +
                            trust.EP + trust.nat.pol)

mod_pol <- multinom(formula_pol, data = full_dat)
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
                                  trust.nat.pol + age_num + children + income + family + ISCED)

mod_pol_socio <- multinom(formula_pol_socio, data = full_dat)
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
