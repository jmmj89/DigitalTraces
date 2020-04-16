library(e1071)
library(plotly)
library(factoextra)
library(pca3d)
library(MASS)
library(nnet)
library(randomForest)
library(randomForestExplainer)


setwd("C:\\DesktopPCGamer\\D\\Data\\IPSDS\\Disciplinas\\Project Consulting\\DigitalTraces_Project\\")
dir()

source("read_data.R", encoding = "UTF-8")
source("ISCED_coding.R", encoding = "UTF-8")
source("data_prep.R", encoding = "UTF-8")

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

### Random Forest
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
full_dat <- merge(per_part_p, exp_dat, by = "panelist_id", all.x = TRUE)
full_dat[,c("cluster", "undecided", "polinterest")] <- as.data.frame(apply(full_dat[,c("cluster", "undecided", "polinterest")], 2, factor))
full_dat[,which(sapply(full_dat, is.character))] <- as.data.frame(apply(full_dat[,which(sapply(full_dat, is.character))], 2, factor))

# Splitting the data per countries
countries <- split(full_dat, full_dat$country)
France <- countries$France
UK <- countries$UK
Germany <- countries$Germany

dir.create("random_forest_res", showWarnings = FALSE)
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
