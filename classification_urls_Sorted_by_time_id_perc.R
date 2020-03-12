library(e1071)
library(plotly)
library(factoextra)
library(pca3d)

set.seed(1)


#Adding changing_vote
per_part$Changing_Vote <- df.survey$change[match(per_part$panelist_id,df.survey$panelist_id)];

head(per_part)

head(df.url1)

#Clustering
totalvars <- 9
# totalvars <- 13
# per_part$Percent_Routine <- per_part$routine/per_part$total_news_visit
# per_part$Percent_Search <- per_part$search/per_part$total_news_visit
# per_part$Percent_Social <- per_part$social/per_part$total_news_visit
# per_part$Percent_Unknown <- per_part$unknown/per_part$total_news_visit

per_part_escaled <- scale(per_part[,2:totalvars])

#Adding Country
per_part$Country <- df.socdem$country[match(per_part$panelist_id,df.socdem$panelist_id)];



# Determine number of clusters
wss <- (nrow(per_part_escaled)-1)*sum(apply(per_part_escaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(per_part_escaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

n_cluster <- 4

# K-Means Cluster Analysis
fit <- kmeans(per_part_escaled, n_cluster) # We can choose more or less groups. We can decide it together

# # get cluster means
groups <- aggregate(per_part_escaled,by=list(fit$cluster),FUN=mean)

groups[order(groups$total_news_visit),]

.per_part_escaled <- as.data.frame(cbind(per_part_escaled,fit$cluster))
colnames(per_part_escaled)[totalvars] <- "cluster"

per_part$Cluster <- per_part_escaled$cluster



#Function to print 7 graphics of each country with X axis = total_news_visit in a pdf file
GraphicByCountry <- function(df){
  pdf("teste.pdf")
  for(country in unique(df$Country)[1:3]){
    cat("Country: ", country, "\n")
    per_part_aux <- na.omit(per_part[per_part$Country == country,])
    
    print(table(per_part_aux$Cluster))
    
    print(table(per_part_aux$Changing_Vote))
    
    graph1<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$routine, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph1)
    
    graph2<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$search, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph2)
    
    
    graph3<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$social, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph3)
    
    
    graph4<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$unknown, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph4)
    
    
    graph5<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$total_active_time, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph5)
    
    
    graph6<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$n, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph6)
    
    
    graph7<-ggplot(per_part_aux, aes(x=per_part_aux$total_news_visit, y=per_part_aux$average_time, 
                                     color=per_part_aux$Cluster)) +
      geom_point() + ggtitle(country) +
      scale_color_gradientn(colours = rainbow(5))
    print(graph7)
    
    
  }
  dev.off()
  
  invisible(NULL)
}

GraphicByCountry(per_part)


#Looking at the same time to the groups and graphics we can identify that we have clearly at least 4 groups:
# 1) People who consume lots of news through routine and search enigine methods, there are many unknown sources for the news too.
# They tends to get few news from Social Medias.
# 3) People who consume a huge amount of news through Social Networks and Search engines and tends to be browsing a lot of time
# 2) People who consume few news, they consume mainly by search engines and social media and tends to be browsing a lot of time
# 4) People who tends to no consume news or consume very few news and spend just a few time browsing



# PCA
#1
set.seed(1)
pca <- prcomp(per_part[,2:10], scale=T)
summary(pca)

plot(pca$x, pch=20, col="blue", type="n") # To plot dots, drop type="n"
text(pca$x, rownames(pca$x), cex=0.8)


#2

set.seed(1)
res.pca <- prcomp(per_part[,2:10], scale = TRUE)

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

# fviz_pca_biplot(res.pca, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )



set.seed(1)
km.res <- kmeans(per_part[,2:10], 4, nstart = 1)
# Visualize
fviz_cluster(km.res, data = per_part[,2:10],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())



#Adding the cluster variable after the PCA be computated
res.pca$x <- cbind(res.pca$x, per_part$Cluster)
colnames(res.pca$x)[10] <- "Cluster"

fig <- plot_ly((as.data.frame(res.pca$x)), x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = c('#BF382A', '#0C4B8E', '#BF552A', '#BF882A'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig

# The most important variables to the PCs are total_news_visit and total_active_time. Pretty obvious, but it is good to confirm!
# The PCA also Confirm that the Knn is consistent in the clusters. Since there is no much intersection in the groups. At least outside the border!



# Didnt work
# fig_var_3d <- plot_ly((as.data.frame(res.pca$rotation)), x = ~PC1, y = ~PC2, z = ~PC3, type="scatter3d")#, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
# fig_var_3d <- fig_var_3d %>%  add_annotations(text = rownames(res.pca$rotation))
# fig_var_3d <- fig_var_3d %>% layout(scene = list(xaxis = list(title = 'PC1'),
#                                                  yaxis = list(title = 'PC2'),
#                                                  zaxis = list(title = 'PC3')))
# fig_var_3d

pca3d(res.pca, group=factor(res.pca$x[,"Cluster"]), fancy=FALSE, biplot=TRUE, palette = c('#BF382A', '#0C4B8E', '#BF552A', '#BF882A'))




