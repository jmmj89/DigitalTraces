library(e1071)

set.seed(1)

#Adding Country
per_part$Country <- df.socdem$country[match(per_part$panelist_id,df.socdem$panelist_id)];
per_part$Cluster <- per_part_escaled$cluster

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

# Determine number of clusters
wss <- (nrow(per_part_escaled)-1)*sum(apply(per_part_escaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(per_part_escaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

n_cluster <- 5

# K-Means Cluster Analysis
fit <- kmeans(per_part_escaled, n_cluster) # We can choose more or less groups. We can decide it together

# # get cluster means
groups <- aggregate(per_part_escaled,by=list(fit$cluster),FUN=mean)

groups[order(groups$total_news_visit),]

per_part_escaled <- as.data.frame(cbind(per_part_escaled,fit$cluster))
colnames(per_part_escaled)[totalvars] <- "cluster"





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
# 1) People who consume lots of news through routine and search enigine methods 
# 4) People who consume a huge amount of news through Social Networks and Search engines and 
#tends to navigate a lot of time
# 2) People who consume few news, they consume mainly by search engines.
# 3) and 5) people who tends to no consume news or consume very few news


