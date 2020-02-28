#setwd("C:\\DesktopPCGamer\\D\\Data\\IPSDS\\Disciplinas\\Project Consulting\\Grouping\\")
#setwd("C:\\Users\\jose\\Desktop\\Pessoal\\Docs Pessoais\\IPSDS\\Disciplinas\\Project Consulting\\Digital Traces\\IPSDS\\")

library(tidyverse)
#library (feather)

library(e1071)
library(ggplot2)

set.seed(1)
#df.survey <- feather::read_feather("df.survey.feather")
#df.socdem <- feather::read_feather("df.socdem.feather")
#df.url1 <- feather::read_feather("df.url1.feather")
#df.visits <- feather::read_feather("df.visits.feather")

df.url1 <- readRDS("./URL1.rds")
df.socdem <- readRDS("./sociodemo.rds")
df.survey <- readRDS("./dat_surv.rds")

df.url1 <- df.url1[
  with(df.url1, order(df.url1$panelist_id, df.url1$web_visits_id, df.url1$used_at)),
  ]

ids_news <- which(grepl("news", df.url1$category))

#df.url1$before_news <- rep(0, nrow(df.url1))
df.url1$news <- rep(0, nrow(df.url1))

#df.url1[ids_news - 1,"before_news"] <- 1
df.url1[ids_news, "news"] <- 1

# Excluding visits with less than 10s
total_time <- df.url1 %>%
  group_by(web_visits_id) %>%
  summarise(active_seconds = sum(active_seconds))
short_visits <- unlist(total_time[which(total_time$active_seconds < 10), "web_visits_id"])

#Number of webvisits excluded because they are shorter than 10s
length(short_visits)

df.url1 <- df.url1[-which(df.url1$web_visits_id %in% short_visits),]

#df.url1 <- df.url1[which(df.url1$active_seconds >= 10 | df.url1$before_news == 1),]

# Excluding participants with no news consumption
news_total <- df.url1 %>%
  group_by(panelist_id) %>%
  summarise(news_total = sum(news))
no_news <- unlist(news_total[which(news_total$news_total == 0), "panelist_id"])

#Number of participants participants excluded, because they have no news consumption 
#(webvisits on sites in the category news longer than 10s)
length(no_news)

df.url1 <- df.url1[-which(df.url1$panelist_id %in% no_news),]

ids_social <- which(grepl("social", df.url1$category))
ids_search <- which(grepl("search", df.url1$category))

df.url1$social <- rep(0, nrow(df.url1))
df.url1$search <- rep(0, nrow(df.url1))

df.url1[ids_social,"social"] <- 1
df.url1[ids_search,"search"] <- 1

# First visit on a site
df.url1 <- mutate(df.url1, prev_web_visits_id = lag(web_visits_id))

df.url1$first_visit <- 0
df.url1$first_visit[1] <- 1
df.url1$first_visit[df.url1$web_visits_id != df.url1$prev_web_visits_id] <- 1
df.url1$first_news <- 0
df.url1$first_news[which(df.url1$first_visit == 1 & df.url1$news == 1)] <- 1

# Order by time
df.url1 <- df.url1[order(df.url1$panelist_id, df.url1$used_at),]

# Database with information about the last visit
df.url1 <- mutate(df.url1, prev_panelist_id = lag(panelist_id), prev_social = lag(social), prev_search = lag(search),
                  prev_used_at = lag(used_at))

# Time spent since last visit
df.url1$time_diff <- df.url1$used_at - df.url1$prev_used_at
df.url1$time_diff <- df.url1$time_diff < 300 & df.url1$prev_panelist_id == df.url1$panelist_id

# Number of news visits with more than 5 minutes after the last visit
nrow(df.url1[df.url1$time_diff == FALSE & df.url1$first_news == 1,])

# Complicated urls
nr_slash <- sapply(df.url1[df.url1$first_news == 1, "url"], str_count, "/")
df.url1$nr_slash <- 0
df.url1[df.url1$first_news == 1, "nr_slash"] <- nr_slash >= 2

# Check if last webvisit is from the previous participant
df.url1$search[df.url1$panelist_id != df.url1$prev_panelist_id] <- 0
df.url1$social[df.url1$panelist_id != df.url1$prev_panelist_id] <- 0

# Classification
df.url1$classification <- 0

df.url1$classification[which(df.url1$first_news == 1 & df.url1$nr_slash == 0)] <- 1
df.url1$classification[which(df.url1$first_news == 1 & df.url1$prev_search == 1 &
                               df.url1$nr_slash == 1 & df.url1$time_diff == TRUE)] <- 2
df.url1$classification[which(df.url1$first_news == 1 & df.url1$prev_social == 1 &
                               df.url1$nr_slash == 1 & df.url1$time_diff == TRUE)] <- 3
df.url1$classification[which(df.url1$first_news == 1 & ((df.url1$time_diff == TRUE & df.url1$prev_search != 1 & 
                                                           df.url1$prev_social != 1) | df.url1$time_diff == FALSE) &
                               df.url1$nr_slash == 1)] <- "unknown"

table(df.url1[df.url1$first_news == 1,]$classification)

# Number of news consumption modes per participant
### Adding total active time 
per_part <- df.url1 %>%
  group_by(panelist_id) %>%
  summarise(total_news_visit = sum(first_news),
            routine = sum(classification == 1),
            search = sum(classification == 2),
            social = sum(classification == 3),
            unknown = sum(classification == "unknown"),
            total_active_time = sum(active_seconds),
            n = n(),
            average_time = total_active_time / n()
  )

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


