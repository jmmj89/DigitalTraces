df.url1 <- df.url1[
  with(df.url1, order(df.url1$panelist_id, df.url1$web_visits_id, df.url1$used_at)),
  ]
# Selected news categories
selected <- c(
  "blogsandpersonal,business,newsandmedia",
  "blogsandpersonal,newsandmedia",
  "business,economyandfinance,newsandmedia",
  "business,education,newsandmedia",
  "business,games,newsandmedia",
  "business,entertainment,newsandmedia",
  "business,informationtech,newsandmedia",
  "business,newsandmedia",
  "business,newsandmedia,shopping", 
  "business,newsandmedia,sports",
  "business,newsandmedia,streamingmedia", 
  "business,newsandmedia,travel",
  "business,newsandmedia,vehicles",
  "economyandfinance,education,newsandmedia",
  "economyandfinance,newsandmedia",
  "education,entertainment,newsandmedia",
  "education,informationtech,newsandmedia",
  "education,newsandmedia",
  "education,newsandmedia,religion", 
  "education,newsandmedia,sports",
  
  "health,newsandmedia",
  
  "newsandmedia",
  "newsandmedia,blogsandpersonal,education",
  "newsandmedia,business ",
  "newsandmedia,economyandfinance", 
  "newsandmedia,economyandfinance,business",
  "newsandmedia,education", 
  "newsandmedia,education,business",
  
  "newsandmedia,travel",
  
  "searchenginesandportals,newsandmedia")

#ids_news <- which(grepl("news", df.url1$category))
ids_news <- which(df.url1$category %in% selected)

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

# Diversity of news consumption
div <- df.url1[df.url1$news == 1,] %>%
  group_by(panelist_id, domain) %>%
  summarise()
div <- div %>%
  group_by(panelist_id) %>%
  summarise(div_news = n())

# Number of news consumption modes per participant
#per_part <- df.url1 %>%
#  group_by(panelist_id) %>%
#  summarise(total_news_visit = sum(first_news),
#            routine = sum(classification == 1),
#            search = sum(classification == 2),
#            social = sum(classification == 3),
#            unknown = sum(classification == "unknown"),
#            total_active_time = sum(active_seconds),
#            n = n(),
#            average_time = total_active_time / n()
#  )
#per_part <- merge(per_part, div, by = "panelist_id", all.x = TRUE)

# Number of web-visits in categories of news consumption
per_part <- df.url1 %>%
  group_by(panelist_id) %>%
  summarise(total_news_visit = sum(first_news),
            routine = sum(classification == 1),
            search = sum(classification == 2),
            social = sum(classification == 3),
            unknown = sum(classification == "unknown")
  )

# Number of web-visits, news visits and active time on desktop (from the visits data, excluding the participants who were excluded
# from the analysis because no news consumption longer than 10s)
per_part_visits <- df.visits[df.visits$d_kind == "desktop" & df.visits$pseudonym %in% per_part$panelist_id,] %>%
  group_by(pseudonym) %>%
  summarise(total_active_time = sum(duration),
            n = n(),
            average_time = total_active_time / n()
  )
colnames(per_part_visits)[which(colnames(per_part_visits) == "pseudonym")] <- "panelist_id"

per_part <- merge(per_part, per_part_visits, by = "panelist_id", all.x = TRUE)
per_part <- merge(per_part, div, by = "panelist_id", all.x = TRUE)


head(per_part)

head(df.url1)
