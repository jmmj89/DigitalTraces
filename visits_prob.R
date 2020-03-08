library(brms)
library(lubridate)


all_visits <- merge(df.visits, df.url1[df.url1$first_news == 1, c("web_visits_id", "classification")], by.x = "visit_id",
                by.y = "web_visits_id", all.x = TRUE)
all_visits <- all_visits[-which(all_visits$duration < 10),]
all_visits <- all_visits[order(all_visits$pseudonym, all_visits$used_at),]

# News, Social and search categories
all_visits$news <- 0
all_visits$social <- 0
all_visits$search <- 0

all_visits[all_visits$d_kind %in% c("desktop", "mobile") & all_visits$category %in% selected,]$news <- 1
all_visits[all_visits$d_kind == "app" & grepl("News", all_visits$category, ignore.case=TRUE),]$news <- 1

all_visits$social[which(grepl("social", all_visits$category, ignore.case=TRUE))] <- 1
all_visits$search[which(grepl("Search", all_visits$category, ignore.case=TRUE))] <- 1

# Splitting the data by kind
prev_visit <- function(kind)
{
  tmp <- all_visits[all_visits$d_kind == kind,]
  nrow(tmp)
  # Info about previous visit (previous social/search 1: At least 1 from the last webvisit was search or social within 5 minutes)
  tmp <- mutate(tmp, prev_pseudonym_1 = lag(pseudonym), prev_pseudonym_2 = lag(lag(pseudonym)), prev_pseudonym_3 = lag(lag(lag(pseudonym))),
                prev_used_at_1 = lag(used_at), prev_social_1 = lag(social), prev_search_1 = lag(search),
                prev_used_at_2 = lag(lag(used_at)), prev_social_2 = lag(lag(social)), prev_search_2 = lag(lag(search)),
                prev_used_at_3 = lag(lag(lag(used_at))), prev_social_3 = lag(lag(lag(social))), prev_search_3 = lag(lag(lag(search)))) 
  # Time spent since last visit and same participant
  tmp$time_diff_1 <- as.numeric(difftime(tmp$used_at, tmp$prev_used_at_1, units = "secs"))
  tmp$stream_1 <- tmp$time_diff_1 < 300 & tmp$prev_pseudonym_1 == tmp$pseudonym
  tmp$time_diff_2 <- as.numeric(difftime(tmp$used_at, tmp$prev_used_at_2, units = "secs"))
  tmp$stream_2 <- tmp$time_diff_2 < 300 & tmp$prev_pseudonym_2 == tmp$pseudonym
  tmp$time_diff_3 <- as.numeric(difftime(tmp$used_at, tmp$prev_used_at_3, units = "secs"))
  tmp$stream_3 <- tmp$time_diff_3 < 300 & tmp$prev_pseudonym_3 == tmp$pseudonym
  
  tmp$prev_search <- 0
  tmp$prev_social <- 0
  
  tmp$prev_search[tmp$prev_search_1 == 1 & tmp$stream_1 == TRUE] <- 1
  tmp$prev_social[tmp$prev_social_1 == 1 & tmp$stream_1 == TRUE] <- 1
  tmp$prev_search[tmp$prev_social == 0 & tmp$prev_search_2 == 1 & tmp$stream_2 == TRUE] <- 1
  tmp$prev_social[tmp$prev_search == 0 & tmp$prev_social_2 == 1 & tmp$stream_2 == TRUE] <- 1
  tmp$prev_search[tmp$prev_social == 0 & tmp$prev_search_3 == 1 & tmp$stream_3 == TRUE] <- 1
  tmp$prev_social[tmp$prev_search == 0 & tmp$prev_social_3 == 1 & tmp$stream_3 == TRUE] <- 1

  tmp
}
desktop <- prev_visit("desktop")
app <- prev_visit("app")
mobile <- prev_visit("mobile")
nrow(desktop)
nrow(app)
nrow(mobile)

# Info about participants
part_level <- rbind(desktop, app, mobile) %>%
  group_by(pseudonym) %>%
  summarise(n = n(),
            p_news = sum(news)/n(),
            p_social = sum(social)/n(),
            p_search = sum(search)/n(),
            p_after_social = sum(news == 1 & prev_social == 1)/sum(news),
            p_after_search = sum(news == 1 & prev_search == 1)/sum(news))
part_level$p_after_search[which(is.na(part_level$p_after_search))] <- 0
part_level$p_after_social[which(is.na(part_level$p_after_social))] <- 0
# Removing participants with no news consumtion
part_level <- part_level[-which(part_level$p_news == 0),]

# Scaling the variables
part_level_escaled <- cbind(part_level[,1], scale(part_level[,2:ncol(part_level)]))

# Merge web-visit level and participant level data
desktop <- merge(desktop, part_level_escaled, by = "pseudonym", all.x = TRUE)
app <- merge(app, part_level_escaled, by = "pseudonym", all.x = TRUE)
mobile <- merge(mobile, part_level_escaled, by = "pseudonym", all.x = TRUE)

desktop$prev_search <- as.factor(desktop$prev_search)
desktop$prev_social <- as.factor(desktop$prev_social)

#Train and test set
d_news <- desktop[which(desktop$news == 1),]
head(d_news)

set.seed(768787)
ids <- unlist(unique(d_news$pseudonym))
id_train <- sample(1:length(ids), (0.8 * length(ids)))
sel_part <- ids[id_train]
d_train <- d_news[which(d_news$pseudonym %in% sel_part), c("pseudonym", "classification", "prev_social", "prev_search", "p_news",
                                                           "p_social", "p_search", "p_after_social", "p_after_search")]
d_test <- d_news[-which(d_news$pseudonym %in% sel_part), c("pseudonym", "visit_id", "classification", "prev_social", "prev_search", "p_news",
                                                           "p_social", "p_search", "p_after_social", "p_after_search")]


d_train$classification <- as.factor(d_train$classification)

# Training the model
# Load Model
# mod <- readRDS("visits_model.rds")

# Train Model
mod <- brm(classification ~ prev_social + prev_search + p_news + p_social + 
             p_search + p_after_social + p_after_search + (1|pseudonym), d_train, family = "categorical", iter = 3000)

saveRDS(mod, "visits_model.rds")

# Evaluating the model
d_test$classification <- as.factor(d_test$classification)
pred <- predict(mod, d_test, allow_new_levels = TRUE, type = "response")
head(pred)

predicted <- max.col(pred)
predicted[which(predicted == 4)] <- "unknown"

d_test <- cbind(d_test, pred, predicted)
acc_routine <- nrow(d_test[which(d_test$classification == "1" &
                                   d_test$predicted == "1"),])/nrow(d_test[which(d_test$classification == "1"),])
acc_search <- nrow(d_test[which(d_test$classification == "2" &
                                   d_test$predicted == "2"),])/nrow(d_test[which(d_test$classification == "2"),])
acc_social <- nrow(d_test[which(d_test$classification == "3" &
                                  d_test$predicted == "3"),])/nrow(d_test[which(d_test$classification == "3"),])
acc_unknown <- nrow(d_test[which(d_test$classification == "unknown" &
                                  d_test$predicted == "unknown"),])/nrow(d_test[which(d_test$classification == "unknown"),])
#acc_total <- nrow(d_test[which(d_test$classification == d_test$predicted),])/nrow(d_test)
#acc_modal <- nrow(d_test[which(d_test$classification == "1"),])/nrow(d_test)

acc_routine
acc_search
acc_social
acc_unknown
#acc_total
#acc_modal

# Predicting for app and mobile data
app <- app[which(app$news == 1),]
mobile <- mobile[which(mobile$news == 1),]
app <- app[, c("pseudonym", "visit_id", "classification", "prev_social", "prev_search", "p_news",
               "p_social", "p_search", "p_after_social", "p_after_search")]
mobile <- mobile[, c("pseudonym", "visit_id", "classification", "prev_social", "prev_search", "p_news",
               "p_social", "p_search", "p_after_social", "p_after_search")]

app$prev_search <- as.factor(app$prev_search)
app$prev_social <- as.factor(app$prev_social)
app$classification <- as.factor(app$classification)

mobile$prev_search <- as.factor(mobile$prev_search)
mobile$prev_social <- as.factor(mobile$prev_social)
mobile$classification <- as.factor(mobile$classification)

# App (In two parts - limited memory)
first_ind <- sample(length(unlist(unique(app$pseudonym))), length(unlist(unique(app$pseudonym)))/2)
first <- unlist(unique(app$pseudonym))[first_ind]
second <- unlist(unique(app$pseudonym))[-first_ind]

pred <- predict(mod, app[which(app$pseudonym %in% first),], allow_new_levels = TRUE, type = "response")
predicted <- max.col(pred)
predicted[which(predicted == 4)] <- "unknown"
app_first <- cbind(app[which(app$pseudonym %in% first),], pred, predicted)

pred <- predict(mod, app[which(app$pseudonym %in% second),], allow_new_levels = TRUE, type = "response")
predicted <- max.col(pred)
predicted[which(predicted == 4)] <- "unknown"
app_second <- cbind(app[which(app$pseudonym %in% second),], pred, predicted)

app <- rbind(app_first, app_second)

# Mobile
x <- split(unlist(unique(mobile$pseudonym)), cut(seq_along(unlist(unique(mobile$pseudonym))), 3, labels = FALSE)) 
res <- list()

for(i in 1:3)
{
  pred <- predict(mod, mobile[which(mobile$pseudonym %in% x[[i]]),], allow_new_levels = TRUE, type = "response")
  predicted <- max.col(pred)
  predicted[which(predicted == 4)] <- "unknown"
  res[[i]] <- cbind(pred, predicted)
}
pred <- as.data.frame(do.call("rbind", res))
pred[,1:4] <- apply(pred[,1:4], 2, as.character)
pred[,1:4] <- apply(pred[,1:4], 2, as.numeric)
mobile <- cbind(mobile, pred)

# Number of webvisits in the categories on participant level
all_pred <- rbind(app, mobile)
dir.create("predicted_cats", showWarnings = FALSE)
write.csv(all_pred, "predicted_cats/Prediction_mobile_App.csv")

per_part_mob <- all_pred %>%
  group_by(pseudonym) %>%
  summarise(routine = sum(predicted == 1),
            search = sum(predicted == 2),
            social = sum(predicted == 3),
            unknown = sum(predicted == "unknown"),
            n = n())
colnames(per_part_mob)[colnames(per_part_mob) == "pseudonym"] <- "panelist_id"

all_cat <- rbind(per_part[,-which(colnames(per_part) %in% c("average_time", "total_news_visit", "total_active_time", "div_news"))],
                 per_part_mob)
all_cat <- all_cat %>%
  group_by(panelist_id) %>%
  summarise(routine = sum(routine),
            search = sum(search),
            social = sum(social),
            unknown = sum(unknown),
            n = sum(n))           
write.csv(all_cat, "predicted_cats/Number_of_visits_all_Category.csv")

