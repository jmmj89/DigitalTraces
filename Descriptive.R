survey <- df.survey
socio <- df.socdem

## Data preparation

desc_summary <- c(nrow(survey), nrow(news_total), sum(news_total$news_total > 0), nrow(total_time),
                  nrow(total_time) - length(short_visits), sum(df.url1$first_news == 1))
names(desc_summary) <- c("Number of participants", "Number of participants with browsing activity",
                         "Number of participants with news consumption", "Number of webvisits", "Number of webvisits with at least 10s",
                         "Number of visits on news sites")

news_con <- merge(survey, per_part, by = "panelist_id", all.x = TRUE)
start_col <- which(colnames(news_con) == "total_news_visit")
end_col <- ncol(news_con)
news_con[,start_col:end_col] <- news_con[,start_col:end_col] %>%
  replace(., is.na(.), 0)

# Descriptive statistics per country
per_c <- as.data.frame(news_con %>%
  group_by(country) %>%
  summarise(n = n(),
           n_news = sum(total_news_visit > 0),
           n_routine = sum(routine > 0),
           n_search = sum(search > 0),
           n_social = sum(social > 0),
           n_reg_vote = sum(reg_vote == "Yes"),
           n_has_twitter = sum(has.twitter == "Yes"),
           n_has_facebook = sum(has.facebook == "Yes"),
           n_has_instagram = sum(has.instagram == "Yes"),
           n_has_linkedin = sum(has.linkedin == "Yes"),
           n_has_oth_smedia = sum(has.oth.smedia == "Yes"),
           n_voted = sum(voted == "Yes"),
           n_change = sum(change == "changed vote"),
           n_not_change = sum(change == "did not change"),
           n_not_vote = sum(change == "did not vote"),
           n_polinterest = sum(polinterest == 1)))
percentage <- (per_c[,which(grepl("n_", colnames(per_c)))]/per_c$n) * 100
colnames(percentage) <- paste("p_", str_remove(colnames(percentage), "n_"), sep="")
per_c <- cbind(per_c, percentage)

# Descriptive statistics overall
total_news <- news_con %>%
  group_by() %>%
  summarise(n = n(),
            n_news = sum(total_news_visit > 0),
            n_routine = sum(routine > 0),
            n_search = sum(search > 0),
            n_social = sum(social > 0),
            n_reg_vote = sum(reg_vote == "Yes"),
            n_has_twitter = sum(has.twitter == "Yes"),
            n_has_facebook = sum(has.facebook == "Yes"),
            n_has_instagram = sum(has.instagram == "Yes"),
            n_has_linkedin = sum(has.linkedin == "Yes"),
            n_has_oth_smedia = sum(has.oth.smedia == "Yes"),
            n_voted = sum(voted == "Yes"),
            n_change = sum(change == "changed vote"),
            n_not_change = sum(change == "did not change"),
            n_not_vote = sum(change == "did not vote"),
            n_polinterest = sum(polinterest == 1))
percentage <- (total_news[,which(grepl("n_", colnames(total_news)))]/total_news$n) * 100
colnames(percentage) <- paste("p_", str_remove(colnames(percentage), "n_"), sep="")
total_news <- cbind(total_news, percentage)

# Political orientation per country
leftmidright <- as.data.frame(news_con %>%
                                group_by(country, leftmidright.num) %>%
                                summarise(n = n()))
per_c_leftmidright <- leftmidright %>%
  spread(leftmidright.num, n)
percentage_leftmidright <- (per_c_leftmidright[,2:12]/rowSums(per_c_leftmidright[,2:12])) * 100
colnames(percentage_leftmidright) <- paste("p_", colnames(percentage_leftmidright), sep = "")
colnames(per_c_leftmidright)[-1] <- paste("n_", colnames(per_c_leftmidright)[-1], sep = "")
per_c_pol_ori <- cbind(per_c_leftmidright, percentage_leftmidright)

# Political orientation overall
t_leftmidright <- as.data.frame(news_con %>%
                                group_by(leftmidright.num) %>%
                                summarise(n = n()))
t_leftmidright <- t_leftmidright %>%
  spread(leftmidright.num, n)
t_percentage_leftmidright <- (t_leftmidright/sum(t_leftmidright)) * 100
colnames(t_percentage_leftmidright) <- paste("p_", colnames(t_percentage_leftmidright), sep = "")
colnames(t_leftmidright) <- paste("n_", colnames(t_leftmidright), sep = "")
t_pol_ori <- cbind(t_leftmidright, t_percentage_leftmidright)

# Sociodemographics
# Remove duplicated rows from socio
socio <- socio[-which(duplicated(socio)),]
# Collapsing categories (family, education, children, income)
socio$family <- forcats::fct_collapse(socio$family,
                      "1: Married" = c("1: Marié", "1: Married"),
                      "2: Civil Partnership" = c("2: Civil Partnership", "2: Pacsé"),
                      "3: Single, living with partner" = c("3: En couple", "3: Single, living with partner"),
                      "4: Single, not living with partner" = c("4: Célibataire", "4: Single, not living with partner"),
                      "5: Divorced/widowed, living with partner" = c("5: Divorcé/Veuf, en couple", "5: Divorced/widowed, living with partner"),
                      "6: Divorced/widowed, not living with partner" = c("6: Divorcé/Veuf, célibataire", "6: Divorced/widowed, not living with partner"))
socio$family <- gsub('\\d+: ', '', socio$family)

#socio$education[which(grepl("1: ", socio$education) | grepl("2: ", socio$education) |
#                        grepl("3: ", socio$education))] <- "1: High school or lower"
#socio$education[which(grepl("4: ", socio$education))] <- "2: Undergraduate degree or equivalent"
#socio$education[which(grepl("5: ", socio$education) | grepl("6: ", socio$education) |
#                        grepl("7: b", socio$education) | grepl("8: ", socio$education))] <- "3: Postgraduate degree or professional qualification"
#socio$education[which(grepl("6: ", socio$education))] <- "4: Professional Qualification"
#socio$education[which(grepl("(yet)", socio$education))] <- "No qualification (yet)"

# Income categories only untill 2500 pounds
socio$income[which(grepl("1: ", socio$income))] <- "1: under 500 EUR/pounds"
socio$income[which(grepl("2: ", socio$income))] <- "2: 500 to 1,000 EUR/pounds"
socio$income[which(grepl("3: ", socio$income))] <- "3: 1,000 to 1,500 EUR/pounds"
socio$income[which(grepl("4: ", socio$income))] <- "4: 1,500 to 2,000 EUR/pounds"
socio$income[which(grepl("5: ", socio$income))] <- "5: 2,000 to 2,500 EUR/pounds"
socio$income[which(grepl("6: ", socio$income) | grepl("7: ", socio$income) | grepl("8: 3", socio$income) |
                     grepl("9: ", socio$income) | grepl("10: ", socio$income) |
                     grepl("11: ", socio$income))] <- "6: Over 2,500 EUR/pounds"
socio$income[which(grepl("98: ", socio$income))] <- "98: no income"

# Number of children
socio$children <- forcats::fct_collapse(socio$children,
                                        "3: 3 Children and more" = c("3: 3 Children", "4: 4 Children",
                                                                     "5: 5 Children and more"))
socio$children <- gsub('\\d+: ', '', socio$children)

# Edit age categories
socio$age_class <- gsub('\\d+: ', '', socio$age_class)

# Prepare data visualization
socio_prep <- function(y)
{
  x <- socio$country
  tmp <- as.data.frame(cbind(table(x, y)))
  rownames(tmp) <- gsub('\\d+: ', '', rownames(tmp))
  tmp <- cbind(rownames(tmp), tmp)
  colnames(tmp)[1] <- "country"
  tmp <- gather(tmp, category_group, value, 2:ncol(tmp), factor_key = TRUE)
  tmp
}
soc_vars <- apply(socio[,c("age_class", "gender", "children", "income", "family")], 2, socio_prep)
soc_vars <- dplyr::bind_rows(soc_vars, .id = "variable")

soc_vars$country <- forcats::fct_collapse(soc_vars$country,
                                       "UK" = c("United Kingdom"))
soc_vars$variable[soc_vars$variable == "age_class"]  <- "age"

soc_vars$percentage <- 0
soc_vars$percentage[which(soc_vars$country == "UK")] <-
  100 * soc_vars$value[which(soc_vars$country == "UK")]/per_c$n[which(per_c$country == "UK")]
soc_vars$percentage[which(soc_vars$country == "France")] <-
  100 * soc_vars$value[which(soc_vars$country == "France")]/per_c$n[which(per_c$country == "France")]
soc_vars$percentage[which(soc_vars$country == "Germany")] <-
  100 * soc_vars$value[which(soc_vars$country == "Germany")]/per_c$n[which(per_c$country == "Germany")]

# Preparation data with clusters
# Clusters and countries
clus_country <- merge(per_part, survey, by = "panelist_id") %>%
  group_by(country, Cluster) %>%
  summarise(participants = n(),
            total_news_visit = sum(total_news_visit),
            routine = sum(routine),
            search = sum(search),
            social = sum(social),
            unknown = sum(unknown),
            webvisits = sum(n),
            voted = sum(voted == "Yes"),
            interested_in_politics = sum(polinterest == 1))
clus_country <- merge(clus_country, per_c[, c("country", "n_news")], by = "country", all.x = TRUE)
clus_country_percentage <- cbind("participants" = (clus_country$participants/clus_country$n_news) * 100,
                         (clus_country[,c("routine", "search", "social", "unknown")]/clus_country$total_news_visit) * 100,
                         (clus_country[,c("voted", "interested_in_politics")]/clus_country$participants) * 100)
colnames(clus_country_percentage) <- paste("p_", colnames(clus_country_percentage), sep="")
clus_country <- cbind(clus_country, clus_country_percentage)

clus_pol_c <- merge(per_part, survey, by = "panelist_id") %>%
  group_by(country, Cluster, leftmidright.num) %>%
  summarise(n_pol = n())
clus_pol_c <- merge(clus_pol_c, clus_country[, c("country", "Cluster", "participants")],
                    by = c("country", "Cluster"), all.x = TRUE)
clus_pol_c$p_pol <- (clus_pol_c$n_pol/clus_pol_c$participants) * 100
clus_pol_c$leftmidright.num <- as.factor(clus_pol_c$leftmidright.num)

# Only clusters
clus <- merge(per_part, survey, by = "panelist_id") %>%
  group_by(Cluster) %>%
  summarise(participants = n(),
            total_news_visit = sum(total_news_visit),
            routine = sum(routine),
            search = sum(search),
            social = sum(social),
            unknown = sum(unknown),
            webvisits = sum(n),
            voted = sum(voted == "Yes"),
            interested_in_politics = sum(polinterest == 1))
clus_percentage <- cbind("participants" = (clus$participants/total_news$n_news) * 100,
                                 (clus[,c("routine", "search", "social", "unknown")]/clus$total_news_visit) * 100,
                                 (clus[,c("voted", "interested_in_politics")]/clus$participants) * 100)
colnames(clus_percentage) <- paste("p_", colnames(clus_percentage), sep="")
clus <- cbind(clus, clus_percentage)

clus_pol <- merge(per_part, survey, by = "panelist_id") %>%
  group_by(Cluster, leftmidright.num) %>%
  summarise(n_pol = n())
clus_pol <- merge(clus_pol, clus[, c("Cluster", "participants")], by = "Cluster", all.x = TRUE)
clus_pol$p_pol <- (clus_pol$n_pol/clus_pol$participants) * 100
clus_pol$leftmidright.num <- as.factor(clus_pol$leftmidright.num)

## Data visualization

# Sociodemographics
dir.create("descriptives", showWarnings = FALSE)
png(filename = "descriptives/sociodemo_1.png", width=800, height=1000)
ggplot(soc_vars[soc_vars$variable %in% c("age", "gender", "children"),], aes(x = category_group, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Distribution of Sociodemographichal variables per countries (table 1)") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 70), breaks = c(0, 15, 30, 45, 60),
                     labels = c("0%", "15%", "30%", "45%", "60%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,2), "cm")) +
  facet_wrap(variable ~ country, ncol = 3, scales = "free")
dev.off()

png(filename = "descriptives/sociodemo_2.png", width=800, height=1000)
ggplot(soc_vars[soc_vars$variable %in% c("family", "income"),], aes(x = category_group, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Distribution of Sociodemographichal variables per countries (table 2)") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 70), breaks = c(0, 15, 30, 45, 60),
                     labels = c("0%", "15%", "30%", "45%", "60%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,2), "cm")) +
  facet_wrap(variable ~ country, ncol = 3, scales = "free")
dev.off()

# Number of participants per country
per_c_stacked <- per_c[,c("country", "n_news", "n")]
per_c_stacked$n <- per_c_stacked$n - per_c_stacked$n_news
per_c_long <- gather(per_c_stacked, number, count, n:n_news, factor_key = TRUE)
per_c_long <- per_c_long[order(per_c_long$country, per_c_long$number),]

percent_labs <- c("", paste(round(per_c$p_news[1], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[2], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[3], 0), "%", sep=""))

png(filename = "descriptives/n_participants.png", width=500, height=400)
ggplot(data = per_c_long, aes(fill = number, y = count, x = country)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  geom_text(aes(label = percent_labs), position = position_stack(), vjust=-0.5) +
  labs(title = "Number of participants per countries",
       fill = "") + 
  scale_fill_discrete(labels=c("Number of participants", "Number of participants with news consumption")) +
  xlab("Country") +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
dev.off()

# News consumtion
#per_c_type <- gather(per_c, category, percentage, p_news:p_social, factor_key = TRUE)
#ggplot(per_c_type, aes(x = category, y = percentage)) +
#  geom_bar(stat = "identity", fill = "red") +
#  labs(title = "Percetage of participants with different types of news consumption per countries") +
#  xlab(NULL) +
#  ylab(NULL) +
#  scale_x_discrete(breaks = c("p_news", "p_routine", "p_search", "p_social"),
#                   labels = c("All News Visits", "Routine", "Search Engine",
#                              "Social Network Sites")) +
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5),
#        panel.spacing = unit(1, "lines")) +
#  facet_wrap(. ~ country)

# Interested in politics
per_c_int <- per_c[,c("country", "p_polinterest", "p_voted")]
per_c_int <- gather(per_c_int, category, percentage,  p_polinterest:p_voted, factor_key = TRUE)

png(filename = "descriptives/interested_in_politics.png", width=500, height=400)
ggplot(per_c_int, aes(country, percentage)) +   
  geom_bar(aes(fill = category), position = "dodge", stat="identity", width = 0.7) +
  labs(title = "Percentage of participants who are voted and\nwho are interested in politics per countries",
       fill = "") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) + 
  scale_fill_discrete(labels=c("Interested in politics", "Voted")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
dev.off()

# Political orientation
per_c_pol <- gather(per_c_pol_ori, category, percentage, p_1:p_11, factor_key = TRUE)

png(filename = "descriptives/political_orientation.png", width=800, height=600)
ggplot(per_c_pol, aes(x = category, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Political orientation per countries") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("p_1", "p_6", "p_11"), labels = c("right", "middle", "left"),
                   limits = rev(levels(per_c_pol$category))) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 15, 30, 45, 60),
                     labels = c("0%", "15%", "30%", "45%", "60%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines")) +
  facet_wrap(. ~ country)
dev.off()

# Clusters
cluster_summary <- gather(clus, type, percentage, p_routine:p_unknown, factor_key = TRUE)

png(filename = "descriptives/cluster_news.png", width=1000, height=600)
ggplot(cluster_summary, aes(x = type, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "News Consumption Types per Clusters") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("p_routine", "p_search", "p_social", "p_unknown"),
                   labels = c("Rotuine", "Search Engine", "Social Sites", "Unknown")) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80),
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_grid(. ~ Cluster)
dev.off()

# Cluster per countries
cluster_summary <- gather(clus_country, type, percentage, p_routine:p_unknown, factor_key = TRUE)

png(filename = "descriptives/cluster_countries_news.png", width=1000, height=600)
ggplot(cluster_summary, aes(x = type, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "News Consumption Types per Clusters and Countries") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("p_routine", "p_search", "p_social", "p_unknown"),
                   labels = c("Rotuine", "Search Engine", "Social Sites", "Unknown")) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80),
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(country ~ Cluster, ncol = n_clusters)
dev.off()

# Political orientation per clusters
png(filename = "descriptives/cluster_pol.png", width=600, height=500)
ggplot(clus_pol, aes(x = leftmidright.num, y = p_pol)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Political Orientation per Clusters") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("1", "6", "11"), labels = c("right", "middle", "left"),
                     limits = rev(levels(clus_pol$leftmidright.num))) +
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(. ~ Cluster, ncol = round(n_clusters/2, 0))
dev.off()

# Political orientation per clusters and countries
png(filename = "descriptives/cluster_countries_pol.png", width=1000, height=800)
ggplot(clus_pol_c, aes(x = leftmidright.num, y = p_pol)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Political Orientation per Clusters and Countries") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("1", "6", "11"), labels = c("right", "middle", "left"),
                   limits = rev(levels(clus_pol_c$leftmidright.num))) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50),
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(country ~ Cluster, ncol = n_clusters)
dev.off()

## Data tables
# News consumption types and interest in politics oritentation per countries and overall
per_c
total_news

# Political orientation per countries and overall
per_c_pol_ori
t_pol_ori

# News consumption types and interest in politics oritentation per clusters and per clusters and countries
clus
clus_country

# Political orientation per countries and overall
clus_pol
clus_pol_c
