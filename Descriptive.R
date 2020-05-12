source("read_data.R")
source("Classification_news_visits.R")
source("ISCED_coding.R")
source("data_prep.R")

############################
## Visits data
#all_cat <- read.csv("predicted_cats/Number_of_visits_all_Category.csv")

## Data preparation

n_visits <- all_visits %>%
  group_by(pseudonym) %>%
  summarise(n_desktop = sum(d_kind == "desktop"),
            n_mobile = sum(d_kind == "mobile"),
            n_app = sum(d_kind == "app"),
            n_news_desktop = sum(d_kind == "desktop" & news == 1),
            n_news_mobile = sum(d_kind == "mobile" & news == 1),
            n_news_app = sum(d_kind == "app" & news == 1))
n_mobile_only <- length(which(n_visits$n_desktop == 0 & (n_visits$n_app > 0 | n_visits$n_mobile > 0)))
n_desktop_only <- length(which(n_visits$n_desktop > 0 & n_visits$n_app == 0 & n_visits$n_mobile == 0))
n_both <- length(which(n_visits$n_desktop > 0 & (n_visits$n_app > 0 | n_visits$n_mobile > 0)))
n_part <- nrow(n_visits)

desc_summary <- c(n_part, n_desktop_only, n_mobile_only, n_both, sum(n_visits$n_news_desktop),
                  (sum(n_visits$n_news_mobile) + sum(n_visits$n_news_app)))
names(desc_summary) <- c("n_participants", "n_desktop_only", "nmobile_only", "n_both", "n_news_desktop",
                         "n_news_mobile")


# Number of news visits in the categories
news_exp <- function(data)
{
  news_con <- merge(exp_dat, data, by = "panelist_id", all.x = TRUE)
  start_col <- which(colnames(news_con) == "total_news_visit")
  end_col <- ncol(news_con)
  news_con[,start_col:end_col] <- news_con[,start_col:end_col] %>%
    replace(., is.na(.), 0)
  return(news_con)
}
cat_news <- function(data, kind)
{
  cat <- c(colSums(data[,c("routine", "search", "social", "unknown")]))
  cat <- as.data.frame(cbind(names(cat), cat))
  colnames(cat) <- c("Mode", "Number")
  cat$Number <- as.numeric(as.character(cat$Number))
  cat$kind <- kind
  return(cat)
}

# desktop
news_con_d <- news_exp(per_part)
cat_n_d <- cat_news(news_con_d, "Desktop")

# mobile
news_con_m <- news_exp(per_part_mob)
cat_n_m <- cat_news(news_con_m, "Mobile")

# total
#news_con_t <- news_exp(all_cat)
#cat_n_t <- cat_news(news_con_t, "Total")

#cat_n <- rbind(cat_n_d, cat_n_m, cat_n_t)

#news_con <- merge(exp_dat, all_cat, by = "panelist_id", all.x = TRUE)

# Descriptive statistics per country
per_c_f <- function(data, kind)
{
  res <- data %>%
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
              n_polinterest = sum(polinterest == 1))
  percentage <- (res[,which(grepl("n_", colnames(res)))]/res$n) * 100
  colnames(percentage) <- paste("p_", str_remove(colnames(percentage), "n_"), sep="")
  res <- cbind(res, percentage)
  res$kind <- kind
  return(res)
}
# desktop
per_c_d <- per_c_f(news_con_d, "Desktop")

# mobile
per_c_m <- per_c_f(news_con_m, "Mobile")

# total
per_c_t <- per_c_f(news_con_t, "Total")

per_c <- rbind(per_c_d, per_c_m, per_c_t)

# Descriptive statistics overall
overall <- function(data, kind)
{
  res <- data %>%
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
  percentage <- (res[,which(grepl("n_", colnames(res)))]/res$n) * 100
  colnames(percentage) <- paste("p_", str_remove(colnames(percentage), "n_"), sep="")
  res <- cbind(res, percentage)
  res$kind <- kind
  return(res)
}

# desktop
total_news_d <- overall(news_con_d, "Desktop")

# mobile
total_news_m <- overall(news_con_m, "Mobile")

# total
total_news_t <- overall(news_con_t, "Total")

total_news <- rbind(total_news_d, total_news_m, total_news_t)

# Political orientation per country
leftmidright <- as.data.frame(exp_dat %>%
                                group_by(country, leftmidright.num) %>%
                                summarise(n = n()))
per_c_leftmidright <- leftmidright %>%
  spread(leftmidright.num, n)
percentage_leftmidright <- (per_c_leftmidright[,2:12]/rowSums(per_c_leftmidright[,2:12])) * 100
colnames(percentage_leftmidright) <- paste("p_", colnames(percentage_leftmidright), sep = "")
colnames(per_c_leftmidright)[-1] <- paste("n_", colnames(per_c_leftmidright)[-1], sep = "")
per_c_pol_ori <- cbind(per_c_leftmidright, percentage_leftmidright)

# Political orientation overall
t_leftmidright <- as.data.frame(exp_dat %>%
                                group_by(leftmidright.num) %>%
                                summarise(n = n()))
t_leftmidright <- t_leftmidright %>%
  spread(leftmidright.num, n)
t_percentage_leftmidright <- (t_leftmidright/sum(t_leftmidright)) * 100
colnames(t_percentage_leftmidright) <- paste("p_", colnames(t_percentage_leftmidright), sep = "")
colnames(t_leftmidright) <- paste("n_", colnames(t_leftmidright), sep = "")
t_pol_ori <- cbind(t_leftmidright, t_percentage_leftmidright)

pol_ori <- rbind(per_c_pol_ori, cbind(country = "Total", t_pol_ori))

# Prepare data visualization
# Sociodemographical variables
socio_prep <- function(y)
{
  x <- exp_dat$country
  tmp <- as.data.frame(cbind(table(x, y)))
  rownames(tmp) <- gsub('\\d+: ', '', rownames(tmp))
  tmp <- cbind(rownames(tmp), tmp)
  colnames(tmp)[1] <- "country"
  tmp <- gather(tmp, category_group, value, 2:ncol(tmp), factor_key = TRUE)
  tmp
}
soc_vars <- apply(exp_dat[,c("age_class", "gender", "children", "income", "family", "ISCED")], 2, socio_prep)
soc_vars <- dplyr::bind_rows(soc_vars, .id = "variable")

soc_vars$variable[soc_vars$variable == "age_class"]  <- "age"

soc_vars_t <- soc_vars %>%
  group_by(variable, category_group) %>%
  summarise(value = sum(value))
soc_vars_t_var <- soc_vars %>%
  group_by(variable) %>%
  summarise(value = sum(value))
soc_vars_t$country <- as.factor("Total")
soc_vars_t <- soc_vars_t[,c("variable", "country", "category_group", "value")]
soc_vars <- rbind(as.data.frame(soc_vars), as.data.frame(soc_vars_t))

soc_vars$percentage <- 0
soc_vars$percentage[which(soc_vars$country == "UK")] <-
  100 * soc_vars$value[which(soc_vars$country == "UK")]/per_c_d$n[which(per_c_d$country == "UK")]
soc_vars$percentage[which(soc_vars$country == "France")] <-
  100 * soc_vars$value[which(soc_vars$country == "France")]/per_c_d$n[which(per_c_d$country == "France")]
soc_vars$percentage[which(soc_vars$country == "Germany")] <-
  100 * soc_vars$value[which(soc_vars$country == "Germany")]/per_c_d$n[which(per_c_d$country == "Germany")]
soc_vars$percentage[which(soc_vars$country == "Total")] <-
  100 * soc_vars$value[which(soc_vars$country == "Total")]/total_news$n[1]

# Political variables
pol_vars <- apply(exp_dat[,c("reg_vote", "voted", "change", "undecided", "polinterest.num", "leftmidright.num",
                               "trust.EP", "trust.nat.pol")], 2, socio_prep)
pol_vars <- dplyr::bind_rows(pol_vars, .id = "variable")

pol_vars_t <- pol_vars %>%
  group_by(variable, category_group) %>%
  summarise(value = sum(value))
pol_vars_t_var <- pol_vars %>%
  group_by(variable) %>%
  summarise(value = sum(value))
pol_vars_t$country <- as.factor("Total")
pol_vars_t <- pol_vars_t[,c("variable", "country", "category_group", "value")]
pol_vars <- rbind(as.data.frame(pol_vars), as.data.frame(pol_vars_t))

pol_vars$percentage <- 0
pol_vars$percentage[which(pol_vars$country == "UK")] <-
  100 * pol_vars$value[which(pol_vars$country == "UK")]/per_c_d$n[which(per_c_d$country == "UK")]
pol_vars$percentage[which(pol_vars$country == "France")] <-
  100 * pol_vars$value[which(pol_vars$country == "France")]/per_c_d$n[which(per_c_d$country == "France")]
pol_vars$percentage[which(pol_vars$country == "Germany")] <-
  100 * pol_vars$value[which(pol_vars$country == "Germany")]/per_c_d$n[which(per_c_d$country == "Germany")]
pol_vars$percentage[which(pol_vars$country == "Total")] <-
  100 * pol_vars$value[which(pol_vars$country == "Total")]/total_news$n[1]

## Data visualization
dir.create("descriptives", showWarnings = FALSE)

# Sociodemographics
plot_socio <- function(varname, title, limit, n_breaks)
{
  filepath <- paste("descriptives/sociodemo_", varname, ".png", sep = "")
  cat(filepath)
  plottitle <- paste("Distribution of Sociodemographichal variables per countries\n\n", title, sep = "")
  p <- ggplot(soc_vars[soc_vars$variable %in% varname,], aes(x = category_group, y = percentage)) +
    geom_bar(stat = "identity", fill = "red") +
    labs(title = plottitle) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(limits = c(0, limit), breaks = (5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5)),
                       labels = paste(5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5), "%", sep = "")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 17),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(1,1,1,2), "cm"),
          strip.text.x = element_text(size = 12)) +
    facet_wrap(.~ country, ncol = 2, scales = "free")
  ggsave(filename = filepath, plot = p, width=7, height=8)
}

plot_socio("children", "Children", 70, 5)
plot_socio("family", "Family", 70, 5)  
plot_socio("age", "Age", 45, 5)  
plot_socio("income", "Income", 45, 5)  
plot_socio("gender", "Gender", 80, 5)  
plot_socio("ISCED", "ISCED", 80, 5)  

# Number of participants per country
per_c_stacked <- per_c[,c("country", "n_news", "n", "kind")]
per_c_stacked$n <- per_c_stacked$n - per_c_stacked$n_news
per_c_long <- gather(per_c_stacked, number, count, n:n_news, factor_key = TRUE)
per_c_long <- per_c_long[order(per_c_long$country, per_c_long$number),]

percent_labs <- c("", paste(round(per_c$p_news[per_c$country == "UK" & per_c$kind == "Desktop"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "France" & per_c$kind == "Desktop"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "Germany" & per_c$kind == "Desktop"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "UK" & per_c$kind == "Mobile"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "France" & per_c$kind == "Mobile"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "Germany" & per_c$kind == "Mobile"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "UK" & per_c$kind == "Total"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "France" & per_c$kind == "Total"], 0), "%", sep=""), "",
                  paste(round(per_c$p_news[per_c$country == "Germany" & per_c$kind == "Total"], 0), "%", sep=""))

png(filename = "descriptives/n_participants.png", width=500, height=400)
ggplot(data = per_c_long, aes(fill = number, y = count, x = country)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  geom_text(aes(label = percent_labs), position = position_stack(), vjust=-0.5) +
  labs(title = "Participants with news consumption on\ndesktop, mobile or any of the two platforms per countries",
       fill = "") + 
  scale_fill_discrete(labels=c("Number of participants", "Number of participants with news consumption")) +
  xlab("Country") +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12)) +
  facet_wrap(. ~ kind, ncol = 3)
dev.off()

# News consumtion in the categories
cat_n_v <- c(colSums(news_con[,c("routine", "search", "social", "unknown")], na.rm = TRUE))
cat_n_v <- as.data.frame(cbind(names(cat_n_v), cat_n_v))
colnames(cat_n_v) <- c("Mode", "Number")
cat_n_v$Number <- as.numeric(as.character(cat_n_v$Number))

png(filename = "descriptives/number_of_webvisits_in_cat.png", width=400, height=300)
ggplot(as.data.frame(cat_n_v), aes(Mode, as.numeric(Number))) +
  geom_bar(stat = "identity", fill = "red", width = 0.6) +
  labs(title = "Number of news-visits on desktop\nin the 4 categories of news engagement modes") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("routine", "search", "social", "unknown"),
                   labels = c("Routine", "Search Engine", "Social Network Sites", "Unknown")) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))
dev.off()

# Political variables
plot_socio("children", "Children", 70, 5)

plot_pol <- function(varname, title, limit, n_breaks)
{
  filepath <- paste("descriptives/pol_", varname, ".png", sep = "")
  cat(filepath)
  plottitle <- paste("Distribution of Political variables per countries\n\n", title, sep = "")
  p <- ggplot(pol_vars[pol_vars$variable %in% varname,], aes(x = category_group, y = percentage)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = plottitle) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(limits = c(0, limit), breaks = (5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5)),
                       labels = paste(5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5), "%", sep = "")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 17),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(1,1,1,2), "cm"),
          strip.text.x = element_text(size = 12)) +
    facet_wrap(.~ country, ncol = 2, scales = "free")
  ggsave(filename = filepath, plot = p, width=7, height=8)
}

plot_pol("reg_vote", "Registered voter", 100, 5)
plot_pol("voted", "Voted", 90, 5)
plot_pol("change", "Changed mind about Vote", 60, 5)
plot_pol("undecided", "Undecided", 80, 5)
plot_pol("polinterest.num", "Interested in Politics (1 - Interested, 5 - Not interested)", 50, 5)
plot_pol("leftmidright.num", "Political orientation (1 - Right, 11 - Left)", 45, 4)
plot_pol("trust.EP", "Trust in EP", 30, 3)
plot_pol("trust.nat.pol", "Trust in the national parliament/other governmental institutions", 30, 3)

## Data tables
# News consumption types and interest in politics oritentation per countries and overall
per_c
total_news

# Political orientation per countries and overall
per_c_pol_ori
t_pol_ori

# Number of news visits in the categories
cat_n_v <- c(colSums(news_con[,c("routine", "search", "social", "unknown")], na.rm = TRUE))
cat_n_v <- as.data.frame(cbind(names(cat_n_v), cat_n_v))
colnames(cat_n_v) <- c("Mode", "Number")
cat_n_v$Number <- as.numeric(as.character(cat_n_v$Number))

# News consumtion in the categories
png(filename = "descriptives/number_of_webvisits_in_cat_visits.png", width=400, height=300)
ggplot(as.data.frame(cat_n), aes(Mode, as.numeric(Number))) +
  geom_bar(stat = "identity", fill = "red", width = 0.6) +
  labs(title = "Number of news-visits\nin the 4 categories of news engagement modes\nExtended with mobile data") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = c("routine", "search", "social", "unknown"),
                   labels = c("Routine", "Search Engine", "Social Network Sites", "Unknown")) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))
dev.off()
