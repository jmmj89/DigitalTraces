library(gridExtra)

source("cluster_multinominal_logistic.R")
source("visits_desktop_analysis.R")

dir.create("cluster_desc", showWarnings = FALSE)

c_desc <- merge(exp_dat, per_part_p[,c("panelist_id", "cluster")], by = "panelist_id")
c_desc_v <- merge(exp_dat, all_cat_p[,c("panelist_id", "cluster")], by = "panelist_id")

# Cluster summary
c_summary <- function(dat)
{
  c_stat <- dat %>%
    group_by(cluster) %>%
    summarise(n = n(),
              p_voted = sum(voted == "Yes")/n,
              p_changed = sum(change == "changed vote")/n,
              p_did_not_change = sum(change == "did not change")/n,
              polinterest_mean = mean(polinterest.num),
              polinterest_var = var(polinterest.num),
              leftmidright_mean = mean(leftmidright.num),
              leftmidright_var = var(leftmidright.num),
              trust_EP_mean = mean(trust.EP),
              trust_EP_var = var(trust.EP),
              trust_NP_mean = mean(trust.nat.pol),
              trust_NP_var = var(trust.nat.pol))
  c_stat$country <- "Total"
  c_stat <- c_stat[, c(1, ncol(c_stat), 2:(ncol(c_stat) - 1))]
  c_stat_country <- dat %>%
    group_by(cluster, country) %>%
    summarise(n = n(),
              p_voted = sum(voted == "Yes")/n,
              p_changed = sum(change == "changed vote")/n,
              p_did_not_change = sum(change == "did not change")/n,
              polinterest_mean = mean(polinterest.num),
              polinterest_var = var(polinterest.num),
              leftmidright_mean = mean(leftmidright.num),
              leftmidright_var = var(leftmidright.num),
              trust_EP_mean = mean(trust.EP),
              trust_EP_var = var(trust.EP),
              trust_NP_mean = mean(trust.nat.pol),
              trust_NP_var = var(trust.nat.pol))
  c_stat <- rbind(as.data.frame(c_stat), as.data.frame(c_stat_country))
  c_stat
}

cluster_table <- c_summary(c_desc)
cluster_table_visits <- c_summary(c_desc_v)
write.csv(cluster_table, "cluster_desc/cluster_desktop.csv")
write.csv(cluster_table_visits, "cluster_desc/cluster_visit.csv")

########################
## Data visualisation ##
########################

# Data preparation
pol_c_prep <- function(..., dat)
{
  tmp <- dat %>%
    group_by(cluster, country, ...) %>%
    summarise(n = n())
  tmp$country <- as.character(tmp$country)
  total <- dat %>%
    group_by(cluster, ...) %>%
    summarise(n = n())
  total$country <- "Total"
  total <- total[,c(1, 4, 2, 3)]
  tmp <- rbind(tmp, total)
  total_c <- tmp %>%
    group_by(cluster, country) %>%
    summarise(total = sum(n))
  tmp <- merge(tmp, total_c, by = c("cluster", "country"), all.x = TRUE)
  tmp <- cbind(tmp, "variable" = rep(colnames(tmp)[3], nrow(tmp)))
  colnames(tmp)[3] <- "value"
  tmp$p <- 100 * tmp$n/tmp$total
  tmp$country <- factor(tmp$country, levels = c("France", "Germany", "UK", "Total"))
  tmp$value <- factor(tmp$value, levels = unique(tmp$value))
  return(tmp)
}

################
# Desktop data #
################
pol_vars <- rbind(pol_c_prep(reg_vote, dat = c_desc), pol_c_prep(voted, dat = c_desc), pol_c_prep(change, dat = c_desc),
                  pol_c_prep(undecided, dat = c_desc), pol_c_prep(polinterest.num, dat = c_desc),
                  pol_c_prep(leftmidright.num, dat = c_desc), pol_c_prep(trust.EP, dat = c_desc), pol_c_prep(trust.nat.pol, dat = c_desc))

###########################
# Desktop and mobile data #
###########################

pol_vars_v <- rbind(pol_c_prep(reg_vote, dat = c_desc_v), pol_c_prep(voted, dat = c_desc_v), pol_c_prep(change, dat = c_desc_v),
                    pol_c_prep(undecided, dat = c_desc_v), pol_c_prep(polinterest.num, dat = c_desc_v),
                    pol_c_prep(leftmidright.num, dat = c_desc_v), pol_c_prep(trust.EP, dat = c_desc_v), pol_c_prep(trust.nat.pol, dat = c_desc_v))


cluster_plot_cat <- function(dat, limit, n_breaks, varname)
{
  p <- ggplot(dat, aes(x = value, y = p, fill = value)) +
    geom_bar(stat = "identity") +
    labs(title = varname) +
    #xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(limits = c(0, limit), breaks = (5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5)),
                       labels = paste(5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5), "%", sep = "")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0, size = 17),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(1,1,1,2), "cm"),
          strip.text.x = element_text(size = 12)) +
    facet_wrap(.~ cluster, ncol = 4) +
    scale_fill_brewer(palette="Set3")
  p
}

cluster_plot_cat <- function(dat, limit, n_breaks, title)
{
  p <- ggplot(dat, aes(x = value, y = p, fill = value)) +
    geom_bar(stat = "identity") +
    labs(title = title) +
    ylab(NULL) +
    scale_y_continuous(limits = c(0, limit), breaks = (5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5)),
                       labels = paste(5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5), "%", sep = "")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0, size = 13),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 8),
          axis.title = element_blank(),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(1,1,1,2), "cm"),
          strip.text.x = element_text(size = 8)) +
    facet_wrap(.~ cluster, ncol = 4) +
    scale_fill_brewer(palette="Set3")
  p
}

cluster_plot_num <- function(dat, limit, n_breaks, title)
{
  p <- ggplot(dat, aes(x = value, y = p, fill = value)) +
    geom_bar(stat = "identity") +
    labs(title = title) +
    ylab(NULL) +
    scale_y_continuous(limits = c(0, limit), breaks = (5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5)),
                       labels = paste(5 * round(seq(0, limit, by = limit/(n_breaks - 1))/5), "%", sep = "")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0, size = 13),
          axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8),
          axis.title = element_blank(),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(1,1,1,2), "cm"),
          strip.text.x = element_text(size = 8)) +
    facet_wrap(.~ cluster, ncol = 4) +
    scale_fill_brewer(palette="Set3")
  p
}

plottitle <- function(varname)
{
  if(varname == "reg_vote") title <- "Registered Voter"
  if(varname == "voted") title <- "Voted"
  if(varname == "change") title <- "Change"
  if(varname == "undecided") title <- "Undecided"
  if(varname == "polinterest.num") title <- "Interested in politics (1 - interested, 5 - not interested)"
  if(varname == "leftmidright.num") title <- "Political Orientation (1 - right, 11 - left)"
  if(varname == "trust.EP") title <- "Trust in European Parliament  (1 - no trust, 11 - complete trust)"
  if(varname == "trust.nat.pol") title <- "Trust in National Parliament  (1 - no trust, 11 - complete trust)"
  title
}

vis_c_pol <- function(..., dat, country)
{
  dat <- dat[dat$country == country,]
  plotlist <- list()
  for (i in 1:length(unique(dat$variable)))
  {
    act_var <- unique(dat$variable)[i]
    tmp <- dat[which(dat$variable == act_var),]
    limit <- round(max(tmp$p) + 5, -1)
    n_breaks <- round(limit/15, 0)
    title <- plottitle(act_var)
    if(is.na(as.numeric(as.character(tmp$value))))
    {
      plotlist[[i]] <- cluster_plot_cat(dat = dat[which(dat$variable == act_var),], limit, n_breaks, title = title)
    } else
    {
      plotlist[[i]] <- cluster_plot_num(dat = dat[which(dat$variable == act_var),], limit, n_breaks, title = title)
    }
  }
  plotlist
}

plot_clusters <- function(dat, cat_name)
{
  # France
  file_n <- paste("cluster_desc/France_", cat_name, ".png", sep = "")
  plots_France <- vis_c_pol(reg_vote, dat = dat, country = "France")
  png(filename = file_n, width=1100, height=1000)
  p_France <- do.call(grid.arrange, c(plots_France, ncol = 2, nrow  = 4))
  dev.off()
  # Germany
  file_n <- paste("cluster_desc/Germany_", cat_name, ".png", sep = "")
  plots_France <- vis_c_pol(reg_vote, dat = dat, country = "Germany")
  png(filename = file_n, width=1100, height=1000)
  p_France <- do.call(grid.arrange, c(plots_France, ncol = 2, nrow  = 4))
  dev.off()
  # UK
  file_n <- paste("cluster_desc/UK_", cat_name, ".png", sep = "")
  plots_France <- vis_c_pol(reg_vote, dat = dat, country = "UK")
  png(filename = file_n, width=1100, height=1000)
  p_France <- do.call(grid.arrange, c(plots_France, ncol = 2, nrow  = 4))
  dev.off()
  # Total
  file_n <- paste("cluster_desc/Total_", cat_name, ".png", sep = "")
  plots_France <- vis_c_pol(reg_vote, dat = dat, country = "Total")
  png(filename = file_n, width=1100, height=1000)
  p_France <- do.call(grid.arrange, c(plots_France, ncol = 2, nrow  = 4))
  dev.off()
}

plot_clusters(pol_vars, "desktop")
plot_clusters(pol_vars_v, "visit")



