vis_prep <- function(data, ...)
{
  n <- data[which(!is.na(data$Cluster)),] %>%
    group_by(leftmidright) %>%
    summarise(n = n())
  colnames(n) <- c("var", "n")
  
  n_c <- data[which(!is.na(data$Cluster)),] %>%
    group_by(..., country) %>%
    summarise(n = sum(!(is.na(Cluster))))
  colnames(n_c) <- c("var", "country", "n")
  
  tmp <- data[which(!is.na(data$Cluster)),] %>%
    group_by(Cluster, ...) %>%
    summarise(n_clus = n())
  tmp$country <- "Total"
  colnames(tmp) <- c("Cluster", "var", "n_clus", "country")
  tmp <- merge(tmp, n, by = "var")
  tmp$Percentage <- 100 * (tmp$n_clus/tmp$n)
  
  tmp_c <- data[which(!is.na(data$Cluster)),] %>%
    group_by(Cluster, ..., country) %>%
    summarise(n_clus = n())
  colnames(tmp_c) <- c("Cluster", "var", "country", "n_clus")
  tmp_c <- merge(tmp_c, n_c, by = c("var", "country"))
  tmp_c$Percentage <- (100* tmp_c$n_clus/tmp_c$n)

  tmp_c <- rbind(tmp_c, as.data.frame(tmp[,c(4, 2, 1, 3, 5, 6)]))
  tmp_c
}

# Political orientation
ori <- vis_prep(news_con, leftmidright)

# Visualisation
png(filename = "descriptives/cluster_pol.png", width=1000, height=800)
ggplot(ori, aes(x = Cluster, y = Percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Political Orientation - Clusters") +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 15, 30, 45, 60, 75),
                     labels = c("0%", "15%", "30%", "45%", "60%", "75%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(country ~ var, ncol = 3)
dev.off()

# Voted
voted <- vis_prep(news_con, voted)
# Visualisation
png(filename = "descriptives/cluster_vote.png", width=600, height=800)
ggplot(voted, aes(x = Cluster, y = Percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Voting Behaviour - Clusters") +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 15, 30, 45, 60, 75),
                     labels = c("0%", "15%", "30%", "45%", "60%", "75%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(country ~ var, ncol = 2)
dev.off()

# Interested in politics
polinterest <- vis_prep(news_con, polinterest)
polinterest$var <- recode(polinterest$var, "0" = "No", "1" = "Yes")
# Visualisation
png(filename = "descriptives/cluster_interested.png", width=600, height=800)
ggplot(polinterest, aes(x = Cluster, y = Percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Interested in politics - Clusters") +
  ylab(NULL) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 15, 30, 45, 60, 75),
                     labels = c("0%", "15%", "30%", "45%", "60%", "75%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1.5), "cm")) +
  facet_wrap(country ~ var, ncol = 2)
dev.off()
