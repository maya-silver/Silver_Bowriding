## Raw data figures

library(readr)
library(ggplot2)
library(boot)

# load the data
bowdf <- read_csv("SharedData/bow_df20251211.csv")

# bootstrap function
propfun <- function(data, indices) {
  d <- data[indices, ]
  prop <- sum(d) / length(d)
  return(prop)
}

#### FIGURE 1

age_prop <- prop.table(table(bowdf$age_increment, bowdf$Bowride), margin = 1)

age_prop <- age_prop[c(1, 2, 5, 3, 4, 6), ] # re-order

table(bowdf$age_increment)

set.seed(286567440)

res <- list()

for (i in 1:nrow(age_prop)) {
  age_bin <- rownames(age_prop)[i]

  data <- bowdf[which(bowdf$age_increment == age_bin), ]

  boot_res <- boot(data = data[, "Bowride", drop = FALSE], statistic = propfun, R = 1000)
  x <- boot.ci(boot_res, 0.95, type = "bca")

  res[[i]] <- x$bca[4:5]
}

table(bowdf$age_increment)

df <- data.frame(
  Age = rownames(age_prop),
  value = age_prop[, 2],
  lwr = c(11 / 309, 559 / 6078, 837 / 6219, 1608 / 17027, 542 / 11102, 1 / 148),
  upr = c(28 / 309, 649 / 6078, 948 / 6219, 1756 / 17027, 638 / 11102, 10 / 148)
)

df$Age <- factor(df$Age, levels = df$Age)

pdf(file = "Figures/Figure1.pdf", height = 4, width = 4)
age <- ggplot(df) +
  theme_classic() +
  geom_bar(aes(x = Age, y = value), stat = "identity", fill = "grey") +
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), width = 0.3, colour = "black") +
  ylab("") +
  xlab("Age class (years)")
dev.off()

#### FIGURE 2

just_sexed <- bowdf[which(bowdf$Sex %in% c("FEMALE", "MALE")), ]

table(just_sexed$Sex, just_sexed$Bowride)

2066 / (2066 + 17519) <- 0.1054889

1590 / (1590 + 17814) <- 0.08194187

set.seed(286567440)

data <- just_sexed[just_sexed$Sex == "MALE", ]
# bootstrap
boot_res <- boot(data = data[, "Bowride", drop = FALSE], statistic = propfun, R = 1000)
boot.ci(boot_res, 0.95, type = "bca")

data <- just_sexed[just_sexed$Sex == "FEMALE", ]

boot_res <- boot(data = data[, "Bowride", drop = FALSE], statistic = propfun, R = 1000)
boot.ci(boot_res, 0.95, type = "bca")


df <- data.frame(
  Sex = c("FEMALE", "MALE"),
  value = c(0.1054889, 0.08194187),
  lwr = c(1983 / 19585, 1517 / 19404),
  upr = c(2152 / 19585, 1659 / 19404)
)

pdf(file = "Figures/Figure2.pdf", height = 4, width = 4)
sex <- ggplot(df) +
  theme_classic() +
  geom_bar(aes(x = Sex, y = value), stat = "identity", fill = "grey") +
  geom_errorbar(aes(x = Sex, ymin = lwr, ymax = upr), width = 0.3, colour = "black") +
  ylab("Proportion of observations with bowriding")
dev.off()

##### FIGURE 3

# load the data
enviro_bowdf <- read_csv("SharedData/enviro_bow_df20251211.csv")

depth_prop <- prop.table(table(enviro_bowdf$depth_class, enviro_bowdf$Bowride_in_survey), margin = 1)

# remove deepest depth class since only one obs and no bowriding
depth_prop <- depth_prop[c(1, 4, 5, 6, 2), ]

set.seed(286567440)

res <- list()

for (i in 1:nrow(depth_prop)) {
  depth_bin <- rownames(depth_prop)[i]

  data <- enviro_bowdf[which(enviro_bowdf$depth_class == depth_bin), ]

  boot_res <- boot(data = data[, "Bowride_in_survey", drop = FALSE], statistic = propfun, R = 1000)
  x <- boot.ci(boot_res, 0.95, type = "bca")

  res[[i]] <- x$bca[4:5]
}

table(enviro_bowdf$depth_class)

df <- data.frame(
  Depth = rownames(depth_prop),
  value = depth_prop[, 2],
  lwr = c(317 / 1707, 485 / 3096, 501 / 3794, 254 / 1926, 12 / 221),
  upr = c(381 / 1707, 566 / 3096, 587 / 3794, 316 / 1926, 29 / 221)
)

df$Depth <- factor(df$Depth, levels = df$Depth)

pdf(file = "Figures/Figure3.pdf", height = 4, width = 4)
depth <- ggplot(df) +
  theme_classic() +
  geom_bar(aes(x = Depth, y = value), stat = "identity", fill = "grey") +
  geom_errorbar(aes(x = Depth, ymin = lwr, ymax = upr), width = 0.3, colour = "black") +
  ylab("") +
  xlab("Water depth (m)")
dev.off()

# Combine raw data plots

pdf("Figures/RawDataFigures.pdf", height = 4.5, width = 12)
p <- ggarrange(sex, age, depth, ncol = 3, nrow = 1, 
               labels = c("a", "b", "c"))
p

dev.off()

