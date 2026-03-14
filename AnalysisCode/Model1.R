library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# 1) Modeling the correlation between maternal and offspring proportions riding the bow

# load the data
bowdf <- read_csv("SharedData/bow_df20251211.csv")
matbowcor <- bowdf
# remove all surveys of individual bowriders less than 3 months old
matbowcor <- bowdf[bowdf$`Age at Observation` > 0.25, ]
# only instances of bowriding
matbowcor <- matbowcor[which(matbowcor$`Behavior State` == "BOW-BOWRIDING"), ]
# remove dolphins we've surveyed less than 10 times
matbowcor <- matbowcor[which(matbowcor$num_surveys > 10), ]
# Delete duplicated offspring ID so we have one datapoint for each offspring bow proportion
# and each maternal bow proportion
matbowcor <- matbowcor[!duplicated(matbowcor$`Dolphin ID`), ]
# reduce dataframe to only necessary variables
matbowcor <- matbowcor[, c("Dolphin ID", "Mother ID", "prop_on_bow", "mat_prop_on_bow", "Sex")]
# remove NA values for the maternal proportion on bow
matbowcor <- matbowcor %>% drop_na("mat_prop_on_bow")
# remove NA values for Sex
matbowcor <- matbowcor %>% drop_na("Sex")

# Linear Model
# Adding Sex as an interaction term to the model
maternalbowcor <- lm(prop_on_bow ~ mat_prop_on_bow + Sex + mat_prop_on_bow:Sex, data = matbowcor)
summary(maternalbowcor)
confint(maternalbowcor)
mat_off_cor <- cor(matbowcor$prop_on_bow, matbowcor$mat_prop_on_bow)
r2 <- summary(maternalbowcor)$adj.r.squared
r2_print <- round(r2, 3)

# plotting
# create the line from the maternalbowcor model and add to the plot instead of the geom_smooth

confint_MPOP <- expand.grid(
  mat_prop_on_bow = seq(min(matbowcor$mat_prop_on_bow), max(matbowcor$mat_prop_on_bow), length.out = 100),
  Sex = unique(matbowcor$Sex)
)

# Add predictions and confidence intervals
predictions <- predict(maternalbowcor, newdata = confint_MPOP, interval = "confidence", level = 0.95)
confint_MPOP <- cbind(confint_MPOP, predictions)

matbowcor$Sex <- as.factor(matbowcor$Sex)
confint_MPOP$Sex <- as.factor(confint_MPOP$Sex)

pdf(file = "Figures/Figure4.pdf", height = 5.4)
ggplot(matbowcor, aes(y = prop_on_bow, x = mat_prop_on_bow, color = Sex, shape = Sex)) +
  theme_classic() +
  theme(text = element_text(size = 14)) +
  geom_segment(x = 0, xend = max(confint_MPOP$mat_prop_on_bow), y = 0.08371857, yend = 0.5853105, color = "purple1") + # FEMALE
  geom_segment(x = 0, xend = max(confint_MPOP$mat_prop_on_bow), y = 0.08329162, yend = 0.4755792, color = "seagreen") + # MALE
  geom_point(size = 1.5) +
  geom_ribbon(
    data = confint_MPOP,
    aes(ymin = lwr, ymax = upr, x = mat_prop_on_bow, fill = Sex),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Maternal Bowriding Proportion",
    y = "Offspring Bowriding Proportion",
  ) +
  scale_color_manual(values = c("FEMALE" = "purple1", "MALE" = "seagreen")) +
  scale_fill_manual(values = c("FEMALE" = "purple1", "MALE" = "seagreen")) +
  scale_shape_manual(values = c("FEMALE" = 21, "MALE" = 17)) +
  annotate("text", x = 0.55, y = 0.1, label = parse(text = paste("R^2", "== ", r2_print)), size = 5, color = "black")
dev.off()
