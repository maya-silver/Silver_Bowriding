library(readr)
library(dplyr)
library(tidyr)

# load the data
enviro_bowdf <- read_csv("SharedData/enviro_bow_df20251211.csv")

# Environmental modeling

surveybowmodel_df <- enviro_bowdf %>% drop_na(
  "Depth", "Group Size", "First Five Activity",
  "biopsy_day", "Survey_Length"
)
# REMOVING UNKNOWN AND OTHER from df and analysis
surveybowmodel_df$`First Five Activity` <- ifelse(surveybowmodel_df$`First Five Activity` == "UNKNOWN" |
  surveybowmodel_df$`First Five Activity` == "NO DATA" |
  surveybowmodel_df$`First Five Activity` == "OTHER",
NA, surveybowmodel_df$`First Five Activity`
)
surveybowmodel_df$`First Five Activity` <- as.factor(surveybowmodel_df$`First Five Activity`)
surveybowmodel_df$`First Five Activity` <- droplevels(surveybowmodel_df$`First Five Activity`)
surveybowmodel_df <- surveybowmodel_df %>% drop_na("First Five Activity")

# One model including all activity states except "OTHER" "UNKNOWN" and "NO DATA"
surveybowmodel <- glm(Bowride_in_survey ~ Depth + `Group Size` +
  `First Five Activity` + biopsy_day
  + Survey_Length, family = binomial(link = "logit"), data = surveybowmodel_df)

summary(surveybowmodel)
confint(surveybowmodel)
exp(surveybowmodel$coefficients) ## This is giving the odds ratio
exp(confint(surveybowmodel))

# Create figure

library(mvtnorm)

set.seed(286567440)
ndraws <- 100 # try 100, 500, 1000 (can also spread further (9 -> 12))
split <- 100 # number of x's for prediction

beta1 <- coef(surveybowmodel)
var1 <- vcov(surveybowmodel)
BETA1 <- rmvnorm(ndraws, beta1, var1) # Taking 100 draws of the posterior vector
xx1 <- seq(0, 72, length.out = split) # The x-range you want to use for plotting later

cib <- 0.05 # Define level for CI
lb <- round((ndraws * cib) / 2) # lb and ub define which predictions to be plotted
ub <- ndraws - lb # >and are based on "cib"

aggregate(Depth ~ `First Five Activity`, data = surveybowmodel_df, mean)

mean(surveybowmodel$model$Depth)
mean(surveybowmodel$model$Survey_Length)

# intercept, depth, group size, activity, biopsy, survey length

forX1 <- cbind(1, 6.53, xx1, 0, 0, 0, 0, 10.5) # forage (reference level)
socX1 <- cbind(1, 5.92, xx1, 0, 1, 0, 0, 10.5) # social
travX1 <- cbind(1, 5.69, xx1, 0, 0, 1, 0, 10.5) # travel

series <- list(forX1, socX1, travX1)

colVal <- matrix(c(
  160, 32, 240,
  0, 178, 0,
  140, 140, 140
), nrow = 3, byrow = TRUE)


# make base graphic
windows()
# pdf(file="Figures/model2_groupsize.pdf")
par(mar = c(4.2, 4.2, 1, 1))
plot(Bowride_in_survey ~ `Group Size`,
  xlab = "Group Size",
  ylab = "Predicited probability of bowriding in group",
  yaxt = "n",
  # xlim=c(0,72),
  type = "n",
  data = surveybowmodel_df,
  cex.lab = 1.25,
  cex.axis = 1.25
)

axis(2, las = 1, cex.axis = 1.25)

for (j in 1:3) {
  X1 <- series[[j]]
  y.lat1 <- BETA1 %*% t(X1)
  y.lat2 <- t(apply(y.lat1, 1, sort)) # Sorting to cut off alpha % y.lat2 <- y.lat1

  y.pred <- 1 / (1 + exp(-y.lat2)) # this is logit link

  lcol <- colVal[j, ]

  lines(xx1, colMeans(y.pred), col = rgb(lcol[1], lcol[2], lcol[3], 255, maxColorValue = 255), lwd = 3)

  for (i in lb:ub) {
    points(xx1, jitter(y.pred[i, ], factor = 4),
      type = "l",
      col = rgb(lcol[1] * 2, lcol[2] * 2, lcol[3] * 2,
        (255 * 2 - abs(round(ndraws / 2) - i)) / 12,
        maxColorValue = 255 * 2
      ),
      lwd = 2
    )
  }
}

# Add real data points
points(Bowride_in_survey ~ `Group Size`,
  pch = 16, col = rgb(0, 0, 0, 20, maxColorValue = 255),
  data = surveybowmodel_df
)

legend(50, 0.20,
  col = c(
    rgb(colVal[1, 1], colVal[1, 2], colVal[1, 3], 255, maxColorValue = 255),
    rgb(colVal[2, 1], colVal[2, 2], colVal[2, 3], 255, maxColorValue = 255),
    rgb(colVal[3, 1], colVal[3, 2], colVal[3, 3], 255, maxColorValue = 255)
  ),
  legend = c("Forage", "Social", "Travel"), lty = 1, lwd = 3, bty = "n", cex = 1.25
)

dev.off()
