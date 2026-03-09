# model 3 figure 

summary(mcmc_bow_mod1)

# Sex and Age

set.seed(286567440)
ndraws <- 100 # try 100, 500, 1000 (can also spread further (9 -> 12))
split <- 100 # number of x's for prediction

BETA1 <- as.data.frame(mcmc_bow_mod1$Sol)
BETA1 <- BETA1[sample(1:nrow(BETA1), ndraws),] |> as.matrix()

xx1 <- seq(0, 52, length.out = split) # The x-range you want to use for plotting later

cib <- 0.05 # Define level for CI
lb <- round((ndraws * cib) / 2) # lb and ub define which predictions to be plotted
ub <- ndraws - lb # >and are based on "cib"


mean(mcmc_bowdf$Depth)
mean(mcmc_bowdf$Survey_Length)
mean(mcmc_bowdf$`Group Size`)

# intercept, age, age2, sex, depth, survey length, biopsy, group size

maleX1 <- cbind(1, xx1, xx1^2, 0, 6.21, 15.01, 0, 7.48) # male
femaleX1 <- cbind(1, xx1, xx1^2, 1, 6.21, 15.01, 0, 7.48) # female

series <- list(maleX1, femaleX1)

colVal <- matrix(c(
  160, 32, 240,
  0, 178, 0), nrow = 2, byrow = TRUE)


# make base graphic
windows()
# pdf(file="Figures/model3_sexage.pdf")
par(mar = c(4.2, 4.2, 1, 1))
plot(Bowride ~ Age,
     xlab = "Age (yrs)",
     ylab = "Predicited probability of bowriding",
     yaxt = "n",
     # xlim=c(0,72),
     type = "n",
     data = mcmc_bowdf,
     cex.lab = 1.25,
     cex.axis = 1.25
)

axis(2, las = 1, cex.axis = 1.25)

for (j in 1:2) {
  X1 <- series[[j]]
  y.lat1 <- BETA1 %*% t(X1)
  y.lat2 <- y.lat1
  # y.lat2 <- t(apply(y.lat1, 1, sort)) # Sorting to cut off alpha % y.lat2 <- y.lat1
  
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
points(Bowride ~ Age,
       pch = 16, col = rgb(0, 0, 0, 20, maxColorValue = 255),
       data = mcmc_bowdf
)

legend(38, 0.40,
       col = c(
         rgb(colVal[1, 1], colVal[1, 2], colVal[1, 3], 255, maxColorValue = 255),
         rgb(colVal[2, 1], colVal[2, 2], colVal[2, 3], 255, maxColorValue = 255)
       ),
       legend = c("Male", "Female"), lty = 1, lwd = 3, bty = "n", cex = 1.25
)

dev.off()