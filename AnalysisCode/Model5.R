library(readr)
library(tidyr)
library(MCMCglmm)

#load the data
bowdf <- read_csv("SharedData/bow_df20251211.csv")

#format
preg_mod_df <- bowdf[, c("Observation ID", "Dolphin ID", "Mother ID", "Sex",
                         "Age at Observation", "Bowride", "pregnant", "mature", "cycling", "lactating")]
preg_mod_df <- preg_mod_df %>% drop_na("Sex")
preg_mod_df$animal <- as.factor(preg_mod_df$`Dolphin ID`)
preg_mod_df$mother <- as.factor(preg_mod_df$`Mother ID`)

#only females
preg_mod_df$Sex <- ifelse(preg_mod_df$Sex == "FEMALE", 1, 0)
preg_mod_df <- preg_mod_df[which(preg_mod_df$Sex == 1),]
#only mature females
preg_mod_df <- preg_mod_df[which(preg_mod_df$mature == 1),]
preg_mod_df <- as.data.frame(preg_mod_df)

#Prior for threshold model (de Villemereuil p. 35)
prior.f1 <- list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))

set.seed(286567440)

#run the model
preg_mod  <- MCMCglmm(Bowride ~ pregnant + cycling + lactating, 
                      family = "threshold",
                      random = ~animal , data = preg_mod_df,
                      prior = prior.f1, nitt = 100000, burnin = 10000, 
                      thin = 10, verbose = TRUE)

#save(preg_mod, file = "IntermediateData/preg_mod_20260306.RData")
#Load pre-run preg_mod output
load("IntermediateData/preg_mod_20260306.RData")

summary(preg_mod)

# Create figure of posterior distribution

set.seed(286567440)

BETA1 <- as.data.frame(preg_mod$Sol)

windows()
# pdf(file="Figures/model5.pdf")
par(mfrow = c(3,1), 
    mar = c(4.2, 4.2, 1, 1))
#Pregnant
hist(BETA1$pregnant, probability = TRUE, breaks = 20,
     ylim = c(0, 10), xlim = c(-0.4, 0.4), border = NA, 
     main = NA, xlab = "Pregnant", col = NA, yaxt = "none")
axis(2, las = 1)
dens <- density(BETA1$pregnant)

lower_bound <- quantile(BETA1$pregnant, probs = 0.025)
upper_bound <- quantile(BETA1$pregnant, probs = 0.975)
x_shade <- dens$x[dens$x >= lower_bound & dens$x <= upper_bound]
y_shade <- dens$y[dens$x >= lower_bound & dens$x <= upper_bound]
polygon(c(lower_bound, x_shade, upper_bound), c(0, y_shade, 0), 
        col = "lavender", border = NA)
lines(dens) 
points(mean(BETA1$pregnant), 0, cex = 2, pch = 15)

#Cycling
hist(BETA1$cycling, probability = TRUE, breaks = 20,
     ylim = c(0, 10), xlim = c(-0.4, 0.4), border = NA, 
     main = NA, xlab = "Cycling", col = NA, yaxt = "none")
axis(2, las = 1)
dens <- density(BETA1$cycling)

lower_bound <- quantile(BETA1$cycling, probs = 0.025)
upper_bound <- quantile(BETA1$cycling, probs = 0.975)
x_shade <- dens$x[dens$x >= lower_bound & dens$x <= upper_bound]
y_shade <- dens$y[dens$x >= lower_bound & dens$x <= upper_bound]
polygon(c(lower_bound, x_shade, upper_bound), c(0, y_shade, 0), 
        col = "darkseagreen2", border = NA)
lines(dens) 
points(mean(BETA1$cycling), 0, cex = 2, pch = 15) 

#Lactating
hist(BETA1$lactating, probability = TRUE, breaks = 20,
     ylim = c(0, 10), xlim = c(-0.4, 0.4), border = NA, 
     main = NA, xlab = "Lactating", col = NA, yaxt = "none")
axis(2, las = 1)
dens <- density(BETA1$lactating)

lower_bound <- quantile(BETA1$lactating, probs = 0.025)
upper_bound <- quantile(BETA1$lactating, probs = 0.975)
x_shade <- dens$x[dens$x >= lower_bound & dens$x <= upper_bound]
y_shade <- dens$y[dens$x >= lower_bound & dens$x <= upper_bound]
polygon(c(lower_bound, x_shade, upper_bound), c(0, y_shade, 0), 
        col = "skyblue", border = NA)
lines(dens) 
points(mean(BETA1$lactating), 0, cex = 2, pch = 15)

dev.off()
