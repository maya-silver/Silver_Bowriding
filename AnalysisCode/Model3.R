library(readr)
library(tidyr)
library(MCMCglmm)

#load the data
bowdf <- read_csv("SharedData/bowdf.csv")

#MCMCglmm modeling
mcmc_bowdf <- bowdf[, c("Observation ID", "Group Size", "Dolphin ID", "Mother ID", "Sex", "Depth", "Survey_Length", "biopsy_day",
                        "Age at Observation", "num_surveys", "Bowride", "pregnant", "mature", "cycling", "lactating")]

#format
mcmc_bowdf$animal <- as.factor(mcmc_bowdf$`Dolphin ID`)
mcmc_bowdf$mother <- as.factor(mcmc_bowdf$`Mother ID`)
mcmc_bowdf$Sex <- ifelse(mcmc_bowdf$Sex == "FEMALE", 1, 0)
mcmc_bowdf$Age <- mcmc_bowdf$`Age at Observation`
mcmc_bowdf$logsurveys <- log(mcmc_bowdf$num_surveys)
mcmc_bowdf <- mcmc_bowdf %>% drop_na("Age", "Sex", "Survey_Length", "biopsy_day", "Group Size", "Depth")
mcmc_bowdf <- mcmc_bowdf[which(mcmc_bowdf$Depth != 0),]

# Set priors (default weak for fixed effects and random effect variance fixed at 1)
# Based on coursenotes recommendations (p. 49, 131)
# http://cran.nexr.com/web/packages/MCMCglmm/vignettes/CourseNotes.pdf


# prior for threshold model (de Villemereuil p. 35)
prior.f1 <- list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))


#Model with just Individual ID (animal) as random effect

mcmc_bowdf <- as.data.frame(mcmc_bowdf) #remove warnings about tibbles

set.seed(286567440) # or skip to line 56 to load in full model results

start <- Sys.time()

mcmc_bow_mod1  <- MCMCglmm(Bowride ~ Age + I(Age^2) + Sex + Depth + Survey_Length + 
                             biopsy_day + `Group Size`,
                           family = "threshold",
                           random = ~animal, data = mcmc_bowdf,
                           prior = prior.f1, 
                           nitt = 100000, burnin = 10000, 
                           thin = 10,
                           verbose = TRUE)

end <- Sys.time()

end - start

#save(mcmc_bow_mod1, file = "IntermediateData/mcmc_bow_mod1_20250811.RData")

#Load pre-run model output stored in Intermediate Data
load("IntermediateData/mcmc_bow_mod1_20250811.RData")
summary(mcmc_bow_mod1)

#Calculate the intraclass correlation coefficient (ICC)
ICC <- mcmc_bow_mod1[["VCV"]][ , "animal"] / rowSums(mcmc_bow_mod1[["VCV"]])

mean(ICC)
HPDinterval(ICC)
