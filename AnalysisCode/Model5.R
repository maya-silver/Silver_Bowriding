library(readr)
library(dplyr)
library(tidyr)
library(MCMCglmm)

#load the data
bowdf <- read_csv("SharedData/bowdf")

#format
preg_mod_df <- bowdf[, c("Observation ID", "Dolphin ID", "Mother ID", "Sex",
                         "Age at Observation", "Bowride", "pregnant", "mature", "cycling", "lactating")]
preg_mod_df <- preg_mod_df %>% drop_na("Sex")
preg_mod_df$animal <- as.numeric(as.factor(preg_mod_df$`Dolphin ID`))
preg_mod_df$mother <- as.numeric(as.factor(preg_mod_df$`Mother ID`))

#only females
preg_mod_df$Sex <- ifelse(preg_mod_df$Sex == "FEMALE", 1, 0)
preg_mod_df <- preg_mod_df[which(preg_mod_df$Sex == 1),]
#only mature females
preg_mod_df <- preg_mod_df[which(preg_mod_df$mature == 1),]
preg_mod_df <- as.data.frame(preg_mod_df)

#run the model
preg_mod  <- MCMCglmm(Bowride ~ pregnant + cycling + lactating, 
                      family = "threshold",
                      random = ~animal , data = preg_mod_df,
                      prior = prior.f1, nitt = 100000, burnin = 10000, 
                      thin = 10, verbose = TRUE)

#save(preg_mod, file = "IntermediateData/preg_mod.RData")
#Load pre-run preg_mod output
load("IntermediateData/preg_mod_20250328.RData")

summary(preg_mod)
