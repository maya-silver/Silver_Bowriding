library(readr)
library(dplyr)
library(tidyr)

#load the data
enviro_bowdf <- read_csv("SharedData/enviro_bowdf.csv")

#Environmental modeling

surveybowmodel_df <- enviro_bowdf %>% drop_na("Depth", "Group Size", "First Five Activity", 
                                              "biopsy_day", "Survey_Length")
#REMOVING UNKNOWN AND OTHER from df and analysis
surveybowmodel_df$`First Five Activity` <- ifelse(surveybowmodel_df$`First Five Activity` == "UNKNOWN" 
                                                  | surveybowmodel_df$`First Five Activity` ==  "NO DATA"
                                                  | surveybowmodel_df$`First Five Activity` == "OTHER",
                                                  NA, surveybowmodel_df$`First Five Activity`)
surveybowmodel_df$`First Five Activity` <- as.factor(surveybowmodel_df$`First Five Activity`)
surveybowmodel_df$`First Five Activity` <- droplevels(surveybowmodel_df$`First Five Activity`)
surveybowmodel_df <- surveybowmodel_df %>% drop_na("First Five Activity")

#One model including all activity states except "OTHER" "UNKNOWN" and "NO DATA"
surveybowmodel <- glm(Bowride_in_survey ~ Depth + `Group Size` + 
                        as.factor(`First Five Activity`) + biopsy_day 
                      + Survey_Length, family = binomial (link = "logit"), data = surveybowmodel_df)

summary(surveybowmodel)
confint(surveybowmodel)
exp(surveybowmodel$coefficients) ##This is giving the odds ratio
exp(confint(surveybowmodel))
