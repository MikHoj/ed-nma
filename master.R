## master

# load packages
library(readxl)
library(dplyr)
library(janitor)


## import
data <- read_excel("data/NMA_Data for Mikkel_19.02.2024.xlsx",
                   sheet = "Time points rows",
                   na = "--") %>%
        janitor::clean_names()


## cleaning
baseline1 <- data %>%
        filter(timepoint == "Baseline") %>% 
        select(study_id:se_intervention) %>% 
        select(-control)

eot1      <- data %>%
        filter(timepoint == "EOT") %>% 
        select(study_id:se_intervention) %>% 
        select(-control) %>% 
        rename(
          post_n = n_intervention,
          post_mean = mean_intervention,
          post_sd = sd_intervention
        )

baseline1 <- data %>%
        filter(timepoint == "Baseline") %>% 
        select(study_id:se_intervention) %>% 
        select(-control)

eot1      <- data %>%
        filter(timepoint == "EOT") %>% 
        select(study_id:se_intervention) %>% 
        select(-intervention)



