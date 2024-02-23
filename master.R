## master

# load packages
library(readxl)
library(dplyr)
library(janitor)


## import
data <- read_excel("data/NMA_Data for Mikkel_19.02.2024.xlsx",
                   sheet = "Time points rows",
                   col_types = c("numeric", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                   na = "--") %>%
        janitor::clean_names()

# SD from SE intervention
missing_sd <- is.na(data$sd_intervention)
replacement_values <- data$se_intervention * sqrt(data$n_intervention)
data$sd_intervention[missing_sd] <- replacement_values[missing_sd]
data$se_intervention <- NULL

# SD from SE control
missing_sd <- is.na(data$sd_control)
replacement_values <- data$se_control * sqrt(data$n_control)
data$sd_control[missing_sd] <- replacement_values[missing_sd]
data$se_control <- NULL


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



