## master

# load packages
library(here)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(netmeta)

## import
data <- read_excel("data/NMA_Data for Mikkel_19.02.2024 copy.xlsx", 
                   sheet = "Sheet1",
                   col_types = c("numeric","text", "numeric", "text", "text",
                                 "text", "text", "numeric", "skip", "skip",
                                 "numeric", "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric","numeric", "numeric",
                                 "numeric", "numeric", "numeric"),
                   na = "--") %>%
                   janitor::clean_names() %>%
                   filter(outc == 1) %>% 
                   select(-starts_with("mean_change_"))
# data <- readxl::read_excel("data/NMA_Data for Mikkel_19.02.2024 copy.xlsx",
#                    sheet = "Time points rows",
#                    col_types = c("numeric", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
#                    na = "--") %>%
#         janitor::clean_names() %>% 
#         select(study_id:sd_control)

# SD from SE intervention
missing_sd <- is.na(data$pre_sd)
replacement_values <- data$pre_se * sqrt(data$pre_n)
data$pre_sd[missing_sd] <- replacement_values[missing_sd]
data$pre_se <- NULL

missing_sd <- is.na(data$post_sd)
replacement_values <- data$post_se * sqrt(data$post_n)
data$post_sd[missing_sd] <- replacement_values[missing_sd]
data$post_se <- NULL


## cleaning
# Study labels
data$studlab <- character(length(data$author))
for (i in 1:length(data$author)) {
  if (is.na(data$suffix[i])) {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i])
  } else {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i], data$suffix[i], sep = "")
  }
}

# Replace different types of control with just "Control"
data$intervention <- sub("^Control_.*", "Control", data$intervention)

# Calculate mean difference (MD)
data$diff_mean <- data$post_mean - data$pre_mean

# Calculate standard deviation of mean difference (SDMD)
data$diff_sd <- sqrt(data$pre_sd^2 + data$post_sd^2 - 2 * 0.5 * data$pre_sd * data$post_sd)


## split data set
split_data <- split(data, interaction(data$outcome, data$type_of_prevention))

# Create a unique identifier for each combination of outcome and type_of_prevention
data$combination <- paste(data$outcome, data$type_of_prevention, sep = "_")

# Split data by the unique combination identifier
split_data <- split(data, data$combination)

## meta-analysis
data.bd <- data %>% filter(outcome == "Body_Dissatisfaction")
ma.bd <- pairwise(treat = intervention,
           n = pre_n,
           mean = diff_mean,
           sd = diff_sd,
           data = data.bd,
           studlab = studlab,
           reference.group = "Control"
           )

net.bd <- netmeta(ma.bd,
                  sm = "SMD",
                  ref = "Control",
                  comb.fixed = FALSE,
                  comb.random = TRUE
                  )

summary(net.bd, digits = 2)

netgraph(net.bd)

forest(net.bd,
       sortvar = TE,
       leftcols = c("studlab", "k"),
       leftlabs = c("Intervention", "Direct\nstudies"),
       xlab = "Response to treatment",
       smlab = "NMA random effects"
       )

netrank(net.bd, small = "bad")

leaguetable <- netleague(net.bd, digits = 2)

# intervention <- data %>% 
#           select(-control, -n_control:-sd_control) %>% 
#           rename(arm = intervention)
# control <- data %>% 
#           select(-intervention, -n_intervention:-sd_intervention) %>% 
#           rename(arm = intervention)
# 
# intervention1 <- intervention %>%
#           filter(timepoint == "Baseline") %>% 
#           select(-timepoint, -weeks) %>% 
#           rename(
#             pre_n = n_intervention,
#             pre_mean = mean_intervention,
#             pre_sd = sd_intervention
#           )
# 
# intervention2 <- intervention %>%
#           filter(timepoint == "EOT") %>%
#           select(-timepoint) %>% 
#           rename(
#             post_n = n_intervention,
#             post_mean = mean_intervention,
#             post_sd = sd_intervention
#           )
# 
# intervention3 <- intervention1 %>%
#           left_join(intervention2,
#                     by = ("study_id", "author", "year", "")
#                     )
# 
# control1 <- control %>% filter(timepoint == "Baseline") %>% select(-timepoint)
# control2 <- control %>% filter(timepoint == "EOT") %>% select(-timepoint)
# 
# baseline1 <- data %>%
#         filter(timepoint == "Baseline") %>% 
#         select(study_id:control, n_intervention:sd_intervention) %>% 
#         rename(
#           n_pre = n_intervention,
#           mean_pre = mean_intervention,
#           sd_pre = sd_intervention
#         )
# 
# eot1      <- data %>%
#         filter(timepoint == "EOT") %>% 
#         select(study_id:control, n_intervention:sd_intervention) %>% 
#         rename(
#           n_post = n_intervention,
#           mean_post = mean_intervention,
#           sd_post = sd_intervention
#         )
# 
# 
# data1 <- left_join(baseline1, eot1, by = join_by(study_id, author, year, type_of_prevention, outcome, intervention, control))
# 
# baseline1 <- data %>%
#         filter(timepoint == "Baseline") %>% 
#         select(study_id:se_intervention) %>% 
#         select(-control)
# 
# eot1      <- data %>%
#         filter(timepoint == "EOT") %>% 
#         select(study_id:se_intervention) %>% 
#         select(-intervention)



## meta-analysis
# net <- netmeta(effect, se,
#                treat1, treat2, study,
#                data = xx,
#                sm = "SMD",
#                ref = "PBO",
#                comb.fixed = FALSE,
#                comb.random = TRUE,
#                tol.multiarm = 0.075
#                )
# 
# 
# summary(net)
# forest(net, sortvar = TE)
# netrank(net)