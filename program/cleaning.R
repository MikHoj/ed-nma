### ED NMA load and clean

## Import -------------------------------------------------------------------
data <- read_excel("data/data05032024clean.xlsx",
                   sheet = "data",
                   na = "--") %>%
                   janitor::clean_names()


## Cleaning ----------------------------------------------------------------
# Stack data set (one line per outcome)
data.ed <- data %>%
  select(id:ed_change_ciub) %>% 
  rename(questionaire = ques_edsymp)
names(data.ed) <- sub("^ed_", "", names(data.ed))
data.ed$outcome <- "ED symptoms"

data.bd <- data %>%
  select(id:n_post, ques_bodydis:bd_change_ciub) %>% 
  rename(questionaire = ques_bodydis,
         questionaire_note = bodydis_or_satis)
names(data.bd) <- sub("^bd_", "", names(data.bd))
data.bd$outcome <- "Body dissatifaction"

data.di <- data %>% select(id:n_post, ques_di:di_change_sd) %>% 
  rename(questionaire = ques_di)
names(data.di) <- sub("^di_", "", names(data.di))
data.di$outcome <- "Dieting"

data <- bind_rows(data.ed, data.bd, data.di)
data <- data[!apply(is.na(data[, c("post_mean", "post_sd", "change_mean", "change_sd", "change_mean_adjusted", "change_cilb", "change_ciub", "change_tvalue")]), 1, all), ]


# calculate SD from SE
for (var in c("pre", "post")) {
  missing_sd <- is.na(data[[paste0(var, "_sd")]])
  replacement_values <- data[[paste0(var, "_se")]] * sqrt(data[[paste0("n_", var)]])
  data[[paste0(var, "_sd")]][missing_sd] <- replacement_values[missing_sd]
  data[[paste0(var, "_se")]] <- NULL
}


# create study labels
data$studlab <- character(length(data$author))
for (i in 1:length(data$author)) {
  if (is.na(data$suffix[i])) {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i])
  } else {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i], data$suffix[i], sep = "")
  }
}


# replace mean change with adjusted mean change if only this is recorded
data$change_mean <- ifelse(!is.na(data$change_mean_adjusted), data$change_mean_adjusted, data$change_mean)

# calculate SD for mean change if mean change + confidence intervals is recorded
data$change_sd <- ifelse(
                          !is.na(data$change_cilb),
                          sqrt(data$n_post) * abs(data$change_ciub - data$change_cilb) / 3.92,
                          data$change_sd
                          )

# calculate mean change from pre/post means
data$change_mean <- ifelse(is.na(data$change_mean), data$post_mean - data$pre_mean, data$change_mean)

# calculate standard deviation of mean difference from pre/post SDs
data$change_sd <- ifelse(is.na(data$change_sd),
                          sqrt(data$pre_sd^2 + data$post_sd^2 - 2 * 0.5 * data$pre_sd * data$post_sd),
                          data$change_sd
                          )

# calculate standard deviation of mean difference from t-value
data$change_sd <- ifelse(!is.na(data$change_tvalue),
                         abs(data$change_mean / data$change_tvalue) * sqrt(data$n_post),
                         data$change_sd
                         )


# fix intervention labels
labels <- read_excel("data/data05032024clean.xlsx", sheet = "labels")
data <- data %>% left_join(labels, by = "approach_mod")

# split data set
data$combination <- paste0(data$outcome, " ", data$preventiontype)
data <- data %>% select(
  id, studlab,
  preventiontype, label, outcome, combination,
  questionaire, questionaire_note,
  analysis, analysis_notes,
  n_pre, n_post,
  change_mean, change_sd
)
split_data <- split(data, data$combination)

# save data sets
saveRDS(split_data, file = "data/datasets.rds")
rm("data.ed", "data.bd", "data.di", "data", "split_data")
rm("i", "missing_sd", "replacement_values", "var")
