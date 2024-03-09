### ed-nma master

## load packages
library(here)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(netmeta)
library(xlsx)

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

# calcualte standard deviation of mean difference from t-value
data$change_sd <- ifelse(!is.na(data$change_tvalue),
                         abs(data$change_mean / data$change_tvalue) * sqrt(data$n_post),
                         data$change_sd
                         )

# split data set
data$combination <- paste0(data$type_of_prevention, "_", data$outcome)
split_data <- split(data, data$combination)

# save data sets
saveRDS(split_data, file = "data/datasets.rds")


### Meta-analysis -----------------------------------------------------------

## Functions for output
# Pairwise comparison
pairmod <- function (x) {
  dts <- pairwise(treat = intervention_mod,
                n = pre_n,
                mean = diff_mean,
                sd = diff_sd,
                data = x,
                studlab = studlab,
                reference.group = "Control",
                sm = "SMD"
  )
  return(dts)
}

# Data set for inspection
datas <- function (x) {
  dts <- x %>% select(studlab, treat1, treat2, n1, mean1, sd1, diff_mean1, diff_sd1, n2, mean2, sd2, diff_mean2, diff_sd2, TE, seTE)
  return(dts)
}

# Netgraph
netgraphmod <- function (x) {
  ngr <- netgraph(x,
           seq = "optimal",
           plastic = FALSE,
           number.of.studies = TRUE,
           cex.points = n.trts,
           cex = 1.25
  )
  return(ngr)
}

# Forest plot
forestplotmod <- function (x) {
  forest(x,
       sortvar = TE,
       leftcols = c("studlab", "k"),
       leftlabs = c("Intervention", "Direct\nstudies"),
       rightcols = c("effect.ci", "Pscore"),
       rightlabs = c("SMD (95%CI)", "P-score")
  )
}



## Selective prevention ----------------------------------------------------
# Body dissatisfaction
ma.s.bd <- pairmod(split_data[["selective_Body_Dissatisfaction"]])
write.xlsx(datas(ma.s.bd), file = "output/data.xlsx", sheetName = "Selec_bodydis", append = TRUE)

net.s.bd <- netmeta(ma.s.bd,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/selective_bodydis_results.txt")
summary(net.s.bd, digits = 2)
sink()

png(filename = "output/selective_bodydis_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.s.bd)
dev.off()

png(filename = "output/selective_bodydis_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.s.bd)
dev.off()


# ED symptoms
ma.s.ed <- pairmod(split_data[["selective_ED_Symptoms"]])
write.xlsx(datas(ma.s.ed), file = "output/data.xlsx", sheetName = "Selec_edsymp", append = TRUE)

net.s.ed <- netmeta(ma.s.ed,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/selective_edsymp_results.txt")
summary(net.s.ed, digits = 2)
sink()

png(filename = "output/selective_edsymp_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.s.ed)
dev.off()

png(filename = "output/selective_edsymp_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.s.ed)
dev.off()


# Dieting
ma.s.di <- pairmod(split_data[["selective_Dieting"]])
write.xlsx(datas(ma.s.di), file = "output/data.xlsx", sheetName = "Selec_dieting", append = TRUE)

net.s.di <- netmeta(ma.s.di,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/selective_dieting_results.txt")
summary(net.s.di, digits = 2)
sink()

png(filename = "output/selective_dieting_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.s.di)
dev.off()

png(filename = "output/selective_dieting_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.s.di)
dev.off()


