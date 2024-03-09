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


## Cleaning ----------------------------------------------------------------
# create study labels
data$studlab <- character(length(data$author))
for (i in 1:length(data$author)) {
  if (is.na(data$suffix[i])) {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i])
  } else {
    data$studlab[i] <- paste0(data$author[i], ", ", data$year[i], data$suffix[i], sep = "")
  }
}

# replace different types of control with just "Control"
data$intervention_mod <- sub("^Control_.*", "Control", data$intervention)

# calculate SD from SE
for (var in c("pre", "post")) {
  missing_sd <- is.na(data[[paste0(var, "_sd")]])
  replacement_values <- data[[paste0(var, "_se")]] * sqrt(data[[paste0(var, "_n")]])
  data[[paste0(var, "_sd")]][missing_sd] <- replacement_values[missing_sd]
  data[[paste0(var, "_se")]] <- NULL
}

# calculate mean difference (MD)
data$diff_mean <- data$post_mean - data$pre_mean

# calculate standard deviation of mean difference (SDMD)
data$diff_sd <- sqrt(data$pre_sd^2 + data$post_sd^2 - 2 * 0.5 * data$pre_sd * data$post_sd)

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


## Indicated prevention ----------------------------------------------------
# Body dissatisfaction
ma.i.bd <- pairmod(split_data[["indicated_Body_Dissatisfaction"]])
write.xlsx(datas(ma.i.bd), file = "output/data.xlsx", sheetName = "Indi_bodydis")

net.i.bd <- netmeta(ma.i.bd,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/indicated_bodydis_results.txt")
summary(net.i.bd, digits = 2)
sink()

png(filename = "output/indicated_bodydis_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.i.bd)
dev.off()

png(filename = "output/indicated_bodydis_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.i.bd)
dev.off()


# ED symptoms
ma.i.ed <- pairmod(split_data[["indicated_ED_Symptoms"]])
write.xlsx(datas(ma.i.ed), file = "output/data.xlsx", sheetName = "Indi_edsymp", append = TRUE)

net.i.ed <- netmeta(ma.i.ed,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/indicated_edsymp_results.txt")
summary(net.i.ed, digits = 2)
sink()

png(filename = "output/indicated_edsymp_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.i.ed)
dev.off()

png(filename = "output/indicated_edsymp_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.i.ed)
dev.off()


# Dieting
ma.i.di <- pairmod(split_data[["indicated_Dieting"]])
write.xlsx(datas(ma.i.di), file = "output/data.xlsx", sheetName = "Indi_dieting", append = TRUE)

net.i.di <- netmeta(ma.i.di,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/indicated_dieting_results.txt")
summary(net.i.di, digits = 2)
sink()

png(filename = "output/indicated_dieting_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.i.di)
dev.off()

png(filename = "output/indicated_dieting_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.i.di)
dev.off()


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


## Universal prevention ----------------------------------------------------
# Body dissatisfaction
ma.u.bd <- pairmod(split_data[["universal_Body_Dissatisfaction"]])
write.xlsx(datas(ma.u.bd), file = "output/data.xlsx", sheetName = "Univ_bodydis", append = TRUE)

net.u.bd <- netmeta(ma.u.bd,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/universal_bodydis_results.txt")
summary(net.u.bd, digits = 2)
sink()

png(filename = "output/universal_bodydis_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.u.bd)
dev.off()

png(filename = "output/universal_bodydis_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.u.bd)
dev.off()


# ED symptoms
ma.u.ed <- pairmod(split_data[["universal_ED_Symptoms"]])
write.xlsx(datas(ma.u.ed), file = "output/data.xlsx", sheetName = "Univ_edsymp", append = TRUE)

net.u.ed <- netmeta(ma.u.ed,
                    common = FALSE,
                    ref = "Control"
                    )

sink(file = "output/universal_edsymp_results.txt")
summary(net.u.ed, digits = 2)
sink()

png(filename = "output/universal_edsymp_netgraph.png", width = 7, height = 5, units = "in", res = 300)
netgraphmod(net.u.ed)
dev.off()

png(filename = "output/universal_edsymp_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.u.ed)
dev.off()

