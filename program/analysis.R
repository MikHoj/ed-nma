### Meta-analysis -----------------------------------------------------------

## Load files
data <- readRDS("data/datasets.rds")

bd <- data[["Body dissatifaction selective"]] %>%
  filter(analysis == "Main") %>%
  filter(!id %in% c(201, 230)) %>% 
  select(id, studlab, approach_mod, n_pre:change_sd)

ed <- data[["ED symptoms selective"]] %>%
  filter(analysis == "Main") %>%
  select(id, studlab, approach_mod, n_pre:change_sd)

di <- data[["Dieting selective"]] %>%
  filter(analysis == "Main") %>%
  select(id, studlab, approach_mod, n_pre:change_sd)

data <- NULL

## Functions for output
# Pairwise comparison
pairmod <- function (x) {
  dts <- pairwise(treat = approach_mod,
                  n = n_pre,
                  mean = change_mean,
                  sd = change_sd,
                  data = x,
                  studlab = studlab,
                  reference.group = "Control",
                  sm = "SMD"
  )
  return(dts)
}

# Data set for inspection
datas <- function (x) {
  dts <- x %>% select(studlab, treat1, treat2, TE, seTE, n1, mean1, sd1, n2, mean2, sd2)
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
ma.s.bd <- pairmod(bd)
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
ma.s.ed <- pairmod(ed)
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
ma.s.di <- pairmod(di)
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


