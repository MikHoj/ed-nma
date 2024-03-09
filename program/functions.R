### Functions for network meta-analysis

# Pairwise comparison
pairmod <- function (x) {
  dts <- pairwise(treat = label,
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

# Network meta-analysis
netmetamod <- function (x) {
  nma <- netmeta(x,
                 common = FALSE,
                 ref = "Control"
                 )
  return(nma)
}

# Netgraph
netgraphmod <- function (x) {
  ngr <- netgraph(x,
                  seq = "optimal",
                  plastic = FALSE,
                  number.of.studies = TRUE,
                  cex.points = n.trts,
                  cex = 1.25,
                  labels = paste0(trts, "\n(n=", round(n.trts), ")")
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
