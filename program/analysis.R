### Meta-analysis -----------------------------------------------------------

## Load files
data <- readRDS("data/datasets.rds")

bd <- data[["Body dissatifaction selective"]] %>%
  filter(analysis == "Main") %>%
  filter(!id %in% c(201, 230)) %>% 
  select(id, studlab, label, questionaire_note, n_pre:change_sd)
bd$change_mean <- ifelse(bd$questionaire_note == "BS", bd$change_mean * -1, bd$change_mean)
treat.bd <- sort(unique(bd$label))

ed <- data[["ED symptoms selective"]] %>%
  filter(analysis == "Main") %>%
  select(id, studlab, label, n_pre:change_sd)
treat.ed <- sort(unique(ed$label))

di <- data[["Dieting selective"]] %>%
  filter(analysis == "Main") %>%
  filter(!id %in% c(201)) %>%
  select(id, studlab, label, n_pre:change_sd)
treat.di <- sort(unique(di$label))

rm("data")


## Selective prevention ----------------------------------------------------
# Body dissatisfaction
ma.s.bd <- pairmod(bd)
write.xlsx(datas(ma.s.bd), file = "output/data.xlsx", sheetName = "Selec_bodydis", append = FALSE)

net.s.bd <- netmetamod(ma.s.bd)

sink(file = "output/selective_bodydis_results.txt")
summary(net.s.bd, digits = 2)
netrank(net.s.bd)
sink()

png(filename = "output/selective_bodydis_netgraph.png", width = 12, height = 10, units = "in", res = 300)
netgraphmod(net.s.bd)
dev.off()

png(filename = "output/selective_bodydis_forest.png", width = 10, height = 6, units = "in", res = 300)
forestplotmod(net.s.bd)
dev.off()

png(filename = "output/selective_bodydis_netsplit.png", width = 12, height = 20, units = "in", res = 300)
forest(netsplit(net.s.bd, show = "with.direct"))
dev.off()

write.xlsx(netleague(net.s.bd,
                     seq = netrank(net.s.bd),
                     digits = 2)$random,
           "output/league.xlsx",
           sheetName = "Selec_bodydis",
           row.names = FALSE,
           col.names = FALSE,
           append = FALSE
           )


png(filename = "output/selective_bodydis_funnel.png", width = 12, height = 8, units = "in", res = 300)
funnel(net.s.bd, order = treat.bd, method.bias = "Egger", digits.pval = 2)
dev.off()


# ED symptoms
ma.s.ed <- pairmod(ed)
write.xlsx(datas(ma.s.ed), file = "output/data.xlsx", sheetName = "Selec_edsymp", append = TRUE)

net.s.ed <- netmetamod(ma.s.ed)

sink(file = "output/selective_edsymp_results.txt")
summary(net.s.ed, digits = 2)
netrank(net.s.ed)
netsplit(net.s.ed, show = "both")
sink()

png(filename = "output/selective_edsymp_netgraph.png", width = 12, height = 10, units = "in", res = 300)
netgraphmod(net.s.ed)
dev.off()

png(filename = "output/selective_edsymp_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.s.ed)
dev.off()

png(filename = "output/selective_edsymp_netsplit.png", width = 12, height = 20, units = "in", res = 300)
forest(netsplit(net.s.ed, show = "with.direct"))
dev.off()

write.xlsx(netleague(net.s.ed,
                     seq = netrank(net.s.ed),
                     digits = 2)$random,
           "output/league.xlsx",
           sheetName = "Selec_edsymp",
           row.names = FALSE,
           col.names = FALSE,
           append = TRUE
)

png(filename = "output/selective_edsymp_funnel.png", width = 12, height = 8, units = "in", res = 300)
funnel(net.s.ed, order = treat.ed, method.bias = "Egger", digits.pval = 2)
dev.off()


# Dieting
ma.s.di <- pairmod(di)
write.xlsx(datas(ma.s.di), file = "output/data.xlsx", sheetName = "Selec_dieting", append = TRUE)

net.s.di <- netmetamod(ma.s.di)

sink(file = "output/selective_dieting_results.txt")
summary(net.s.di, digits = 2)
netrank(net.s.di)
netsplit(net.s.di, show = "with.both")
sink()

png(filename = "output/selective_dieting_netgraph.png", width = 12, height = 8, units = "in", res = 300)
netgraphmod(net.s.di)
dev.off()

png(filename = "output/selective_dieting_forest.png", width = 10, height = 5, units = "in", res = 300)
forestplotmod(net.s.di)
dev.off()

png(filename = "output/selective_dieting_netsplit.png", width = 12, height = 20, units = "in", res = 300)
forest(netsplit(net.s.di, show = "with.direct"))
dev.off()

write.xlsx(netleague(net.s.di,
                     seq = netrank(net.s.di),
                     digits = 2)$random,
           "output/league.xlsx",
           sheetName = "Selec_dieting",
           row.names = FALSE,
           col.names = FALSE,
           append = TRUE
)

png(filename = "output/selective_dieting_funnel.png", width = 12, height = 8, units = "in", res = 300)
funnel(net.s.di, order = treat.di, method.bias = "Egger", digits.pval = 2)
dev.off()