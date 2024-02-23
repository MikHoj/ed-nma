

## ------------------------------------------------------------------------
library(here)
library(readxl)
library(netmeta)

## ------------------------------------------------------------------------
AcuteMania = read_excel("tutorial/AcuteMania.xls")
AcuteMania = as.data.frame(AcuteMania)
str(AcuteMania)

## ------------------------------------------------------------------------
Leucht = read_excel("tutorial/Leucht.xls")
Leucht = as.data.frame(Leucht)
str(Leucht)

## ------------------------------------------------------------------------
table(AcuteMania$treatment)

## ------------------------------------------------------------------------
table(table(AcuteMania$studyid))

## ------------------------------------------------------------------------
AcuteManiaPair = pairwise(treat = treatment,
                          event = r,
                          n = n,
                          data = AcuteMania,
                          studlab = studyid,
                          sm = "OR"
                          )

## ------------------------------------------------------------------------
AcuteMania[AcuteMania$studyid < 4, ]
AcuteManiaPair[as.numeric(AcuteManiaPair$studlab) < 4, 1:9]

## ------------------------------------------------------------------------
net1 = netmeta(AcuteManiaPair)

## ------------------------------------------------------------------------
netgraph(net1)

## ------------------------------------------------------------------------
netgraph(net1,
         plastic = FALSE,
         multiarm = FALSE, 
         thickness = "number.of.studies",
         points = TRUE, 
         cex.points = table(AcuteMania$treatment) / 2, 
         col = 1)

## ---- eval = FALSE-------------------------------------------------------
## # install.packages("rgl")
## # netgraph(net1, plastic = FALSE, multiarm = FALSE, dim = "3d", col = 1)

## ------------------------------------------------------------------------
net1 = netmeta(AcuteManiaPair,  sm = "OR", ref = "PLA",
               comb.fixed = FALSE, comb.random = TRUE)
summary(net1, digits = 2)

## ------------------------------------------------------------------------
net1 = netmeta(AcuteManiaPair,  sm = "OR", ref = "PLA",
               comb.fixed = FALSE, comb.random = TRUE,
               baseline.reference = FALSE)
summary(net1, digits = 2)

## ------------------------------------------------------------------------
round(net1$tau, 3)

## ------------------------------------------------------------------------
paste(round(net1$I2), "%", sep = "")

## ------------------------------------------------------------------------
leaguetable = netleague(net1, digits = 2)
leaguetable

## ------------------------------------------------------------------------
write.csv(leaguetable$random, "leaguetable-random.csv", row.names = FALSE)

## ---- eval = FALSE-------------------------------------------------------
## # install.packages("WriteXLS")
## # library(WriteXLS)
## # WriteXLS(leaguetable$random, ExcelFileName = "leaguetable-random.xls",
## #          SheetNames = "leaguetable (random)", col.names = FALSE)

## ------------------------------------------------------------------------
forest(net1)
forest(net1, ref = "HAL")

## ------------------------------------------------------------------------
forest(net1, sortvar = TE)

## ------------------------------------------------------------------------
forest(net1, sortvar = TE,
       leftcols = c("studlab", "k"), leftlabs = c("Drug", "Direct\nstudies",
       xlab = "Response to treatment", smlab = "NMA random effects"))

## -SUCRA-----------------------------------------------------------------------
netrank(net1, small = "bad")

## ------------------------------------------------------------------------
forest(net1,
       rightcols = c("effect", "ci", "Pscore"),
       rightlabs = "P-Score", sortvar = -Pscore, small = "bad",
       xlab = "Response to treatment", smlab = "Active drug vs placebo")

## ------------------------------------------------------------------------
data.rob = AcuteMania[!duplicated(AcuteMania$studyid), c("studyid", "rob")]
AcuteManiaPair = merge(AcuteManiaPair, data.rob, by.x = "studlab", by.y = "studyid")

## ------------------------------------------------------------------------
table(AcuteManiaPair$rob.x)

## ------------------------------------------------------------------------
net2 = netmeta(TE, seTE, treat1, treat2, studlab, data = AcuteManiaPair, subset = rob.x == 1,
               sm = "OR", ref = "PLA", comb.fixed = FALSE, comb.random = TRUE)
summary(net2, digits = 2)

## ------------------------------------------------------------------------
net3 = netmeta(effect, se, treat1, treat2, study, data = Leucht,
               sm = "SMD", ref = "PBO", comb.fixed = FALSE, comb.random = TRUE,
               tol.multiarm = 0.075)

## ------------------------------------------------------------------------
summary(net3)
forest(net3, sortvar = TE)
netrank(net3)
