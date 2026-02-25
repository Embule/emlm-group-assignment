load("Group_7.RData")
head(DF)


# df.long <- reshape(df., idvar = "patient", 
#                      varying = cbind("dh0", "dh6", "dh12"), 
#                      direction = "long", times = c(0, 6, 12), 
#                      timevar = "Period", v.names = "Growth")
# df.long$TreatOrder <- factor(df.$group, 
#                                levels = c("GH-placebo", "placebo-GH"))
# df.long$sex <- factor(df.$sex, levels = 1:2, 
#                         labels = c("Male", "Female"))
# df.long$Period.f <- factor(df.long$Period, levels = c(0, 6, 12))
# df.long <- df.long[order(df.long$patient, df.long$Period), ]
# head(df.long)