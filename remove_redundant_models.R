rm(list = ls())
library(dplyr)
vfile_list = list.files(pattern = 'IC')
vtable = read.csv(vfile_list[1])
vtable1 = distinct(vtable,BIC, .keep_all= TRUE)

diff_threshold = 1e-6

vdata_sorted = vtable[order(vtable$AIC),]

vdiff = diff(sort(vdata_sorted$AIC))
keep_ind5 = which(vdiff>diff_threshold)

vdata_sorted1 = rbind(vdata_sorted[1,],vdata_sorted[keep_ind5+1,])

vdata_sorted1$Name_length = nchar(as.character(vdata_sorted1$Model))
vdata_sorted2 = vdata_sorted1[order(vdata_sorted1$Model),]
vdata_sorted3 = vdata_sorted2[order(vdata_sorted2$Name_length),]

write.table(vdata_sorted3, 'non_redundant_formula.csv', sep = ",", row.names = F)

x = c(1:10)
sx = x^2
dx = diff(sx)
xind = which(dx>11)
sx[xind]
