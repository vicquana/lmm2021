rm(list = ls())
# set up environment

library(lme4)
library(AICcmodavg)
library(gtools)

vram1 = matrix(rnorm(20*6, mean=5, sd=10), ncol=6)
vram2 = matrix(runif(20*6, min=-5, max = 3), ncol=6)
vram3 = matrix(rexp(20*6, rate = 4), ncol=6)
vram4 = rbind(vram1,vram2,vram3)
write.table(vram4,'rand.csv',sep = ',')

# load data
vdata <- read.csv(file = 'sim_data.csv')
csv_file_name_IC = 'selected_model_IC_result1.csv'
if(file.exists(csv_file_name_IC)){
  file.remove(csv_file_name_IC)
}

# generate formula
vformula = read.csv(file = 'all_formula.csv', stringsAsFactors = FALSE)
vmodel = lapply(vformula$Model,function(i)
  lmer(as.formula(i),data = vdata,  na.action=na.exclude))
vresidual = 0
vanova = data.frame()
for (val in 1:length(vmodel)) {
  vresidual[val] = vmodel[[val]]$residuals[1]
  
}
vIC = data.frame(
  Model = (vformula),
  residaul = vresidual
)
write.table(vIC, csv_file_name_IC, sep = ",", col.names = !file.exists(csv_file_name_IC), append = T, row.names = F)
