rm(list = ls())
# set up environment

library(nlme)
library(AICcmodavg)
library(gtools)
library(dplyr)

vram1 = matrix(rnorm(120*1, mean=5, sd=10), ncol=1)
vram2 = matrix(runif(120*1, min=-50, max = 300), ncol=1)
vram3 = matrix(rexp(120*1, rate = 4), ncol=1)
vram4 = matrix(rnorm(120*1, mean=-300, sd=40), ncol=1)
vram5 = matrix(runif(120*1, min=-1, max = 6), ncol=1)
vram6 = matrix(rexp(120*1, rate = 10), ncol=1)
vram7 = sum(1*vram1,2*vram2,3*vram3,4*vram4,5*vram5)
vram8 = cbind(vram1,vram2,vram3,vram4,vram5,vram6,vram7)

write.table(vram8,'rand.csv',sep = ',')

# load data
vdata <- read.csv(file = 'sim_data.csv')
csv_file_name_IC = 'selected_model_IC_result1.csv'
if(file.exists(csv_file_name_IC)){
  file.remove(csv_file_name_IC)
}

# generate formula
vformula = read.csv(file = 'all_formula.csv', stringsAsFactors = FALSE)
# vformula = data.frame(Model = vformula[c(1,6,10,3165,3164),],stringsAsFactors = F) # only select known redundant for testing
vmodel = lapply(vformula$Model,function(i)
  lme(as.formula(i),data = vdata, random = ~1|Subject))
vresidual = 0
vsigma = 0
vAIC = 0
vAICc = 0
vBIC = 0

for (val in 1:length(vmodel)) {
  vresidual[val] = vmodel[[val]]$residuals[1]
  vresidual[val] = sum(abs(vmodel[[val]]$residuals))
  vsigma[val] = vmodel[[val]]$sigma
  vsumary = summary(vmodel[[val]])
  vAICc[val] = AICc(vmodel[[val]])
  vAIC[val] = vsumary$AIC
  vBIC[val] = vsumary$BIC
  
}
vIC = data.frame(
  Model = (vformula),
  residaul = vresidual,
  sigma = vsigma,
  AIC = vAIC,
  BIC = vBIC,
  AICc = vAICc
)
write.table(vIC, csv_file_name_IC, sep = ",", col.names = !file.exists(csv_file_name_IC), append = T, row.names = F)
