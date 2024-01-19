rm(list=ls())
library(gtools)

# lad data
vinput = c('in1','in2','in3','in4','in5')
# vinput = c('in1','in2','in3','in4')
vinput = c('in1','in2','in3')

voutput = 'out'
ninput = length(vinput)
vcomb = unlist(
  lapply(1:ninput,
         function(i)combn(1:ninput,i,simplify=FALSE)
  )
  ,recursive=FALSE)
vperm = lapply(1:ninput,
               function(i)permutations(n=ninput,r=i,v=1:ninput,repeats.allowed=F)
)
# generate formula
vformula4 = 0
nformula4 = 0
comb_list = combn(1:ninput,1,simplify=F)

for (iinput in 1 : length(comb_list)){
  nformula4 = nformula4 + 1
  vformula4[nformula4] = paste(voutput," ~ ", '1+',vinput[comb_list[[iinput]]])
}
for (iterm in 2 : ninput){
  operator_perm = permutations(n = 2, r = iterm-1, repeats.allowed=T)
  # print(operator_perm)
  comb_list = combn(1:ninput,iterm,simplify=F)
   print(comb_list)
  perm_list = permutations(n=ninput,r=iterm,repeats.allowed=F)
  # print(perm_list)
  operator_list=0
    for (irow in 1:dim(operator_perm)[1]){
    c_operator_row = operator_perm[irow,]
    # print(c_operator_row)
    operator_list[c_operator_row==1] = '+'
    operator_list[c_operator_row==2] = '*'
    # print(operator_list)
    if (iterm == 2){
      for (iinput in 1 : dim(perm_list)[1]){
        nformula4 = nformula4 + 1
        c_vinput = vinput[perm_list[iinput,]]
        vformula4[nformula4] = paste(voutput," ~ ", '1+',c_vinput[1],operator_list[1],c_vinput[2])
      }
    }
    if (iterm == 3){
      for (iinput in 1 : dim(perm_list)[1]){
        nformula4 = nformula4 + 1
        c_vinput = vinput[perm_list[iinput,]]
        vformula4[nformula4] = paste(voutput," ~ ", '1+',c_vinput[1],operator_list[1],c_vinput[2],operator_list[2],c_vinput[3])
      }
    }
    if (iterm == 4){
      for (iinput in 1 : dim(perm_list)[1]){
        nformula4 = nformula4 + 1
        c_vinput = vinput[perm_list[iinput,]]
        vformula4[nformula4] = paste(voutput," ~ ", '1+',c_vinput[1],operator_list[1],c_vinput[2],operator_list[2],c_vinput[3],operator_list[3],c_vinput[4])
      }
    }
    if (iterm == 5){
      for (iinput in 1 : dim(perm_list)[1]){
        nformula4 = nformula4 + 1
        c_vinput = vinput[perm_list[iinput,]]
        vformula4[nformula4] = paste(voutput," ~ ", '1+',c_vinput[1],operator_list[1],c_vinput[2],operator_list[2],c_vinput[3],operator_list[3],c_vinput[4],operator_list[4],c_vinput[5])
      }
    }
    
    }
}
print(vformula4)
write.table(vformula4, 'all_formula.csv', sep = ",", col.names = 'Model', row.names = F)
