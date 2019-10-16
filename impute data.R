library(mitml)

n_burnin <- 10000
n.iter <- 1000
m <- 1

great_mitml <- jomoImpute(formula = as.formula("bmi+age+sbp+dbp+hr+bnp+afib~gender+lvef+(1|centre)"), 
                     random.L1 = "full",  # of "none" voor homoscedastic
                     data = great_unimputed, 
                     m = m, # 1 imputatien
                     n.burn = n_burnin,
                     n.iter = n.iter)

# Put results in a list in mitml format
great_imputed <- mitmlComplete(great_mitml, print = seq_len(m))

source("ignore/data/save imputed data.R")
