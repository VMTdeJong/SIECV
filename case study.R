source("ignore/data/get imputed data.R")

##### Functions
# metamisc version of 29 August 2019 or later is necessary.
# install.packages("metamisc", repos="http://R-Forge.R-project.org")
source("ignore/functions.R")
library(metamisc)
library(logistf)

great_imputed$afib <- as.numeric(great_imputed$afib) - min(as.numeric(great_imputed$afib) )
great_final <- great_imputed
great_final$age <- great_imputed$age / 25
great_final$hr  <- great_imputed$hr / 25
great_final$sbp <- great_imputed$sbp / 100
great_final$dbp <- great_imputed$dbp / 100
great_final$bmi <- great_imputed$bmi / 25

# So that the center ID's are 1 to 20.
length(with(great_imputed, table(centre)))
great_final$centre[great_final$centre == 27] <- 3
great_final$centre[great_final$centre == 26] <- 5
great_final$centre[great_final$centre == 25] <- 10
great_final$centre[great_final$centre == 24] <- 11
great_final$centre[great_final$centre == 23] <- 13
great_final$centre[great_final$centre == 22] <- 17
great_final$centre[great_final$centre == 21] <- 20

f <- afib ~ 1
s <- afib ~ gender + bmi + age + sbp + dbp + hr + bnp +
  I(bmi^2) + I(age^2) + I(sbp^2) + I(dbp^2) + I(hr^2) + I(bnp^2)

library(splines)

### Strategy 1: Ignoring heterogeneity
# Model 1: Ignoring heterogeneity
suppressMessages(m1 <- metapred(data = great_final, 
               strata = "centre", 
               formula = f, 
               scope = s, 
               estFUN = "logistfirth",
               family = binomial,
               perfFUN = list("mse", "bin.cal.int", "cal.slope", "auc"),
               genFUN = list("abs.mean", "rema.beta", "rema.tau"),
               center = TRUE))


### Strategy 2: Weighted Meta-Analysis
# Model 2a: Weighted Meta-Analysis: Mean effect
suppressMessages(m2a <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 1))

# Model 2b: Weighted Meta-Analysis: Mean effect + heterogeneity
suppressMessages(m2b <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 1/2))

# Model 2c: Weighted Meta-Analysis: Heterogeneity
suppressMessages(m2c <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 0))

### Strategy 3: Heterogeneity only
# Model 3: Heterogeneity only: parametric
suppressMessages(m3a <- update(m1, genFUN = list("SD", "rema.beta", "rema.tau")))

# Model 3: Heterogeneity only: non-parametric
suppressMessages(m3b <- update(m1, genFUN = list("gmd", "rema.beta", "rema.tau")))

models <- list(m1 = m1, m2a = m2a, m2b = m2b, m2c = m2c, m3a = m3a, m3b = m3b)

# source("ignore/models/save_models.R")
# source("ignore/models/load_models.R")

lapply(models, coef)

##### Comparison.
### Coefficients
# Which variables are selected, in any model?
selected <- unique(unlist(lapply(lapply(models, coef), names))) 

# It is important that the ordering of the variables is maintained.
# Thus we select them as follows.
# Note that NA means that a variable was not selected in the model: it's coefficient is zero.
all_coefs <- sapply(lapply(models, coef), '[', selected)

# Some of the variables' names are not passed on, as they were not present in the first model.
rownames(all_coefs) <- selected
model_names <- c("Mean", "MA mean", "MA both", "MA heterogeneity", "SD", "Gini")
colnames(all_coefs) <- model_names

all_coefs_manuscript <- round(all_coefs, digits = 2)
all_coefs_manuscript[is.na(all_coefs_manuscript)] <- ""
all_coefs_manuscript
data.frame(all_coefs_manuscript, stringsAsFactors = TRUE)

write.csv(data.frame(all_coefs_manuscript, stringsAsFactors = TRUE),
          file = "ignore/tables/GREAT CS with selection - coefs sep 2020.csv")


### Performance and Generalizability
# MSE
# get_summary(models, "mse", model_names = model_names, digits = 3)

# Calibration intercept
ints <- get_summary(models, "bin.cal.int", model_names = model_names)

# Calibration slope
slopes <- get_summary(models, "cal.slope", model_names = model_names)

# AUC
aucs <- get_summary(models, "auc", model_names = model_names)

all <- rbind(t(slopes), t(ints), t(aucs))

all_formatted <- format_summary(all)
# Combined
write.csv(all_formatted, file =  "ignore/tables/GREAT CS with selection - ma of perfs - sep 2020.csv")


### Forest plots
# Calibration intercept
mapply(pdf_forest, models = models[1:6], m = 1:6, stat_id = 2)

# Calibration slope
mapply(pdf_forest, models = models[1:6], m = 1:6, stat_id = 3)

# AUC
mapply(pdf_forest, models = models[1:6], m = 1:6, stat_id = 4)

##### The End. ##### 
