##### The data
source("ignore/data/get imputed data.R")

library(dplyr)
library(finalfit)

great_table1data <- great_imputed
great_table1data$afib <- as.factor(great_table1data$afib)

# Select variables.
table1 <- great_table1data %>%
    summary_factorlist(dependent = "afib", 
                       explanatory = c("gender", "bmi", "age", "sbp", "dbp", "hr", "bnp"),
                       add_dependent_label=T,
                       total_col = T)
table(great_imputed$gender) # Which is which?

write.csv(table1, file = "ignore/tables/GREAT table1.csv")
