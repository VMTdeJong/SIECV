# The unimputed GREAT data is part of the micemd package.
library(micemd)
data("IPDNa")

# Select all centers that have > 50 events and non-events.
selected_centers <- which(rowSums(with(IPDNa, table(centre, afib)) > 50) == 2)
great_unimputed <- IPDNa[IPDNa$centre %in% selected_centers, ]

source("ignore/data/save unimputed data.R")

