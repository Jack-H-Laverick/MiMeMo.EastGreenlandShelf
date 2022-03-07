
library(StrathE2EPolar)

model <- e2ep_read("Greenland_Mike","2011-2019", models.path = "StrathE2E/Models/", results.path = "StrathE2E/Results/")
 
results <- e2ep_run(model,nyears = 50)

e2ep_plot_ts(model, results)

