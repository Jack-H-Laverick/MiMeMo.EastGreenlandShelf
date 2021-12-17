
library(StrathE2EPolar)

model <- e2ep_read("Greenland_Sea","2011-2019", models.path = "StrathE2E/Models/", results.path = "StrathE2E/Results/")

results <- e2ep_run(model,nyears = 50)

plot_full_length_timeseries(model, results)
