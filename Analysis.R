# Read functions
source("PETPEESE.R")

# Read data
data <- read.csv("data_fig3a.csv", dec=",")
data$ci.halfwidth = abs(data$smd - data$ci.lower)
data$sei = data$ci.halfwidth/1.96
data$yi = data$smd # Naming to work with PETPEESE function

# Reanalyse data
res <- rma(yi = smd, sei = sei, data = data, slab=study)
forest(res)
funnel(res)
trimfill(res)

# Perform PET and PEESE
PET(data)
PEESE(data)
funnelPETPEESE(data)

# Plot was exported manually on 2015-12-03 with 600x600 resolution.