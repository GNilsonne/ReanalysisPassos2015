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

# IL-6 as biomarker of severity?

# Plot was exported manually on 2015-12-03 with 600x600 resolution.

# Sensitivity analysis excluding outlier study by Guo et al.
res2 <- rma(yi = smd, sei = sei, data = data[data$study != "Guo 2012", ], slab=study)
forest(res2)
funnel(res2)
trimfill(res2)
funnel(trimfill(res2))
PET(data[data$study != "Guo 2012", ])
PEESE(data[data$study != "Guo 2012", ])
funnelPETPEESE(data[data$study != "Guo 2012", ])

egger2 <- rma(yi = smd, sei = sei, data = data[data$study != "Guo 2012", ], slab=study,
              mods = ~sei)
summary(egger2) # significant, p = .019

# Other considerations ----
# Correlation btwn moderators, MDD diagnosis and medication
table(data$mdd, data$medication, dnn = list("MDD", "Medication"))
temp = data.frame("medicated" = ifelse(data$medication == "psychotropic", 1, 0),
                  "mdd" = ifelse(data$mdd == "with", 1, 0),
                  "sei" = data$sei)
cor(temp)

# Heterogeneity
funnel(res, pch = ifelse(data$medication == "psychotropic", 15, 17))
funnel(res, pch = ifelse(data$mdd == "without", 15, 17))

# PET and PEESE within moderator groups
PET(data[data$medication == "free",])
PEESE(data[data$medication == "free",])
funnelPETPEESE(data[data$medication == "free",])
PET(data[data$medication == "psychotropic",])
PEESE(data[data$medication == "psychotropic",])
funnelPETPEESE(data[data$medication == "psychotropic",])

PET(data[data$mdd == "without",])
PEESE(data[data$mdd == "without",])
funnelPETPEESE(data[data$mdd == "without",])
PET(data[data$mdd == "with",])
PEESE(data[data$mdd == "with",])
funnelPETPEESE(data[data$mdd == "with",])

funnelPETPEESE(data[data$mdd == "without" & data$study != "Guo 2012",])

# Maybe not so big a moderation after considering pub bias & excluding outlier?
funnelPETPEESE(data[data$mdd == "without" & data$study != "Guo 2012",])
PEESE(data[data$mdd == "without" & data$study != "Guo 2012",])
funnelPETPEESE(data[data$mdd == "with",])
PEESE(data[data$mdd == "with",])
# both suggest d = .50, although I don't know why these groups would
  # exhibit different degrees of bias

# Plots w/ significance zone might help
funnel(res, refline = 0, back = "grey90",
       level = c(90, 95, 99), shade = c("grey98", "grey55", "grey75"))

# Make cumulative forest plot ordered by study precision
res3 <- cumul(res, order=order(data$sei))
forest(res3)
