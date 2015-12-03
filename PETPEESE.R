require(metafor)
require(magrittr)

### Define functions
# Adapted with gratitude from: https://github.com/Joe-Hilgard/Anderson-meta/blob/master/PETPEESE_functions.R
# The following changes have been made:
# - Analysis method was changed from fixed effects "FE" to random effects "REML"
# - Variable named for "vi" and "sei" were changed

# naive meta-analysis ----
naive = function(dataset, ...) {
  rma(yi = yi,
      sei = sei,
      data = dataset,
      method = "REML",
      ...)
}

# basic PET ----
PET=function(dataset, error = "additive") {
  if (error == "additive") {
    petOut = rma(yi = yi, 
                 sei = sei, 
                 mods = ~sei, 
                 data=dataset,
                 method = "REML")
  }
  if (error == "multiplicative") {
    petOut = lm(yi ~ sei,
                weights = 1/sei,
                data=dataset)
  }
  return(petOut)
}

# basic PEESE ----
PEESE=function(dataset, error = "additive") {
  if (error == "additive") {
    peeseOut = rma(yi = yi, 
                   sei = sei, 
                   mods = ~I(sei^2), 
                   data=dataset,
                   method = "REML")
  }
  if (error == "multiplicative") {
    peeseOut = lm(yi ~ I(sei^2), 
                  weights = 1/sei,
                  data=dataset)
  }
  return(peeseOut)
}

# funnel plot with PET line and conditional PEESE line ----
funnelPETPEESE = function(dataset, 
                          error = "additive",
                          alwaysPEESE=T, plotName=NULL, printText = F,
                          ...) {
  naiveModel = naive(dataset)
  petModel = PET(dataset, error)
  peeseModel = PEESE(dataset, error)
  # make funnel plot
  funnel(naiveModel, ...)
  if (printText == T) title(plotName, line=3)
  if (printText == F) title(plotName)
  if (error == "additive") {
    if (printText == T) naiveModel$b[1] %>% 
      tanh %>% 
      round(3) %>%
      paste("Naive meta estimate, r =", .) %>%
      mtext(side=1)
    # add line and text from PET
    petModel %$% 
      abline(a = -b[1]/b[2], b = 1/b[2])
    r = petModel$b[1] %>% tanh %>% round(3)
    p.effect = petModel$pval[1] %>% round(3)
    p.bias = petModel$pval[2] %>% round(3)
    if (printText == T) mtext(paste("PET r = ", r
                                    , ", p-effect = ", p.effect
                                    , ", p-bias = ", p.bias
                                    , sep=""))
    points(x = petModel$b[1], y=0, cex=1.5)
    #abline(v = petModel$b[1], lty = 2)
    #lines(x = rep(petModel$b[1], 2), y = c(ymin, ymin - .1)
    # add line and text from PEESE
    if(petModel$pval[1] < .05 || alwaysPEESE == T) {
      grid = 
        naiveModel$vi %>%
        raise_to_power(.5) %>%
        max %>%
        seq(0, ., .001) %>%
        data.frame("sei" = .)
      grid$Var = grid$sei^2
      grid$yi = 
        peeseModel$b[1] + peeseModel$b[2]*grid$Var
      grid %$% lines(x=yi, y=sei, typ='l')
      points(x = (peeseModel$b[1]), y=0, cex=1.5, pch=5)
      #abline(v = peeseModel$b[1], lty = 2)
      if (printText == T) { 
        peeseModel$b[1] %>%
          tanh %>%
          round(3) %>%
          paste("PEESE r =", .) %>%
          mtext(line = 1)
      }
    }
  }
  if(error == "multiplicative") {
    if (printText == T) naiveModel$b[1] %>% 
      tanh %>% 
      round(3) %>%
      paste("Naive meta estimate, r =", .) %>%
      mtext(side=1)
    # add line and text from PET
    b = summary(petModel)$coefficients[,1]
    petModel %$% 
      abline(a = -b[1]/b[2], b = 1/b[2])
    r = b[1] %>% tanh %>% round(3)
    p.effect = summary(petModel)$coefficients[1,4] %>% round(3)
    p.bias = summary(petModel)$coefficients[2,4] %>% round(3)
    if (printText == T) mtext(paste("PET r = ", r
                                    , ", p-effect = ", p.effect
                                    , ", p-bias = ", p.bias
                                    , sep=""))
    points(x = b[1], y=0, cex=1.5)
    #abline(v = b[1], lty = 2)
    # add line and text from PEESE
    if(summary(petModel)$coefficients[1,4] < .05 || alwaysPEESE == T) {
      grid = 
        naiveModel$vi %>%
        raise_to_power(.5) %>%
        max %>%
        seq(0, ., .001) %>%
        data.frame("sei" = .)
      grid$Var = grid$sei^2
      b = summary(peeseModel)$coefficients[,1]
      grid$yi = 
        b[1] + b[2]*grid$Var
      grid %$% lines(x=yi, y=sei, typ='l')
      points(x = b[1], y=0, cex=1.5, pch=5)
      #abline(v = b[1], lty = 2)
      if (printText == T) {
        b[1] %>%
          tanh %>%
          round(3) %>%
          paste("PEESE r =", .) %>%
          mtext(line = 1)
      }
    }
  }
}