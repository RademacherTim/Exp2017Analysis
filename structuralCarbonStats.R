#========================================================================================
# This script test for differences between treatments and sampling height in structural 
# carbon related variables, such as ring widths, cell wall thickness, lumen diameter, 
# number of cell per ring and cell size. 
#----------------------------------------------------------------------------------------

# load dependencies
library ('nlme')

# wranlge standardised ring width at the end of the experiment
standardisedRW2017 <- add_column (standardisedRW2017, 
                                  treatment = allometricData [['treatment']] [1:40])
RW2017 <- tibble (tree = 1, height = 'M', treatment = 1, 
                  RW = standardisedRW2017 [1, 3])
for (i in 2:40) {
  if (standardisedRW2017 [['treatment']] [i] == 1) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'M', RW = standardisedRW2017 [i, 3]) 
  } else if (standardisedRW2017 [['treatment']] [i] == 2 |
             standardisedRW2017 [['treatment']] [i] == 3) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'A', RW = standardisedRW2017 [i, 2])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'B', RW = standardisedRW2017 [i, 4])
  } else if (standardisedRW2017 [['treatment']] [i] == 4) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'A', RW = standardisedRW2017 [i, 1])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'M', RW = standardisedRW2017 [i, 3])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'B', RW = standardisedRW2017 [i, 5])
  }
}

# convert height to a factor 
RW2017 [['height']] <-  as.factor (RW2017 [['height']])

# plot ring width data
boxplot (RW ~ height * treatment, data = RW2017, las = 1, 
         xlab = 'treatment and sampling height', ylab = 'adjusted ring width', xaxt = 'n')
axis 

# fit mixed effects model with tree as random effect
fit0 <- lme (RW ~ treatment, random = list (tree = ~1, height = ~1), 
             method = "REML", data = RW2017, na.action = 'na.omit')
fit1 <- lme (RW ~ height, random = list (tree = ~1, treatment = ~1), 
             method = "REML", data = RW2017, na.action = 'na.omit')
fit2 <- lme (RW ~ height, random = ~1|tree, 
             method = "REML", data = RW2017, na.action = 'na.omit')
fit3 <- lme (RW ~ height + treatment, random = ~1|tree, 
             method = "REML", data = RW2017, na.action = 'na.omit')
fit4 <- gls (RW ~ height * treatment, data = RW2017, na.action = 'na.omit')

AIC (fit0, fit1, fit2, fit3, fit4)
#========================================================================================
