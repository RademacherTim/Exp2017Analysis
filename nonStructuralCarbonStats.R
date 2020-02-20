#========================================================================================
# This script test for differences between treatments and sampling height in 
# nonstructural carbon related variables, such as soluble sugar and strach concentrations. 
#----------------------------------------------------------------------------------------

# load dependencies
library ('lme4')
library ('nlme')

# Read the sugar and starch concentration (means for needles and roots and first centimeter for wood sections)
suppressMessages (source ('/home/trademacehr/projects/PlantGrowth/nonstructuralCarbon/processExpNSCData.R'))

# Convert tree, date, treatment and sampleHeight to factors
stemData2017 [['tree']]      <- factor (stemData2017 [['treeID']])
stemData2017 [['date']]      <- factor (stemData2017 [['date']])
stemData2017 [['treatment']] <- factor (stemData2017 [['treatment']], levels = c (4:1))
for (i in 1:dim (stemData2017) [1]) {
  if (stemData2017 [['treatment']] [i] == 1) {
    stemData2017 [['height']] [i] <- 'C'
  } else if (stemData2017 [['treatment']] [i] == 2 | 
             stemData2017 [['treatment']] [i] == 3) {
    if (stemData2017 [['sampleHeight']] [i] == 1.0) {
      stemData2017 [['height']] [i] <- 'B'  
    } else if (stemData2017 [['sampleHeight']] [i] == 2.0) {
      stemData2017 [['height']] [i] <- 'A'  
    }
  } else if (stemData2017 [['treatment']] [i] == 4) {
    if (stemData2017 [['sampleHeight']] [i] == 0.5) {
      stemData2017 [['height']] [i] <- 'B'  
    } else if (stemData2017 [['sampleHeight']] [i] == 1.5) {
      stemData2017 [['height']] [i] <- 'M'  
    } else if (stemData2017 [['sampleHeight']] [i] == 2.5) {
      stemData2017 [['height']] [i] <- 'A'  
    }
  }
}
stemData2017 [['height']] <- factor (stemData2017 [['height']], levels = c ('A','M','B','C'))

# Fit mixed effects model with tree as random effect to analyse stem soluble sugar
# M1 <- lmer (formula = sugar ~ (1 | tree) + date + date:treatment:height, 
#             data = stemData2017,
#             REML = FALSE)
# summary (M1)
M2 <- lmer (formula = sugar ~ (1 | tree) + date + treatment:height, 
            data = stemData2017,
            REML = FALSE)
summary (M2)
M3 <- lmer (formula = sugar ~ (1 | tree) + treatment:height, 
            data = stemData2017,
            REML = FALSE)
summary (M3)
#anova (M1, M2, M3)
# Model one fits is a more suscinct description of the data with lower AIC, BIC and an anova indicating that it is a better fit.

# Convert tree, date, treatment and sampleHeight to factors
leafData2017 [['tree']]      <- factor (leafData2017 [['treeID']])
leafData2017 [['date']]      <- factor (leafData2017 [['date']])
leafData2017 [['treatment']] <- factor (leafData2017 [['treatment']], levels = c (1:4))

# Fit mixed effects model to the leaf soluble sugar data
M4 <- lmer (formula = sugar ~ (1 | tree) + date + treatment:date,
            data = leafData2017,
            REML = TRUE)
summary (M4)
