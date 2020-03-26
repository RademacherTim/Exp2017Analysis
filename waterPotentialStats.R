#========================================================================================
# This script test for differences between treatments and sampling height in pre-dawn 
# needle and branch water potential. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('readxl') 
library ('lubridate')

# load waterpotential data
#----------------------------------------------------------------------------------------
phiNeedles <- read_excel (path = '../data/waterPotential/waterPotentialMeasurementsExp2017.xlsx',
                          sheet = 'phiNeedlesWeekly', 
                          col_names = c ('tree','treatment','week0','week1','week2',
                                         'week3','week4','week5','week6','week7','week8',
                                         'week9','week10','week11','week12','week13',
                                         'week15','week17','week47','week53','week94',
                                         'week105'), skip = 1, n_max = 40, 
                          col_types = rep ('numeric', 22), na = 'NA')
phiBranches <- read_excel (path = '../data/waterPotential/waterPotentialMeasurementsExp2017.xlsx',
                           sheet = 'phiBranchesWeekly', 
                           col_names = c ('tree','treatment','week0','week1','week2',
                                          'week3','week4','week5','week6','week7','week8',
                                          'week9','week10','week11','week12','week13',
                                          'week15','week17','week47','week53','week94',
                                          'week105'), skip = 1, n_max = 40, 
                           col_types = rep ('numeric', 22), na = 'NA')
  
# wrangle data
#----------------------------------------------------------------------------------------
phi <- pivot_longer (phiNeedles, cols = 3:dim (phiNeedles) [2], values_to = 'needles',
                     names_to = 'week', names_prefix = 'week')
phi <- merge (phi, pivot_longer (phiBranches, cols = 3:dim (phiBranches) [2], 
                                 names_to = 'week', values_to = 'branches', 
                                 names_prefix = 'week'))
phi [['tree']] <- factor (phi [['tree']])
phi [['treatment']] <- factor (phi [['treatment']])


# replace week count with dates
#----------------------------------------------------------------------------------------
phi [['date']] <- factor (as_date (as.numeric (phi [['week']]) * 7 + as_date ('2017-07-03')))

#  fit mixed effects model for needle water potential with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = needles ~ (1 | tree) + date + date:treatment, 
            data = phi,
            REML = TRUE)
summary (M1)

#  fit mixed effects model for branch water potential with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = branches ~ (1 | tree) + date + date:treatment, 
            data = phi,
            REML = TRUE)
summary (M2)

#========================================================================================