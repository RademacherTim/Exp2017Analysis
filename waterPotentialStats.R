#========================================================================================
# This script test for differences between treatments and sampling height in pre-dawn 
# needle and branch water potential. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('readxl') 

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
                     names_to = 'week')
phi <- merge (phi, pivot_longer (phiBranches, cols = 3:dim (phiBranches) [2], 
                                 names_to = 'week', values_to = 'branches'))

#  fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = needles ~ (1 | tree) + treatment, 
            data = phi,
            REML = TRUE)
summary (M1)

#========================================================================================