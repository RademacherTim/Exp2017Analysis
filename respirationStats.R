#========================================================================================
# This script test for differences between treatments and sampling height in 
# stem CO2 efflux for the 2017 experiment at Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')

# Read processed respiration data
#----------------------------------------------------------------------------------------
source ('/home/tim/projects/PlantGrowth/stemCO2Efflux/readProcessedRespData.R')

# Wrangle the data 
#----------------------------------------------------------------------------------------
respData [['date']] <- as_date (respData [['timestamp']])
respData [['treatment']] <- factor (respData [['treatment']], levels = c (4:1))
respData [['tree']] <- factor (respData [['tree']])
respData [['height']] <- respData [['chamber']]
respData [['height']] [respData [['height']] == 1 & respData [['treatment']] == 1] <- 'C'
respData [['height']] [respData [['height']] == 1 & respData [['treatment']] != 1] <- 'B'
respData [['height']] [respData [['height']] == 2 & (respData [['treatment']] == 2 | 
                                                     respData [['treatment']] == 3)] <- 'A'
respData [['height']] [respData [['height']] == 2 & respData [['treatment']] == 4] <- 'M'
respData [['height']] [respData [['height']] == 3 & respData [['treatment']] == 4] <- 'A'
respData [['height']] <- factor (respData [['height']], levels = c ('A','M','B','C'))

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux ~ (1 | tree) + factor (date) + factor (date):treatment:height, 
            data = filter (respData, study == 'Exp2017'),
            REML = FALSE)
summary (M1)
plot (M1)
qqnorm (resid (M1))
#========================================================================================