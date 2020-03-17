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

# Get rid of tree 41 
#----------------------------------------------------------------------------------------
respData <- filter (respData, tree <= 40)

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
M1 <- lmer (formula = fluxRaw ~ (1 | tree) + factor (date) + factor (date):treatment:height, 
            data = filter (respData, study == 'Exp2017'),
            REML = FALSE)
summary (M1)
plot (M1)
qqnorm (resid (M1))

# divide period into before, during and after
#----------------------------------------------------------------------------------------
respData [['period']] <- 3 
respData [['period']] [respData [['date']] <= as_date ('2017-10-09')] <- 2
respData [['period']] [respData [['date']] <= as_date ('2017-08-09')] <- 1
respData [['period']] [respData [['date']] <= as_date ('2017-07-05')] <- 0
respData [['period']] <- factor (respData [['period']])

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = flux ~ (1 | tree) + period + period:treatment:height, 
            data = filter (respData, study == 'Exp2017'),
            REML = FALSE)
summary (M2)
plot (M2)
qqnorm (resid (M2))

#========================================================================================