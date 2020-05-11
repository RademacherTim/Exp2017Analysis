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

# Source colour schemes and plotingFunctions
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

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
phi <- arrange (phi, tree, date)

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

# make Supplmentary figure 1 of leaf water potential
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017NeedleWaterPotential.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
summaryPhi <- phi %>% group_by (treatment, date) %>% 
              summarise (weeklyMeans = mean (needles / -10.0, na.rm = TRUE), 
                         weeklySE = sd (needles / -10.0, na.rm = TRUE) / sqrt (10)) %>% 
              arrange (date)
for (i in c (1, 3, 4, 2)) {
  par (mgp = c (3, 2, 0))
  
  # Determine the panel name
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    descriptor <- 'control'
    par (mar = c (5, 8, 1, 0))
  } else if (i == 2) {
    descriptor <- 'girdled'
    par (mar = c (5, 0, 1, 1))
  } else if (i == 3) {
    descriptor <- 'compressed'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 4) {
    descriptor <- 'double compressed'
    par (mar = c (5, 0, 1, 0))
  }
  
  # plot control
  #--------------------------------------------------------------------------------------
  plot (x = as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == 1]),
        y = summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == 1], 
        xlim = as_date (c('2017-06-30','2017-11-10')), ylim = c (0, -1.2), 
        las = 1, typ = 'l', lwd = 2,
        col = ifelse (phi [['treatment']] [i] == 1, tColours [['colour']] [phi [['treatment']] [i]], '#999999'),
        xlab = '', ylab = '', axes = FALSE)
  
  # add shading for uncertainty (plus minus estiamted standard error)
  #--------------------------------------------------------------------------------------
  polygon (x = c (as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == 1]), 
                  rev (as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == 1]))),
           y = c (summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == 1] - 
                  summaryPhi [['weeklySE']] [summaryPhi [['treatment']] == 1], 
                  rev (summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == 1] + 
                         summaryPhi [['weeklySE']] [summaryPhi [['treatment']] == 1])), 
           col = ifelse (i == 1, addOpacity (tColours [['colour']] [i], 0.4), 
                         addOpacity ('#999999', 0.4)), 
           lty = 0)
  
  # plot treatment
  #--------------------------------------------------------------------------------------
  if (i > 1) {
    # add shading for uncertainty (plus minus estiamted standard error)
    #------------------------------------------------------------------------------------
    polygon (x = c (as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == i]), 
                    rev (as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == i]))),
             y = c (summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == i] - 
                      summaryPhi [['weeklySE']] [summaryPhi [['treatment']] == i], 
                    rev (summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == i] + 
                           summaryPhi [['weeklySE']] [summaryPhi [['treatment']] == i])), 
             col = addOpacity (tColours [['colour']] [i], 0.4), lty = 0)
    # add teatment 
    #------------------------------------------------------------------------------------
    lines (x = as_date (summaryPhi [['date']] [summaryPhi [['treatment']] == i]),
          y = summaryPhi [['weeklyMeans']] [summaryPhi [['treatment']] == i], 
          lwd = 2,
          col = ifelse (phi [['treatment']] [i] == 1, 
                        tColours [['colour']] [i], 
                        '#999999'))
  }
  
  # draw vertical lines for critical dates
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-07-05'), col = '#99999999', lty = 2) # start date
  if (i == 3) abline (v = as_date ('2017-08-09'), col = '#99999999', lty = 2) # end date compression
  if (i == 4) abline (v = as_date ('2017-10-08'), col = '#99999999', lty = 2) # end date double compression
  
  # add y-axis
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, at = seq (0, -1, by = -0.2), las = 1, cex.axis = 2.2)
    mtext (expression (paste (Phi[needles],' (MPa)')), side = 2, line = 6, cex = 1.5)
  }
  
  # add x-axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = as_date (c ('2017-07-01', '2017-08-01','2017-09-01','2017-10-01',
                                   '2017-11-01')),
        labels = c ('Jul','Aug','Sep','Oct','Nov'), cex.axis = 2.2)
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-25'), y = -1.1, labels = descriptor, pos = 4, col = '#333333', 
        cex = 2.7)
  
  # draw separating line
  #--------------------------------------------------------------------------------------
  if (i %in% c(1,3)) abline (v = as_date ('2017-11-15'), col = '#999999')
}
dev.off ()
#========================================================================================