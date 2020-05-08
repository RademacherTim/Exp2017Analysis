#========================================================================================
# Script to plot collar pressure over time for teh experiment at Harvard Forest in 2017 
#----------------------------------------------------------------------------------------


# Load dependencies 
#----------------------------------------------------------------------------------------
library ('readxl')
library ('tidyverse')
library ('lubridate')

# Source colour schemes and plotingFunctions
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Read in pressure data from spreadsheets 
#----------------------------------------------------------------------------------------
suppressWarnings (phi2017 <- read_excel (path = '/media/tim/dataDisk/PlantGrowth/data/pressureSensors/pressureOfCollarsExp2017.xlsx',
                                         sheet = 'pressure'))
phiCir <- read_csv ('/media/tim/dataDisk/PlantGrowth/data/pressureSensors/circumferentialPressureDistribution.csv',
                    col_types = cols ())

# get sampling dates
#----------------------------------------------------------------------------------------
dates <- as.POSIXct (names (phi2017 [, 5:dim (phi2017) [2]]), 
                     format = '%Y/%m/%d', tz = 'EST')
# Set -9999 to NA
#----------------------------------------------------------------------------------------
phi2017 [phi2017 == -9999] <- NA

# Change treatment to a number
#----------------------------------------------------------------------------------------
phi2017 [['treatment']] [phi2017 [['treatment']] == "compression"] <- 3
phi2017 [['treatment']] [phi2017 [['treatment']] == "double compression"] <- 4
phi2017 [['treatment']] <- as.numeric (phi2017 [['treatment']])

# Calculate the average collar pressure
#----------------------------------------------------------------------------------------
phi <- phi2017 %>% pivot_longer (cols = 5:21, names_to = 'date', values_to = 'phi') %>% 
  group_by (tree, treatment, collar, date) %>% 
  summarise (meanCollarPhi = mean (phi, na.rm = TRUE))

# Calculate the treatment average and standard error
#----------------------------------------------------------------------------------------
summaryData <- phi %>% group_by (treatment, collar, date) %>% 
  summarise (meanTreatmentPhi = mean (meanCollarPhi, na.rm = TRUE), 
             seTreatmentPhi = se (meanCollarPhi),
             sdTreatmentPhi = sd (meanCollarPhi, na.rm = TRUE))

# Wrangle the dates
#----------------------------------------------------------------------------------------
summaryData [['date']] [c (7:8, 24:25, 41:42)] <- '2017-08-13'
summaryData [['date']] <- as_date (summaryData [['date']])

# Plot the collor pressures over time
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017CompressionCollarPressure.png', width = 900, height = 380)
layout (matrix (1:2, nrow = 1, byrow = TRUE), widths  = c (1.2, 1))
par (mar = c (6, 6, 1, 0))
con <-  summaryData [['treatment']] == 3        & 
        summaryData [['collar']]    == 'middle' & 
        !is.na (summaryData [['meanTreatmentPhi']])
plot (x = summaryData [['date']] [con],
      y = summaryData [['meanTreatmentPhi']] [con], typ = 'l', axes = FALSE, 
      col = tColours [['colour']] [3], lwd = 3,
      xlim = as_date (c('2017-06-30','2017-11-10')), ylim = c (0, 6), 
      xlab = '', ylab = '', cex.lab = 1.5) 
mtext ('date', side = 1, line = 4, cex = 1.5)
mtext ('normal pressure (MPa)', side = 2, line = 4, cex = 1.5)
axis (side = 1, at = as_date (c ('2017-07-01', '2017-08-01','2017-09-01','2017-10-01',
                                 '2017-11-01')),
      labels = c ('Jul','Aug','Sep','Oct','Nov'), cex.axis = 1.5)
polygon (x = c (summaryData [['date']] [con], rev (summaryData [['date']] [con])),
         y = c (summaryData [['meanTreatmentPhi']] [con] - 
                  summaryData [['seTreatmentPhi']] [con], 
                rev (summaryData [['meanTreatmentPhi']] [con] + 
                       summaryData [['seTreatmentPhi']] [con])),
         col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)

# Add lower collar of double compression
#----------------------------------------------------------------------------------------
con <-  summaryData [['treatment']] == 4        & 
  summaryData [['collar']]    == 'lower' & 
  !is.na (summaryData [['meanTreatmentPhi']])
polygon (x = c (summaryData [['date']] [con], rev (summaryData [['date']] [con])),
         y = c (summaryData [['meanTreatmentPhi']] [con] - 
                  summaryData [['seTreatmentPhi']] [con], 
                rev (summaryData [['meanTreatmentPhi']] [con] + 
                       summaryData [['seTreatmentPhi']] [con])),
         col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
lines (x = as_date (summaryData [['date']] [con]),
       y = summaryData [['meanTreatmentPhi']] [con], lty = 2, 
       col = tColours [['colour']] [4], lwd = 3)

# Add upper collar of double compression
#----------------------------------------------------------------------------------------
con <-  summaryData [['treatment']] == 4        & 
  summaryData [['collar']]    == 'upper' & 
  !is.na (summaryData [['meanTreatmentPhi']])
polygon (x = c (summaryData [['date']] [con], rev (summaryData [['date']] [con])),
         y = c (summaryData [['meanTreatmentPhi']] [con] - 
                  summaryData [['seTreatmentPhi']] [con], 
                rev (summaryData [['meanTreatmentPhi']] [con] + 
                       summaryData [['seTreatmentPhi']] [con])),
         col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
lines (x = as_date (summaryData [['date']] [con]),
       y = summaryData [['meanTreatmentPhi']] [con], lty = 1, 
       col = tColours [['colour']] [4], lwd = 3)

# Add y-axis
#--------------------------------------------------------------------------------------
axis (side = 2, las = 1, cex.axis = 1.5)

# Add legend 
#----------------------------------------------------------------------------------------
legend (x = as_date ('2017-07-20'), y = 6.2, box.lty = 0, bg = 'transparent', lty = c (1, 2, 1),
        legend = c ('single compression', 'double compression lower', 'double compression upper'),
col = tColours [['colour']] [c (3,4,4)], lwd = 3, cex = 1.3)

# Plot separating line
#--------------------------------------------------------------------------------------
abline (v = as_date ('2017-11-10'), col = '#999999')


# Add panel descriptor
#----------------------------------------------------------------------------------------
text (x = as_date ('2017-06-30'), y = 6.0, labels = 'a', cex = 2.0)

# Add critical dates
#--------------------------------------------------------------------------------------
return <- criticalDates ('double compressed') 
return <- criticalDates ('compressed') 

# Plot circumferential pressure
#--------------------------------------------------------------------------------------
par (mar = c (6, 0, 1, 0))
plot (x = phiCir [['angle']],
      y = phiCir [['pressure']], axes = FALSE, typ = 'l', lwd = 2, lty = 2,
      col = '#99999999', xlab = '', ylim = c (0, 6.0),
      ylab = '', cex.lab = 2.2)
mtext (expression (paste ('angle (',degree,')')), side = 1, line = 4, cex = 1.5)
#mtext ('normal pressure (MPa)', side = 2, line = 6, cex = 1.5)
lines (x = phiCir [['angle']],
       y = phiCir [['pressure offset by 180']], lwd = 2, lty = 3,
       col = '#99999999')
lines (x = phiCir [['angle']],
       y = phiCir [['max pressure combined']], lwd = 3, lty = 1,
       col = tColours [['colour']] [2])
axis (side = 1, at = seq (0, 300, by = 60), cex.axis = 1.5)

# Add a legend 
#----------------------------------------------------------------------------------------
legend (x = 0, y = 6.2, box.lty = 0, bg = 'transparent', lty = 1:3, lwd = c (3,2,2),
        col = c (tColours [['colour']] [2], rep ('#99999999', 2)),
        legend = c ('combined pressure','belt', 'opposing belt'), cex = 1.3)

# Add panel descriptor
#----------------------------------------------------------------------------------------
text (x = -10, y = 6.0, labels = 'b', cex = 2.0) 
dev.off ()
#========================================================================================