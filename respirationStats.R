#========================================================================================
# This script test for differences between treatments and sampling height in 
# stem CO2 efflux for the 2017 experiment at Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('/home/tim/projects/PlantGrowth/stemCO2Efflux/readProcessedRespData.R')

# get colours and opacity function
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# get rid of tree 41 
#----------------------------------------------------------------------------------------
respData <- filter (respData, tree <= 40 & study == 'Exp2017' & 
                      respData [['timestamp']] <= as_datetime ('2018-01-01') & 
                      respData [['timestamp']] > as_datetime ('2017-06-25'))

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
            data = respData,
            REML = FALSE)
summary (M1)

# get list of measurements dates
#----------------------------------------------------------------------------------------
dates <- unique (respData [['date']])

# extract the model parameters from the lumen diameter model
#----------------------------------------------------------------------------------------
M01Values <- tibble (beta   = getME (M1, 'beta'), 
                     se     = as.numeric (coef (summary (M1)) [, 2]), 
                     tValue = as.numeric (coef (summary (M1)) [, 3]))
M01Values <- add_column (M01Values, .before = 1,
                         treatment = c (rep (1, 20), rep (4, 19),    rep (3, 20), 
                                        rep (2, 19), rep (4, 19+19), rep (3, 20), 
                                        rep (2, 19)),
                         height = c (rep (21, 20), rep (24, 19+20+19), rep (22, 19), 
                                     rep (25, 19+20+19)),
                         date = c (dates, dates [-11], dates,  
                                   dates [-11], dates [-11],  
                                   dates [-11], dates, 
                                   dates [-11]))

# add the date effect
#----------------------------------------------------------------------------------------
for (iDate in dates) {
  M01Values [['beta']] [M01Values [['date']] == iDate] [2:length (M01Values [['beta']] [M01Values [['date']] == iDate])] <- 
    M01Values [['beta']] [M01Values [['date']] == iDate] [2:length (M01Values [['beta']] [M01Values [['date']] == iDate])] + 
    M01Values [['beta']] [M01Values [['date']] == iDate] [1]
}

# add the intercept
#----------------------------------------------------------------------------------------
M01Values [['beta']] [2:length (M01Values [['beta']])] <-
  M01Values [['beta']] [2:length (M01Values [['beta']])] + M01Values [['beta']] [1]

# plot respiration data
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemCO2Efflux.png', width = 600, height = 400)
  layout (matrix (1:4, nrow = 2, byrow = TRUE), widths  = c (1.2, 1, 1.2, 1), 
          heights = c (1, 1.3))
  par (mar = c (0, 5, 1, 0))
  plot (x = M01Values [['date']] [M01Values [['treatment']] == 1],
        y = M01Values [['beta']] [M01Values [['treatment']] == 1],
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (-0.3, 5.5),
        typ = 'l', col = tColours [['colour']] [1], axes = FALSE, cex.lab = 1.2,
        las = 1, xlab = '', lwd = 2,
        ylab = expression (paste (CO[2],' efflux (',mu,' mol ',s^-1, m^-2,')')))
  
  # draw vertical lines for critical dates
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-07-03'), col = '#99999999', lty = 2) # start date
  
  # add shading for uncertainty (plus minus estiamted standard error)
  #--------------------------------------------------------------------------------------
  polygon (x = c (M01Values [['date']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['date']] [M01Values [['treatment']] == 1])),
           y = c (M01Values [['beta']] [M01Values [['treatment']] == 1] - 
                  M01Values [['se']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['beta']] [M01Values [['treatment']] == 1] + 
                       M01Values [['se']] [M01Values [['treatment']] == 1])), 
                  col = addOpacity (tColours [['colour']] [1], 0.4), lty = 0)
  
  # add y-axis 
  axis (side = 2, at = 0:5, las = 1, cex.axis = 1.3)
  
  # add panel descriptor
  text (x = as_date ('2017-06-20'), y = 5.4, labels = 'control', pos = 4, col = '#333333', 
        cex = 1.5)
  
  # draw separating line
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  abline (h = -0.5, col = '#999999')
  
  # add legend 
  legend (x = as_date ('2017-09-10'), y = 5.4, box.lty = 0, lwd = 2, lty = c (1, 2, 4, 3), 
          legend = c ('control','above','middle','below'), col = '#999999', 
          bg = 'transparent')
  
  # plot girdled group
  par (mar = c (0, 0, 1, 1))
  plot (x = M01Values [['date']] [M01Values [['treatment']] == 1],
        y = M01Values [['beta']] [M01Values [['treatment']] == 1],
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (-0.3, 5.5),
        typ = 'l', col = '#999999', axes = FALSE, 
        las = 1, xlab = '', lwd = 1,
        ylab = '')
  
  # draw vertical lines for critical dates
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-07-03'), col = '#99999999', lty = 2) # start date
  
  polygon (x = c (M01Values [['date']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['date']] [M01Values [['treatment']] == 1])),
           y = c (M01Values [['beta']] [M01Values [['treatment']] == 1] - 
                    M01Values [['se']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['beta']] [M01Values [['treatment']] == 1] + 
                         M01Values [['se']] [M01Values [['treatment']] == 1])), 
           col = addOpacity ('#999999', 0.3), lty = 0)
  con <- M01Values [['treatment']] == 2 & M01Values [['height']] == 25
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 25, col = tColours [['colour']] [2], lty = 3, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  
  # plot above the girdle 
  con <- M01Values [['treatment']] == 2 & M01Values [['height']] == 24
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 24, col = tColours [['colour']] [2], lty = 2, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  
  # add panel descriptor
  text (x = as_date ('2017-06-20'), y = 5.4, labels = 'girdled', pos = 4, col = '#333333', 
        cex = 1.5)
  
  # draw separating line
  abline (h = -0.5, col = '#999999')
  
  # plot compressed group
  par (mar = c (5, 5, 0, 0))
  plot (x = M01Values [['date']] [M01Values [['treatment']] == 1],
        y = M01Values [['beta']] [M01Values [['treatment']] == 1],
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.7),
        typ = 'l', col = '#999999', axes = FALSE, cex.lab = 1.2,
        las = 1, xlab = 'date', lwd = 1,
        ylab = expression (paste (CO[2],' efflux (',mu,' mol ',s^-1, m^-2,')')))
  
  # draw vertical lines for critical dates
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-07-03'), col = '#99999999', lty = 2) # start date
  abline (v = as_date ('2017-08-09'), col = '#99999999', lty = 2) # end date compression
  
  polygon (x = c (M01Values [['date']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['date']] [M01Values [['treatment']] == 1])),
           y = c (M01Values [['beta']] [M01Values [['treatment']] == 1] - 
                    M01Values [['se']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['beta']] [M01Values [['treatment']] == 1] + 
                         M01Values [['se']] [M01Values [['treatment']] == 1])), 
           col = addOpacity ('#999999', 0.3), lty = 0)
  con <- M01Values [['treatment']] == 3 & M01Values [['height']] == 25
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 25, col = tColours [['colour']] [3], lty = 3, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  
  # plot above the compression 
  con <- M01Values [['treatment']] == 3 & M01Values [['height']] == 24
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 24, col = tColours [['colour']] [3], lty = 2, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  
  # add x-axis 
  axis (side = 1, at = as_date (c ('2017-07-01', '2017-08-01','2017-09-01','2017-10-01',
                                   '2017-11-01')),
        labels = c ('jul','aug','sep','oct','nov'), cex.axis = 1.3)
  
  # add y-axis 
  axis (side = 2, at = 0:5, las = 1, cex.axis = 1.3)
  
  # add panel descriptor
  text (x = as_date ('2017-06-20'), y = 5.4, labels = 'compressed', pos = 4, col ='#333333', 
        cex = 1.5)
  
  # draw separating line
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # plot double compressed group
  par (mar = c (5, 0, 0, 1))
  plot (x = M01Values [['date']] [M01Values [['treatment']] == 1],
        y = M01Values [['beta']] [M01Values [['treatment']] == 1],
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.7),
        typ = 'l', col = '#999999', axes = FALSE, cex.lab = 1.2,
        las = 1, xlab = 'date', lwd = 1,
        ylab = '')
  
  # draw vertical lines for critical dates
  #--------------------------------------------------------------------------------------
  abline (v = as_date ('2017-07-03'), col = '#99999999', lty = 2) # start date
  abline (v = as_date ('2017-10-08'), col = '#99999999', lty = 2) # end date double compression
  
  polygon (x = c (M01Values [['date']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['date']] [M01Values [['treatment']] == 1])),
           y = c (M01Values [['beta']] [M01Values [['treatment']] == 1] - 
                    M01Values [['se']] [M01Values [['treatment']] == 1], 
                  rev (M01Values [['beta']] [M01Values [['treatment']] == 1] + 
                         M01Values [['se']] [M01Values [['treatment']] == 1])), 
           col = addOpacity ('#999999', 0.3), lty = 0)
  con <- M01Values [['treatment']] == 4 & M01Values [['height']] == 25
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 25, col = tColours [['colour']] [4], lty = 3, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  
  # plot inbetwen the double compression 
  con <- M01Values [['treatment']] == 4 & M01Values [['height']] == 22
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 22, col = tColours [['colour']] [4], lty = 4, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  
  # plot above the double compression 
  con <- M01Values [['treatment']] == 4 & M01Values [['height']] == 24
  lines (x = M01Values [['date']] [con],
         y = M01Values [['beta']] [con],
         pch = 24, col = tColours [['colour']] [4], lty = 2, lwd = 2)
  polygon (x = c (M01Values [['date']] [con], rev (M01Values [['date']] [con])),
           y = c (M01Values [['beta']] [con] - M01Values [['se']] [con], 
                  rev (M01Values [['beta']] [con] + M01Values [['se']] [con])), 
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  
  # add x-axis 
  axis (side = 1, at = as_date (c ('2017-07-01', '2017-08-01','2017-09-01','2017-10-01',
                                   '2017-11-01')),
        labels = c ('jul','aug','sep','oct','nov'), cex.axis = 1.3)
  
  # add panel descriptor
  text (x = as_date ('2017-06-20'), y = 5.4, labels = 'double compressed', pos = 4, 
        col = '#333333', cex = 1.5)
dev.off ()

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

#========================================================================================