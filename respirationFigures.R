#========================================================================================
# Script to make figures with regard to stem CO2 efflux in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# define tree labels for each group
#----------------------------------------------------------------------------------------
controlTrees <- c ( 1,  3,  4,  6,  7,  9, 18, 30, 31, 36)
girdledTrees <- c ( 5, 11, 15, 16, 19, 23, 29, 35, 39, 40)
compresTrees <- c (10, 12, 13, 17, 20, 21, 28, 32, 33, 38)
douCompTrees <- c ( 2,  8, 14, 22, 24, 25, 26, 27, 34, 37)

# Read processed respiration data
#----------------------------------------------------------------------------------------
source ('/home/tim/projects/PlantGrowth/stemCO2Efflux/readProcessedRespData.R')

# get rid of tree 41, respiration data from other experiments, and from irrelevant dates
#----------------------------------------------------------------------------------------
respData <- filter (respData, tree <= 40 & study == 'Exp2017' & 
                      respData [['timestamp']] <= as_datetime ('2018-01-01') & 
                      respData [['timestamp']] > as_datetime ('2017-06-25'))

# Wrangle the data 
#----------------------------------------------------------------------------------------
respData [['date']] <- as_date (respData [['timestamp']])

# Calculate the treatment and sampling height means and standard errors 
#----------------------------------------------------------------------------------------
summaryData <- respData %>% 
  group_by (treatment, chamber, date) %>% 
  summarise (meanResp = mean (fluxRaw, na.rm = TRUE), 
             seResp   = se (fluxRaw), 
             nResp    = sum (!is.na (fluxRaw)))

# Add adjustement ratio to scale control to mean treatment start
#----------------------------------------------------------------------------------------
ratios <- summaryData %>% 
  group_by (treatment) %>% 
  filter (date  == '2017-6-28') %>% 
  summarise (treatmentMean = mean (meanResp)) %>% 
  add_column (controlMean = summaryData [['meanResp']] [1]) %>% 
  mutate (adjRatio = treatmentMean / controlMean)
summaryData <- summaryData %>% mutate (adjRatio = ratios [['adjRatio']] [treatment])

# Plot the respiration rate over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  png ('../fig/Exp2017StemCO2Efflux.png', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 7, 1, 0))
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['date']] [con],
        y = summaryData [['meanResp']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 4, cex = 1.5, expression (paste (CO[2],' efflux (',mu,'mol ',s^-1, m^-2,')')))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'control', cex = 2.7, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 5.1, box.lty = 0, lwd = c (3, 2, 3, 3, 3), lty = c (1, 1, 2, 4, 3), 
          legend = c ('control','adjusted control','above','middle','below'), col = c ('#91b9a4', rep ('#999999', 4)), 
          bg = 'transparent', cex = 2)
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['date']] [con],
        y = summaryData [['meanResp']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 3 & summaryData [['chamber']] == 2
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 3 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'compressed', cex = 2.8, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['date']] [con],
        y = summaryData [['meanResp']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add double compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 3
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 2
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'double compressed', cex = 2.8, 
        col = '#333333')
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['date']] [con],
        y = summaryData [['meanResp']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 2 & summaryData [['chamber']] == 2
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 2 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'girdled', cex = 2.8, 
        col = '#333333')
  
  dev.off  ()
}

PLOT <- TRUE; if (PLOT) {
  png ('../fig/Exp2017StemCO2EffluxAdjusted.png', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 7, 1, 0))
  con1 <- summaryData [['treatment']] == 1
  plot (x = summaryData [['date']] [con1],
        y = summaryData [['meanResp']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  polygon (x = c (summaryData [['date']] [con1], 
                  rev (summaryData [['date']] [con1])),
           y = c (summaryData [['meanResp']] [con1] - summaryData [['seResp']] [con1], 
                  rev (summaryData [['meanResp']] [con1] + summaryData [['seResp']] [con1])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con1], 
         y = summaryData [['meanResp']] [con1],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 4, cex = 1.5, expression (paste (CO[2],' efflux (',mu,'mol ',s^-1, m^-2,')')))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'control', cex = 2.7, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 5.1, box.lty = 0, lwd = c (3, 2, 3, 3, 3), lty = c (1, 1, 2, 4, 3), 
          legend = c ('control','adjusted control','above','middle','below'), col = c ('#91b9a4', rep ('#999999', 4)),  
          bg = 'transparent', cex = 2)
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryData [['treatment']] == 3 & summaryData [['chamber']] == 2
  plot (x = summaryData [['date']] [con1],
        y = summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20), 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con1], 
                  rev (summaryData [['date']] [con1])),
           y = c (summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20) - summaryData [['seResp']] [con1], 
                  rev (summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20) + summaryData [['seResp']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)

  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 3 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'compressed', cex = 2.8, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 3
  plot (x = summaryData [['date']] [con1],
        y = summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20), 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con1], 
                  rev (summaryData [['date']] [con1])),
           y = c (summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20) - summaryData [['seResp']] [con1], 
                  rev (summaryData [['meanResp']] [con1] * rep (unique (summaryData [['adjRatio']] [con]), 20) + summaryData [['seResp']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add double compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 2
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryData [['treatment']] == 4 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'double compressed', cex = 2.8, 
        col = '#333333')

  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryData [['treatment']] == 2 & summaryData [['chamber']] == 2
  plot (x = summaryData [['date']] [con1],
        y = summaryData [['meanResp']] [con1] * summaryData [['adjRatio']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryData [['date']] [con1], 
                  rev (summaryData [['date']] [con1])),
           y = c (summaryData [['meanResp']] [con1] * summaryData [['adjRatio']] [con] - summaryData [['seResp']] [con1], 
                  rev (summaryData [['meanResp']] [con1] * summaryData [['adjRatio']] [con] + summaryData [['seResp']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryData [['treatment']] == 2 & summaryData [['chamber']] == 1
  polygon (x = c (summaryData [['date']] [con], 
                  rev (summaryData [['date']] [con])),
           y = c (summaryData [['meanResp']] [con] - summaryData [['seResp']] [con], 
                  rev (summaryData [['meanResp']] [con] + summaryData [['seResp']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryData [['date']] [con], 
         y = summaryData [['meanResp']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = 'girdled', cex = 2.8, 
        col = '#333333')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  dev.off  ()
}
#========================================================================================

