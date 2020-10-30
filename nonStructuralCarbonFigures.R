#========================================================================================
# Script to make figures with regard to nonstructural carbon in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# read the sugar and starch concentration (means for needles and roots and first centimeter for wood sections)
#----------------------------------------------------------------------------------------
suppressMessages (source ('/home/tim/projects/PlantGrowth/nonstructuralCarbon/processExpNSCData.R'))

# remove data for tree 41
#----------------------------------------------------------------------------------------
leafData2017 <- filter (leafData2017, treeID != 41)  
stemData2017 <- filter (stemData2017, treeID != 41)  
rootData2017 <- filter (rootData2017, treeID != 41)  

# change date and time of collection to just date
#----------------------------------------------------------------------------------------
leafData2017 [['date']] <- as_date (leafData2017 [['date']])
stemData2017 [['date']] <- as_date (stemData2017 [['date']])
rootData2017 [['date']] <- as_date (rootData2017 [['date']])

# Estimate treatment (and sampling height) mean and standard error of leaf, wood and root 
# sugar and starch concentrations 
#----------------------------------------------------------------------------------------
summaryDataLeaf <- leafData2017 %>% group_by (date, treatment) %>% 
  summarise (meanSugar  = mean (sugar,  na.rm = T), seSugar  = se (sugar),  nSugar  = sum (!is.na (sugar)),
             meanStarch = mean (starch, na.rm = T), seStarch = se (starch), nStarch = sum (!is.na (starch)))
summaryDataStem <- stemData2017 %>% group_by (date, treatment, sampleHeight, sampleDepth) %>% 
  summarise (meanSugar  = mean (sugar,  na.rm = T), seSugar  = se (sugar),  nSugar  = sum (!is.na (sugar)),
             meanStarch = mean (starch, na.rm = T), seStarch = se (starch), nStarch = sum (!is.na (starch)))
summaryDataRoot <- rootData2017 %>% group_by (date, treatment) %>% 
  summarise (meanSugar  = mean (sugar,  na.rm = T), seSugar  = se (sugar),  nSugar  = sum (!is.na (sugar)),
             meanStarch = mean (starch, na.rm = T), seStarch = se (starch), nStarch = sum (!is.na (starch)))

# Add adjustment ratio for sugar and starch to scale control baseline to treatment baseline
#----------------------------------------------------------------------------------------
adjRatiosSugar  <- summaryDataLeaf [['meanSugar']]  [1:4] / summaryDataLeaf [['meanSugar']]  [1]
adjRatiosStarch <- summaryDataLeaf [['meanStarch']] [1:4] / summaryDataLeaf [['meanStarch']] [1]
summaryDataLeaf  <- summaryDataLeaf %>% mutate (adjRatioSugar  = adjRatiosSugar  [treatment],
                                                adjRatioStarch = adjRatiosStarch [treatment])
ratios0 <- summaryDataStem %>% filter (date == '2017-07-05', sampleDepth == 1) %>% 
  group_by (treatment) %>% mutate (treatmentMeanSugar = mean (meanSugar), 
                                                treatmentMeanStarch = mean (meanStarch)) %>%
  add_column (controlMeanSugar  = summaryDataStem [['meanSugar']]  [1], 
              controlMeanStarch = summaryDataStem [['meanStarch']] [1]) %>%  
  mutate (adjRatioSugar  = treatmentMeanSugar  / controlMeanSugar, 
          adjRatioStarch = treatmentMeanStarch / controlMeanStarch) %>% ungroup %>% 
  select (adjRatioSugar, adjRatioStarch)
summaryDataStem0 <- summaryDataStem %>% filter (sampleDepth == 1) %>% 
  add_column (adjRatioSugar  = rep (ratios0 [['adjRatioSugar']],  4), 
              adjRatioStarch = rep (ratios0 [['adjRatioStarch']], 4))
ratios1 <- summaryDataStem %>% filter (date == '2017-07-05', sampleDepth == 2) %>% 
  group_by (treatment) %>% mutate (treatmentMeanSugar  = mean (meanSugar), 
                                   treatmentMeanStarch = mean (meanStarch)) %>%
  add_column (controlMeanSugar  = summaryDataStem [['meanSugar']]  [1], 
              controlMeanStarch = summaryDataStem [['meanStarch']] [1]) %>%  
  mutate (adjRatioSugar  = treatmentMeanSugar  / controlMeanSugar, 
          adjRatioStarch = treatmentMeanStarch / controlMeanStarch) %>% ungroup %>% 
  select (adjRatioSugar, adjRatioStarch)
summaryDataStem1 <- summaryDataStem %>% filter (sampleDepth == 2) %>% 
  add_column (adjRatioSugar  = rep (ratios0 [['adjRatioSugar']],  2), 
              adjRatioStarch = rep (ratios0 [['adjRatioStarch']], 2))
adjRatiosSugar  <- summaryDataRoot [['meanSugar']]  [1:4] / summaryDataRoot [['meanSugar']]  [1]
adjRatiosStarch <- summaryDataRoot [['meanStarch']] [1:4] / summaryDataRoot [['meanStarch']] [1]
summaryDataRoot  <- summaryDataRoot %>% mutate (adjRatioSugar = adjRatiosSugar [treatment],
                                                adjRatioStarch = adjRatiosStarch [treatment])

# Plot leaf soluble sugar concentrations over time
#----------------------------------------------------------------------------------------
tiff ('../fig/Exp2017NeedleSugarConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in c (1, 3, 4, 2)) {
  if (i == 1) {
    par (mar = c (3, 12, 1, 0))
    descriptor <- 'control'
  } else if (i %in% 2:3) {
    par (mar = c (3, 0, 1, 0))
    if (i == 2) descriptor <- 'girdled'
    if (i == 3) descriptor <- 'compressed'
  } else {
    par (mar = c (3, 0, 1, 1))
    descriptor <- 'double compressed'
  }
  con  <- summaryDataLeaf [['treatment']] == i
  con1 <- summaryDataLeaf [['treatment']] == 1
  plot (x = summaryDataLeaf [['date']] [con],
        y = summaryDataLeaf [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 12), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataLeaf [['date']] [con1], 
                    rev (summaryDataLeaf [['date']] [con1])),
             y = c (summaryDataLeaf [['meanSugar']] [con1] * summaryDataLeaf [['adjRatioSugar']] [con] - summaryDataLeaf [['seSugar']] [con1], 
                    rev (summaryDataLeaf [['meanSugar']] [con1] * summaryDataLeaf [['adjRatioSugar']] [con] + summaryDataLeaf [['seSugar']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataLeaf [['date']] [con1], 
           y = summaryDataLeaf [['meanSugar']] [con1] * summaryDataLeaf [['adjRatioSugar']] [con1],
           col = '#999999', lwd = 2)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  
  # Add actual treatment
  #--------------------------------------------------------------------------------------
  polygon (x = c (summaryDataLeaf [['date']] [con], 
                  rev (summaryDataLeaf [['date']] [con])),
           y = c (summaryDataLeaf [['meanSugar']] [con] - summaryDataLeaf [['seSugar']] [con], 
                  rev (summaryDataLeaf [['meanSugar']] [con] + summaryDataLeaf [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
  lines (x = summaryDataLeaf [['date']] [con], 
         y = summaryDataLeaf [['meanSugar']] [con],
         col = tColours [['colour']] [i], lwd = 3, lty = ifelse (i == 1, 1, 2))
  
  # Add axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'Sugar concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'Needles')
  } else {
    axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 12, by = 2), labels = rep ('', 7))
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 12.5, pos = 1, labels = descriptor, cex = 3.2, 
        col = '#333333')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 6, box.lty = 0, 
                      lwd = c (3, 2, 3, 3, 3), lty = c (1, 1, 2, 4, 3), 
                      legend = c ('control','adjusted control','above','middle','below'), 
                      col = c ('#91b9a4',rep ('#999999', 4)), 
                      bg = 'transparent', cex = 2.3)
}
dev.off  ()

# Plot leaf starch concentrations over time
#----------------------------------------------------------------------------------------
tiff ('../fig/Exp2017NeedleStarchConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in c (1, 3, 4, 2)) {
  if (i == 1) {
    par (mar = c (3, 12, 1, 0))
    descriptor <- 'control'
  } else if (i %in% 2:3) {
    par (mar = c (3, 0, 1, 0))
    if (i == 2) descriptor <- 'girdled'
    if (i == 3) descriptor <- 'compressed'
  } else {
    par (mar = c (3, 0, 1, 1))
    descriptor <- 'double compressed'
  }
  con  <- summaryDataLeaf [['treatment']] == i
  con1 <- summaryDataLeaf [['treatment']] == 1
  plot (x = summaryDataLeaf [['date']] [con],
        y = summaryDataLeaf [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 4.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataLeaf [['date']] [con1], 
                    rev (summaryDataLeaf [['date']] [con1])),
             y = c (summaryDataLeaf [['meanStarch']] [con1] * summaryDataLeaf [['adjRatioStarch']] [con] - summaryDataLeaf [['seStarch']] [con1], 
                    rev (summaryDataLeaf [['meanStarch']] [con1] * summaryDataLeaf [['adjRatioStarch']] [con] + summaryDataLeaf [['seStarch']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataLeaf [['date']] [con1], 
           y = summaryDataLeaf [['meanStarch']] [con1] * summaryDataLeaf [['adjRatioStarch']] [con],
           col = '#999999', lwd = 2)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  
  # Add actual treatment
  #--------------------------------------------------------------------------------------
  polygon (x = c (summaryDataLeaf [['date']] [con], 
                  rev (summaryDataLeaf [['date']] [con])),
           y = c (summaryDataLeaf [['meanStarch']] [con] - summaryDataLeaf [['seStarch']] [con], 
                  rev (summaryDataLeaf [['meanStarch']] [con] + summaryDataLeaf [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
  lines (x = summaryDataLeaf [['date']] [con], 
         y = summaryDataLeaf [['meanStarch']] [con],
         col = tColours [['colour']] [i], lwd = 3, lty = ifelse (i == 1, 1, 2))
  
  # Add axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'Starch concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'Needles')
  } else {
    axis (side = 2, cex.axis = 2.2, las = 1, at = 0:4, labels = rep ('', 5))
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 4.7, pos = 1, labels = descriptor, cex = 3.2, 
        col = '#333333')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 4.2, box.lty = 0, 
                      lwd = c (3, 2, 3, 3, 3), lty = c (1, 1, 2, 4, 3), 
                      legend = c ('control','adjusted control','above','middle','below'), 
                      col = c ('#91b9a4',rep ('#999999', 4)), 
                      bg = 'transparent', cex = 2.3)
}
dev.off  ()

# Plot wood soluble sugar concentrations for 0-1 cm over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  tiff ('../fig/Exp2017StemSugarConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (3, 12, 1, 0))
  con1 <- summaryDataStem0 [['treatment']] == 1
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanSugar']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanSugar']] [con1] - summaryDataStem0 [['seSugar']] [con1], 
                  rev (summaryDataStem0 [['meanSugar']] [con1] + summaryDataStem0 [['seSugar']] [con1])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con1], 
         y = summaryDataStem0 [['meanSugar']] [con1],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 5, cex = 1.5, 'Sugar concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'Wood')
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (3, 0, 1, 0))
  con <- summaryDataStem0 [['treatment']] == 3 & summaryDataStem0 [['sampleHeight']] == 2
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] - summaryDataStem0 [['seSugar']] [con1], 
                  rev (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] + summaryDataStem0 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)

  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add compressed trees
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 3 & summaryDataStem0 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 1.5, by = 0.5), labels = rep ('', 4))
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (3, 0, 1, 0))
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 2.5
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] - summaryDataStem0 [['seSugar']] [con1], 
                  rev (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] + summaryDataStem0 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)

    # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add double compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 1.5, by = 0.5), labels = rep ('', 4))
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (3, 0, 1, 0))
  con <- summaryDataStem0 [['treatment']] == 2 & summaryDataStem0 [['sampleHeight']] == 2
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] - summaryDataStem0 [['seSugar']] [con1], 
                  rev (summaryDataStem0 [['meanSugar']] [con1] * summaryDataStem0 [['adjRatioSugar']] [con] + summaryDataStem0 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)

    # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem0 [['treatment']] == 2 & summaryDataStem0 [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 2 & summaryDataStem0 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanSugar']] [con] - summaryDataStem0 [['seSugar']] [con], 
                  rev (summaryDataStem0 [['meanSugar']] [con] + summaryDataStem0 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 1.5, by = 0.5), labels = rep ('', 4))
  
  dev.off  ()
}

# Plot wood starch concentrations for 0-1 cm over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  tiff ('../fig/Exp2017StemStarchConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (3, 12, 1, 0))
  con1 <- summaryDataStem0 [['treatment']] == 1
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanStarch']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 0.8), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #----------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  # Add actual group
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanStarch']] [con1] - summaryDataStem0 [['seStarch']] [con1], 
                  rev (summaryDataStem0 [['meanStarch']] [con1] + summaryDataStem0 [['seStarch']] [con1])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con1], 
         y = summaryDataStem0 [['meanStarch']] [con1],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 0.8, by = 0.2))
  mtext (side = 2, line = 5, cex = 1.5, 'Starch concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'Wood')
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 0.8, pos = 1, labels = 'control', cex = 3.2, 
        col = '#333333')
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem0 [['treatment']] == 3 & summaryDataStem0 [['sampleHeight']] == 2
  par (mar = c (3, 0, 1, 0))
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 0.8), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] - summaryDataStem0 [['seStarch']] [con1], 
                  rev (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] + summaryDataStem0 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 3 & summaryDataStem0 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 0.8, by = 0.2), labels = rep ('', 5))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 0.8, pos = 1, labels = 'compressed', cex = 3.2, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (3, 0, 1, 0))
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 2.5
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 0.8), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] - summaryDataStem0 [['seStarch']] [con1], 
                  rev (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] + summaryDataStem0 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 2.5
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem0 [['treatment']] == 4 & summaryDataStem0 [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 0.8, by = 0.2), labels = rep ('', 5))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 0.8, pos = 1, labels = 'double compressed', cex = 3.2, 
        col = '#333333')
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (3, 0, 1, 1))
  con <- summaryDataStem0 [['treatment']] == 2 & summaryDataStem0 [['sampleHeight']] == 2
  plot (x = summaryDataStem0 [['date']] [con1],
        y = summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 0.8), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem0 [['date']] [con1], 
                  rev (summaryDataStem0 [['date']] [con1])),
           y = c (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] - summaryDataStem0 [['seStarch']] [con1], 
                  rev (summaryDataStem0 [['meanStarch']] [con1] * summaryDataStem0 [['adjRatioStarch']] [con] + summaryDataStem0 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem0 [['treatment']] == 2 & summaryDataStem0 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem0 [['date']] [con], 
                  rev (summaryDataStem0 [['date']] [con])),
           y = c (summaryDataStem0 [['meanStarch']] [con] - summaryDataStem0 [['seStarch']] [con], 
                  rev (summaryDataStem0 [['meanStarch']] [con] + summaryDataStem0 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem0 [['date']] [con], 
         y = summaryDataStem0 [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = rep ('', 5),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1, at = seq (0, 0.8, by = 0.2), labels = rep ('', 5))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 0.8, pos = 1, labels = 'girdled', cex = 3.2, 
        col = '#333333')
  
  dev.off  ()
}

# Plot root soluble sugar concentrations over time
#----------------------------------------------------------------------------------------
tiff ('../fig/Exp2017RootSugarConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in c (1, 3, 4, 2)) {
  if (i == 1) {
    par (mar = c (5, 12, 1, 0))
    descriptor <- 'control'
  } else if (i %in% 2:3) {
    par (mar = c (5, 0, 1, 0))
    if (i == 2) descriptor <- 'girdled'
    if (i == 3) descriptor <- 'compressed'
  } else {
    par (mar = c (5, 0, 1, 1))
    descriptor <- 'double compressed'
  }
  con  <- summaryDataRoot [['treatment']] == i
  con1 <- summaryDataRoot [['treatment']] == 1
  plot (x = summaryDataRoot [['date']] [con],
        y = summaryDataRoot [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 2.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataRoot [['date']] [con1], 
                    rev (summaryDataRoot [['date']] [con1])),
             y = c (summaryDataRoot [['meanSugar']] [con1] * summaryDataRoot [['adjRatioSugar']] [con] - summaryDataRoot [['seSugar']] [con1], 
                    rev (summaryDataRoot [['meanSugar']] [con1] * summaryDataRoot [['adjRatioSugar']] [con] + summaryDataRoot [['seSugar']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataRoot [['date']] [con1], 
           y = summaryDataRoot [['meanSugar']] [con1] * summaryDataRoot [['adjRatioSugar']] [con],
           col = '#999999', lwd = 2)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  
  # Add actual treatment
  #--------------------------------------------------------------------------------------
  polygon (x = c (summaryDataRoot [['date']] [con], 
                  rev (summaryDataRoot [['date']] [con])),
           y = c (summaryDataRoot [['meanSugar']] [con] - summaryDataRoot [['seSugar']] [con], 
                  rev (summaryDataRoot [['meanSugar']] [con] + summaryDataRoot [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
  lines (x = summaryDataRoot [['date']] [con], 
         y = summaryDataRoot [['meanSugar']] [con],
         col = tColours [['colour']] [i], lwd = 3, lty = ifelse (i == 1, 1, 3))
  
  # Add axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'Sugar concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'Roots')
  } else {
    axis (side = 2, at = seq (0, 2.5, by = 0.5), labels = rep ('', 6))
  }

}
dev.off  ()

# Plot root starch concentrations over time
#----------------------------------------------------------------------------------------
tiff ('../fig/Exp2017RootStarchConcentrationOverDateAdjusted.tiff', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in c (1, 3, 4, 2)) {
  if (i == 1) {
    par (mar = c (5, 12, 1, 0))
    descriptor <- 'control'
  } else if (i %in% 2:3) {
    par (mar = c (5, 0, 1, 0))
    if (i == 2) descriptor <- 'girdled'
    if (i == 3) descriptor <- 'compressed'
  } else {
    par (mar = c (5, 0, 1, 1))
    descriptor <- 'double compressed'
  }
  con  <- summaryDataRoot [['treatment']] == i
  con1 <- summaryDataRoot [['treatment']] == 1
  plot (x = summaryDataRoot [['date']] [con],
        y = summaryDataRoot [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataRoot [['date']] [con1], 
                    rev (summaryDataRoot [['date']] [con1])),
             y = c (summaryDataRoot [['meanStarch']] [con1] * summaryDataRoot [['adjRatioStarch']] [con] - summaryDataRoot [['seStarch']] [con1], 
                    rev (summaryDataRoot [['meanStarch']] [con1] * summaryDataRoot [['adjRatioStarch']] [con] + summaryDataRoot [['seStarch']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataRoot [['date']] [con1], 
           y = summaryDataRoot [['meanStarch']] [con1] * summaryDataRoot [['adjRatioStarch']] [con],
           col = '#999999', lwd = 2)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  
  # Add actual treatment
  #--------------------------------------------------------------------------------------
  polygon (x = c (summaryDataRoot [['date']] [con], 
                  rev (summaryDataRoot [['date']] [con])),
           y = c (summaryDataRoot [['meanStarch']] [con] - summaryDataRoot [['seStarch']] [con], 
                  rev (summaryDataRoot [['meanStarch']] [con] + summaryDataRoot [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
  lines (x = summaryDataRoot [['date']] [con], 
         y = summaryDataRoot [['meanStarch']] [con],
         col = tColours [['colour']] [i], lwd = 3, lty = ifelse (i == 1, 1, 3))
  
  # Add axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'Starch concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'Roots')
  } else {
    axis (side = 2, at = seq (0, 1.2, by = 0.2), labels = rep ('', 7))
  }
  
}
dev.off  ()

# Plot wood soluble sugar concentrations for 1-2 cm over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  tiff ('../fig/Exp2017DeepStemSugarConcentrationOverDate.tiff', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 12, 1, 0))
  con1 <- summaryDataStem1 [['treatment']] == 1
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanSugar']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('Control') 
  
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanSugar']] [con1] - summaryDataStem1 [['seSugar']] [con1], 
                  rev (summaryDataStem1 [['meanSugar']] [con1] + summaryDataStem1 [['seSugar']] [con1])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con1], 
         y = summaryDataStem1 [['meanSugar']] [con1],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 5, cex = 1.5, 'Sugar concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'Wood (1-2 cm)')
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'control', cex = 3.2, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 1.15, box.lty = 0, lwd = c (3, 2, 3, 3, 3), 
          lty = c (1, 1, 2, 4, 3), 
          legend = c ('control','adjusted control','above','middle','below'), 
          col = c ( '#91b9a4', rep ('#999999', 4)), 
          bg = 'transparent', cex = 2.3)
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem1 [['treatment']] == 3 & summaryDataStem1 [['sampleHeight']] == 2
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanSugar']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanSugar']] [con1] - summaryDataStem1 [['seSugar']] [con1], 
                  rev (summaryDataStem1 [['meanSugar']] [con1] + summaryDataStem1 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 3 & summaryDataStem1 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'compressed', cex = 3.2, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 2.5
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanSugar']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanSugar']] [con1] - summaryDataStem1 [['seSugar']] [con1], 
                  rev (summaryDataStem1 [['meanSugar']] [con1] + summaryDataStem1 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add double compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'double compressed', cex = 3.2, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem1 [['treatment']] == 2 & summaryDataStem1 [['sampleHeight']] == 2
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanSugar']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanSugar']] [con1] - summaryDataStem1 [['seSugar']] [con1], 
                  rev (summaryDataStem1 [['meanSugar']] [con1] + summaryDataStem1 [['seSugar']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem1 [['treatment']] == 2 & summaryDataStem1 [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 2 & summaryDataStem1 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanSugar']] [con] - summaryDataStem1 [['seSugar']] [con], 
                  rev (summaryDataStem1 [['meanSugar']] [con] + summaryDataStem1 [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'girdled', cex = 3.2, 
        col = '#333333')
  
  dev.off  ()
}

# Plot wood starch concentrations for 0-1 cm over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  tiff ('../fig/Exp2017DeepStemStarchConcentrationOverDate.tiff', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 12, 1, 0))
  con1 <- summaryDataStem1 [['treatment']] == 1
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanStarch']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #----------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add actual group
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanStarch']] [con1] - summaryDataStem1 [['seStarch']] [con1], 
                  rev (summaryDataStem1 [['meanStarch']] [con1] + summaryDataStem1 [['seStarch']] [con1])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con1], 
         y = summaryDataStem1 [['meanStarch']] [con1],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 5, cex = 1.5, 'Starch concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'Wood (1-2 cm)')
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'control', cex = 3.2, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 1.1, box.lty = 0, lwd = c (3, 2, 3, 3, 3), 
          lty = c (1, 1, 2, 4, 3), 
          legend = c ('control','adjusted control','above','middle','below'), 
          col = c ('#91b9a4', rep ('#999999', 4)), 
          bg = 'transparent', cex = 2.3)
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem1 [['treatment']] == 3 & summaryDataStem1 [['sampleHeight']] == 2
  par (mar = c (5, 0, 1, 0))
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanStarch']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanStarch']] [con1] - summaryDataStem1 [['seStarch']] [con1], 
                  rev (summaryDataStem1 [['meanStarch']] [con1] + summaryDataStem1 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 3 & summaryDataStem1 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'compressed', cex = 3.2, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 2.5
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanStarch']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanStarch']] [con1] - summaryDataStem1 [['seStarch']] [con1], 
                  rev (summaryDataStem1 [['meanStarch']] [con1] + summaryDataStem1 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 2.5
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem1 [['treatment']] == 4 & summaryDataStem1 [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'double compressed', cex = 3.2, 
        col = '#333333')
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  con <- summaryDataStem1 [['treatment']] == 2 & summaryDataStem1 [['sampleHeight']] == 2
  plot (x = summaryDataStem1 [['date']] [con1],
        y = summaryDataStem1 [['meanStarch']] [con1], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.2), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem1 [['date']] [con1], 
                  rev (summaryDataStem1 [['date']] [con1])),
           y = c (summaryDataStem1 [['meanStarch']] [con1] - summaryDataStem1 [['seStarch']] [con1], 
                  rev (summaryDataStem1 [['meanStarch']] [con1] + summaryDataStem1 [['seStarch']] [con1])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem1 [['treatment']] == 2 & summaryDataStem1 [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem1 [['date']] [con], 
                  rev (summaryDataStem1 [['date']] [con])),
           y = c (summaryDataStem1 [['meanStarch']] [con] - summaryDataStem1 [['seStarch']] [con], 
                  rev (summaryDataStem1 [['meanStarch']] [con] + summaryDataStem1 [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem1 [['date']] [con], 
         y = summaryDataStem1 [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('Jul','Aug','Sep','Oct','Nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-08-30'), y = 1.2, pos = 1, labels = 'girdled', cex = 3.2, 
        col = '#333333')
  
  dev.off  ()
}
