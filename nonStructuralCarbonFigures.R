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
summaryDataStem <- stemData2017 %>% group_by (date, treatment, sampleHeight) %>% 
  summarise (meanSugar  = mean (sugar,  na.rm = T), seSugar  = se (sugar),  nSugar  = sum (!is.na (sugar)),
             meanStarch = mean (starch, na.rm = T), seStarch = se (starch), nStarch = sum (!is.na (starch)))
summaryDataRoot <- rootData2017 %>% group_by (date, treatment) %>% 
  summarise (meanSugar  = mean (sugar,  na.rm = T), seSugar  = se (sugar),  nSugar  = sum (!is.na (sugar)),
             meanStarch = mean (starch, na.rm = T), seStarch = se (starch), nStarch = sum (!is.na (starch)))

# Plot leaf soluble sugar concentrations over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017LeafSugarConcentrationOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in 1:4) {
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
             y = c (summaryDataLeaf [['meanSugar']] [con1] - summaryDataLeaf [['seSugar']] [con1], 
                    rev (summaryDataLeaf [['meanSugar']] [con1] + summaryDataLeaf [['seSugar']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataLeaf [['date']] [con1], 
           y = summaryDataLeaf [['meanSugar']] [con1],
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
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'sugar concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'leaves')
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 12, pos = 4, labels = descriptor, cex = 2.7, 
      col = '#333333')
  
  # Add panel separator
  #--------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 5, box.lty = 0, lwd = 3, lty = c (1, 2), 
                      legend = c ('control','above'), col = '#999999', 
                      bg = 'transparent', cex = 2)
}
dev.off  ()

# Plot leaf starch concentrations over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017LeafStarchConcentrationOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in 1:4) {
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
  con  <- summaryDataLeaf [['treatment']] == i
  con1 <- summaryDataLeaf [['treatment']] == 1
  plot (x = summaryDataLeaf [['date']] [con],
        y = summaryDataLeaf [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 6), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataLeaf [['date']] [con1], 
                    rev (summaryDataLeaf [['date']] [con1])),
             y = c (summaryDataLeaf [['meanStarch']] [con1] - summaryDataLeaf [['seStarch']] [con1], 
                    rev (summaryDataLeaf [['meanStarch']] [con1] + summaryDataLeaf [['seStarch']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataLeaf [['date']] [con1], 
           y = summaryDataLeaf [['meanStarch']] [con1],
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
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'starch concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'leaves')
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 6, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
  # Add panel separator
  #--------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 5.3, box.lty = 0, lwd = 3, lty = c (1, 2), 
                      legend = c ('control','above'), col = '#999999', 
                      bg = 'transparent', cex = 2)
}
dev.off  ()


# Plot wood soluble sugar concentrations over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  png ('../fig/Exp2017StemSugarConcentrationOverDate.png', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 12, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 2.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = tColours [['colour']] [1], cex.lab = 1.8)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('control') 
  
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 5, cex = 1.5, 'sugar concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'wood')
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 2.5, pos = 4, labels = 'control', cex = 2.7, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 2.0, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
          legend = c ('control','above','middle','below'), col = '#999999', 
          bg = 'transparent', cex = 2)
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 2.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 2 & summaryDataStem [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 2 & summaryDataStem [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 2.5, pos = 4, labels = 'girdled', cex = 2.8, 
        col = '#333333')
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 2.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 3 & summaryDataStem [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 3 & summaryDataStem [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 2.5, pos = 4, labels = 'compressed', cex = 2.8, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 2.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add double compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 2.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                  rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanSugar']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 2.5, pos = 4, labels = 'double compressed', cex = 2.8, 
        col = '#333333')
  dev.off  ()
}

# Plot wood starch concentrations over time
#----------------------------------------------------------------------------------------
PLOT <- TRUE; if (PLOT) {
  png ('../fig/Exp2017StemStarchConcentrationOverDate.png', width = 1200, height = 380)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
  par (mgp = c (3, 1, 0), mar = c (5, 12, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
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
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [1], lwd = 3)
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  axis (side = 2, cex.axis = 2.2, las = 1)
  mtext (side = 2, line = 5, cex = 1.5, 'starch concentration (% dry weight)')
  mtext (side = 2, line = 9, cex = 3, 'wood')
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 1.5, pos = 4, labels = 'control', cex = 2.7, 
        col = '#333333')
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #----------------------------------------------------------------------------------------
  legend (x = as_date ('2017-07-20'), y = 1.3, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
          legend = c ('control','above','middle','below'), col = '#999999', 
          bg = 'transparent', cex = 2)
  
  # Add panel of the girdled trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('girdled') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 2 & summaryDataStem [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 2 & summaryDataStem [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [2], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 1.5, pos = 4, labels = 'girdled', cex = 2.8, 
        col = '#333333')
  
  # Add panel of the compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('compressed') 
  
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add compressed trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 3 & summaryDataStem [['sampleHeight']] == 2
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 3 & summaryDataStem [['sampleHeight']] == 1
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [3], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 1.5, pos = 4, labels = 'compressed', cex = 2.8, 
        col = '#333333')
  
  # Add panel for double compressed trees
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  con <- summaryDataStem [['treatment']] == 1
  plot (x = summaryDataStem [['date']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 2, col = '#999999')
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity ('#999999', 0.2), lty = 0)
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates ('double compressed') 
  
  # Add girdled trees
  #----------------------------------------------------------------------------------------
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 2.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 2)
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 1.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 4)
  con <- summaryDataStem [['treatment']] == 4 & summaryDataStem [['sampleHeight']] == 0.5
  polygon (x = c (summaryDataStem [['date']] [con], 
                  rev (summaryDataStem [['date']] [con])),
           y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                  rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.3), lty = 0)
  lines (x = summaryDataStem [['date']] [con], 
         y = summaryDataStem [['meanStarch']] [con],
         col = tColours [['colour']] [4], lwd = 3, lty = 3)
  
  # Add axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 1.5, pos = 4, labels = 'double compressed', cex = 2.8, 
        col = '#333333')
  
  dev.off  ()
}

# Plot root soluble sugar concentrations over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017RootSugarConcentrationOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in 1:4) {
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
             y = c (summaryDataRoot [['meanSugar']] [con1] - summaryDataRoot [['seSugar']] [con1], 
                    rev (summaryDataRoot [['meanSugar']] [con1] + summaryDataRoot [['seSugar']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataRoot [['date']] [con1], 
           y = summaryDataRoot [['meanSugar']] [con1],
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
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'sugar concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'roots')
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 2.5, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
  # Add panel separator
  #--------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 0.8, box.lty = 0, lwd = 3, lty = c (1, 3), 
                      legend = c ('control','below'), col = '#999999', 
                      bg = 'transparent', cex = 2)
}
dev.off  ()

# Plot root starch concentrations over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017RootStarchConcentrationOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.5, 1, 1, 1.05))
par (mgp = c (3, 1, 0))
for (i in 1:4) {
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
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 1.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = 3, 
        col = 'white', cex.lab = 1.8)
  
  # Add control in the background
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    polygon (x = c (summaryDataRoot [['date']] [con1], 
                    rev (summaryDataRoot [['date']] [con1])),
             y = c (summaryDataRoot [['meanStarch']] [con1] - summaryDataRoot [['seStarch']] [con1], 
                    rev (summaryDataRoot [['meanStarch']] [con1] + summaryDataRoot [['seStarch']] [con1])),
             col = addOpacity ('#999999', 0.2), lty = 0)
    lines (x = summaryDataRoot [['date']] [con1], 
           y = summaryDataRoot [['meanStarch']] [con1],
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
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  
  # Add y-axis and label 
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 5, cex = 1.5, 'starch concentration (% dry weight)')
    mtext (side = 2, line = 9, cex = 3, 'roots')
  }
  
  # Add panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 1.5, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
  # Add panel separator
  #--------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add legend 
  #--------------------------------------------------------------------------------------
  if (i == 1) legend (x = as_date ('2017-07-20'), y = 1.2, box.lty = 0, lwd = 3, lty = c (1, 3), 
                      legend = c ('control','below'), col = '#999999', 
                      bg = 'transparent', cex = 2)
}
dev.off  ()

# create yPosition and plotting symbols for the stemData
stemData2017 [['y']] <- NA ; stemData2017 [['pch']] <- NA
con <- which (stemData2017 [['treatment']] == 1)
stemData2017 [['y']] [con] <- yPositions [1]
stemData2017 [['p']] <- 21

con <- which (stemData2017 [['treatment']] == 2 & stemData2017 [['sampleHeight']] == 1)
stemData2017 [['y']] [con] <- yPositions [2]
stemData2017 [['p']] [con] <- 25

con <- which (stemData2017 [['treatment']] == 2 & stemData2017 [['sampleHeight']] == 2)
stemData2017 [['y']] [con] <- yPositions [3]
stemData2017 [['p']] [con] <- 24

con <- which (stemData2017 [['treatment']] == 3 & stemData2017 [['sampleHeight']] == 1)
stemData2017 [['y']] [con] <- yPositions [4]
stemData2017 [['p']] [con] <- 25

con <- which (stemData2017 [['treatment']] == 3 & stemData2017 [['sampleHeight']] == 2)
stemData2017 [['y']] [con] <- yPositions [5]
stemData2017 [['p']] [con] <- 24

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 0.5)
stemData2017 [['y']] [con] <- yPositions [6]
stemData2017 [['p']] [con] <- 25

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 1.5)
stemData2017 [['y']] [con] <- yPositions [7]
stemData2017 [['p']] [con] <- 22

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 2.5)
stemData2017 [['y']] [con] <- yPositions [8]
stemData2017 [['p']] [con] <- 24
  
# calculate mean and standard deviations of NSC concentrations by treatment and sample 
# height for each tissue
#----------------------------------------------------------------------------------------
leafMeans <- leafData2017 %>% select (treatment, date, sugar, starch) %>% 
                              group_by (treatment, date) %>% 
                              summarise_each (funs (mean (., na.rm = TRUE))) 
leafSDs <- leafData2017 %>% select (treatment, date, sugar, starch) %>% 
                            group_by (treatment, date) %>% 
                            summarise_each (funs (sd (., na.rm = TRUE)))
stemMeans <- stemData2017 %>% select (treatment, sampleHeight, date, y, sugar, starch) %>% 
                              group_by (treatment, sampleHeight, date, y) %>% 
                              summarise_each (funs (mean (., na.rm = TRUE))) 
stemSDs <- stemData2017 %>% select (treatment, sampleHeight, date, sugar, starch) %>% 
                            group_by (treatment, sampleHeight, date) %>% 
                            summarise_each (funs (sd (., na.rm = TRUE)))
rootMeans <- rootData2017 %>% select (treatment, date, sugar, starch) %>% 
                              group_by (treatment, date) %>% 
                              summarise_each (funs (mean (., na.rm = TRUE))) 
rootSDs <- rootData2017 %>% select (treatment, date, sugar, starch) %>% 
                            group_by (treatment, date) %>% 
                            summarise_each (funs (sd (., na.rm = TRUE)))

# plot leaf sugar concentrations for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafSugarConcentrations.png', width = 600, height = 375)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (6, 10, 6, 0))
  plot (y = 1:4,
        x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 24, cex = 2, lwd = 3,
        xlab = '', ylab = '', 
#        xaxt = 'n', yaxt = 'n',
        axes = FALSE,
        xlim = c (-1, 13), ylim = c (0, 5))

  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3), cex.lab = 1.5)

  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'girdled', 'compressed', expression ('    double \ncompressed')),
        tick = 1, cex.lab = 1.1)
  
  # add month label
  #--------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 2, pos = 4)
  
  # add tissues label
  #--------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'leaves', cex = 3)
  
  # individual leaf sugar measurements as points
  #--------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']] [leafData2017 [['date']] == '2017-07-05'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2, bg = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA))
  
  # add mean and standard deviation of leaf sugar by treatment
  #--------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-07-05'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'] + 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-07-05'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = '#333333', code = 3, length = 0.05, angle = 90,lwd = 3)
  points (x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-07-05'],
          y = leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05'],
          col = colours [leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05']], 
          pch =  c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # plot august leaf sugar concentrations
  #--------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 24, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual leaf sugar measurements form august  as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']]     [leafData2017 [['date']] == '2017-08-10'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation for august leaf sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'] + 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = '#333333', code = 3, length = 0.05, angle = 90,lwd = 3)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # add title
  #----------------------------------------------------------------------------------------
  mtext (side = 3, line = 3.5, text = 'sugar', at = 12.5, cex = 3)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 12.5, cex = 1.5)
  
  # plot october leaf sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 24, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']]     [leafData2017 [['date']] == '2017-10-09'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'] +  
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09']], 
          pch = c(21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # plot november leaf sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 1))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 24, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of leaf sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']] [leafData2017 [['date']] == '2017-11-03'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation for leaf sugar in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'] +  
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# plot leaf starch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafStarchConcentrations.png', width = 600, height = 375)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (6, 5, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
        x = leafMeans [['starch']]    [leafMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'gridled', 'compressed', expression ('    double \ncompressed')), 
        tick = 1, cex.lab = 1.1)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 2, pos = 4)

  # individual measurements of leaf starch in July as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-07-05'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation for leaf starch in July
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-07-05'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-07-05'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-07-05'] + 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-07-05'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-07-05'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = colours [leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # plot august concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of leaf starch in august as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-08-10'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation for leaf starch in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-08-10'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'] + 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-08-10'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
  
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # add title
  #----------------------------------------------------------------------------------------
  mtext (side = 3, line = 3.5, text = 'starch', at = 7.5, cex = 3)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 7.5, cex = 1.5)
  
  # plot october concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 0))
  plot (y =  leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of leaf starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-10-09'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation for laaf starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-10-09'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'] +  
               leafSDs [['starch']] [leafMeans [['date']] == '2017-10-09'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
 
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # plot november leaf starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 6, 1))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-11-03'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03']], ALPHA),
          bg  = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'] == 1, 21, 24), 
          cex = 2)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-11-03'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'] +  
               leafSDs [['starch']] [leafMeans [['date']] == '2017-11-03'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03']], 
          pch = c (21, 24, 24, 24), bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# plot wood sugar concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemSugarConcentrations.png', width = 600, height = 425)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (6, 10, 3, 0))
  plot (y = stemMeans [['y']]     [stemMeans [['date']] == '2017-07-05'],
        x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', 
        axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, 
        labels = c ('C','B','A','B','A','B','M','A'), 
        tick = 1, las = 1, cex.lab = 1.2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # add treatments
  #----------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control', at = yPositions [1], cex = 1.1)
  mtext (side = 2, line = 2, text = 'girdled', at = mean (yPositions [c(2,3)]), cex = 1.1)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]), cex = 1.1)
  mtext (side = 2, line = 3, text = 'double', at = mean (yPositions [c(6,7,8)]), cex = 1.1)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]), cex = 1.1)
  mtext (side = 2, line = 6, text = 'wood', cex = 3)
  
  # individual measurements of wood sugar in july as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-07-05'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-07-05'], cex = 2)
  
  # add mean and standard deviation for july wood sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-07-05'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'] + 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-07-05'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-07-05'],
          y = yPositions,
          col = colours [stemMeans [['treatment']]  [stemMeans [['date']] == '2017-07-05']], 
          pch =c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot august wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements of wood sugar in august as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-08-10'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-08-10'],
          cex = 2)
  
  # add mean and standard deviation for wood sugar in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'] + 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-08-10']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 2.5, cex = 1.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot october wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements of wood sugar in october as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-10-09'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-10-09'], cex = 2)
  
  # add mean and standard deviation for wood sugar in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'] +  
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-10-09']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot november wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 1))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements of wood sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-11-03'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-11-03']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-11-03'], cex = 2)
  
  # add mean and standard deviation of november wood sugar concentrations
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'] +  
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-11-03']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# plot wood starch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemStarchConcentrations.png', width = 600, height = 425)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (6, 5, 3, 0))
  plot (y = stemMeans [['y']]     [stemMeans [['date']] == '2017-07-05'],
        x = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
        
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, 
        labels = c ('C','B','A','B','A','B','M','A'), 
        tick = 1, las = 1, cex.lab = 1.2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # add treatments
  #----------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control', at = yPositions [1], cex = 1.1)
  mtext (side = 2, line = 2, text = 'girdled', at = mean (yPositions [c(2,3)]), cex = 1.1)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]), cex = 1.1)
  mtext (side = 2, line = 3, text = 'double', at = mean (yPositions [c(6,7,8)]), cex = 1.1)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]), cex = 1.1)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-07-05'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-07-05'], cex = 2)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-07-05'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'] + 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-07-05'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-07-05'],
          y = yPositions,
          col = colours [stemMeans [['treatment']]  [stemMeans [['date']] == '2017-07-05']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)

  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot august wood starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = yPositions,
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements for august wood starch as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-08-10'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-08-10'], cex = 2)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-08-10'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'] + 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-08-10'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-08-10']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 1, cex = 1.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot october starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = yPositions,
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes =FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements of wood starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-10-09'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-10-09'], cex = 2)
  
  # add mean and standard deviation for wood starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-10-09'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'] +  
               stemSDs [['starch']] [stemMeans [['date']] == '2017-10-09'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-10-09']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot november wood starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 1))
  plot (y = yPositions,
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 6.8, cex = 2, pos = 4)
  
  # individual measurements of wood starch in november as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-11-03'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-11-03']], ALPHA),
          bg = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-11-03'], cex = 2)
  
  # add mean and standard deviation for wood starch in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-11-03'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'] +  
               stemSDs [['starch']] [stemMeans [['date']] == '2017-11-03'],
          y0 = yPositions,
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-11-03']], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24), 
          bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# plot root sugar concentrations for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootSugarConcentrations.png', width = 600, height = 375)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (6, 10, 3, 0))
  plot (y = 1:4,
        x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', 
        #        xaxt = 'n', yaxt = 'n',
        axes = FALSE,
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'girdled', 'compressed', expression ('    double \ncompressed')),
        tick = 1, cex.lab = 1.2, cex.axis = 1.2)
  
  # add month label
  #--------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 2, pos = 4)
  
  # add tissues label
  #--------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'roots', cex = 3)
  
  # individual july root sugar measurements as points
  #--------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']] [rootData2017 [['date']] == '2017-07-05'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation of july root sugar by treatment
  #--------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'] - 
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-07-05'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'] + 
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-07-05'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = '#333333', code = 3, length = 0.05, angle = 90,lwd = 3)
  points (x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-07-05'],
          y = rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05'],
          col = colours [rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # plot august root sugar concentrations
  #--------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual root sugar measurements form august  as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']]     [rootData2017 [['date']] == '2017-08-10'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for august root sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'] - 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'] + 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = '#333333', code = 3, length = 0.05, angle = 90,lwd = 3)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 5.5, cex = 1.5)
  
  # plot october root sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of october root sugar as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']]     [rootData2017 [['date']] == '2017-10-09'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for october root sugar
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'] - 
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'] +  
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # plot november root sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 1))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of root sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']] [rootData2017 [['date']] == '2017-11-03'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for root sugar in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'] - 
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'] +  
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# plot root starch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootStarchConcentrations.png', width = 600, height = 375)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (6, 5, 3, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
        x = rootMeans [['starch']]    [rootMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'gridled', 'compressed', expression ('    double \ncompressed')), 
        tick = 1, cex.lab = 1.2, cex.axis = 1.2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of root starch in July as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-07-05'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for root starch in July
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-07-05'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-07-05'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-07-05'] + 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-07-05'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-07-05'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = colours [rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')

  # plot august concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of root starch in august as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-08-10'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for root starch in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-08-10'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'] + 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-08-10'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 4, text = 'concentration (% dry weight)', at = 1.7, cex = 1.5)
  
  # plot october concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 0))
  plot (y =  rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements of root starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-10-09'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for elaf starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-10-09'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'] +  
               rootSDs [['starch']] [rootMeans [['date']] == '2017-10-09'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')
  
  # plot november root starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (6, 0, 3, 1))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 2, lwd = 3,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 2, pos = 4)
  
  # individual measurements november root starch as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-11-03'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03']], ALPHA),
          bg  = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = ifelse (rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'] == 1, 21, 25), 
          cex = 2)
  
  # add mean and standard deviation for november root starch
  #--------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-11-03'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'] +  
               rootSDs [['starch']] [rootMeans [['date']] == '2017-11-03'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = '#333333', code = 3, length = 0.05, angle = 90, lwd = 3)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03']], 
          pch = c (21, 25, 25, 25), bg = 'white', lwd = 3, cex = 2.5)
dev.off ()

# create legend to go underneath all NSC graphs
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017NSCLegend.png', height = 90, width = 900)
  par (mar = c (1, 0, 1, 0))
  plot (x = rep (2:5, 2),
        y = c (rep (2, 4), rep (0, 4)),
        col = c (colours, rep ('#333333', 4)),
        pch = c (rep (26, 4), 21, 25, 23, 24), 
        lty = c (rep (1, 4), rep (0, 4)),
        lwd = 3, typ = 'p',
        xlab = '', ylab = '',
        xlim = c (0, 6), ylim = c (-0.2, 2.7),
        axes = FALSE)
  segments (x0 = seq (1.8, 4.8), x1 = seq (2.2, 5.2), y0 = 2, col = colours, lwd = 3)
  text (x = rep (2:5, 2),
        y = c (rep (2, 4), rep (0, 4)),
        labels = c ('control','girdled','compressed','double compressed',
                    'no treatment','below','inbetween','above'),
        pos = 3)
  text (x = 1, y = c (0.5, 2.5), c ('position:','treatment:'), pos = 4)
dev.off ()
