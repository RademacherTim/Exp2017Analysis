#========================================================================================
# Script to make figures with regard to structural carbon in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('zoo')

# Load colour scheme and anatomical data 
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')
source ('processAnatomicalData.R')

# Get treatment and sampling height mean and standard error for cell wall thickness along
# the radial file and by date of formation 
#----------------------------------------------------------------------------------------
radMeans <- data %>% filter (year == 2017) %>% group_by (treatment, height, RADDISTR.BAND) %>% 
  summarise (meanCWT = mean (CWTALL, na.rm = TRUE), seCWT = se (CWTALL), 
             nCWT = sum (!is.na (CWTALL)), meanNCells = mean (nCells, na.rm = TRUE), 
             seNCells = se (nCells), nNCells = sum (!is.na (nCells)), 
             meanCellSize = mean (cellRadWidth, na.rm = TRUE), seCellSize = se (cellRadWidth), 
             nCellSize = sum (!is.na (cellRadWidth))) 
temMeans <- data %>% filter (year == 2017) %>% group_by (treatment, height, formationDate) %>% 
  summarise (meanCWT = mean (CWTALL, na.rm = TRUE), seCWT = se (CWTALL), 
             nCWT = sum (!is.na (CWTALL)), meanNCells = mean (nCells, na.em = TRUE), 
             seNCells = se (nCells), nNCells = sum (!is.na (nCells)), 
             meanCellSize = mean (cellRadWidth, na.em = TRUE), seCellSize = se (cellRadWidth), 
             nCellSize = sum (!is.na (cellRadWidth))) 

# Calculate treatment and sampling height mean and standard error by period
#----------------------------------------------------------------------------------------
summaryData <- data %>% filter (year == 2017) %>% group_by (treatment, height, period) %>%
  summarise (meanCWT = mean (CWTALL, na.rm = TRUE), seCWT = se (CWTALL), 
             nCWT = sum (!is.na (CWTALL)), meanNCells = mean (nCells, na.rm = TRUE), 
             seNCells = se (nCells), nNCells = sum (!is.na (nCells)), 
             meanCellSize = mean (cellRadWidth, na.rm = TRUE), seCellSize = se (cellRadWidth), 
             nCellSize = sum (!is.na (cellRadWidth)))

# Add the mean ring width at each point in time
#-----------------------------------------------------------------------------------------
temp1 <- data %>% filter (year == 2017) %>% group_by (treatment, height, tree, period) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% mutate (period = as_date (period))
temp2 <- data %>% filter (year == 2017) %>% group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% add_column (period = as_date ('2017-08-09'), .before = 4)
temp3 <- data %>% filter (year == 2017) %>% group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% add_column (period = as_date ('2017-10-09'), .before = 4)
temp4 <- data %>% filter (year == 2017) %>% group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% add_column (period = as_date ('2017-11-03'), .before = 4)
temp <- dplyr::union_all (temp1, temp2) %>% dplyr::union_all (temp3) %>% dplyr::union_all (temp4) %>% 
  arrange (treatment, height, tree, period) %>% group_by (treatment, height, tree, period) %>% 
  summarise (maxRW = min (maxRW, na.rm = TRUE)) %>% 
  group_by (treatment, height, period) %>% summarise (meanRW = mean (maxRW, na.rm = TRUE), seRW = se (maxRW), nRW = sum (!is.na (maxRW))) 

summaryData <- cbind (summaryData, meanRW = temp [['meanRW']], seRW = temp [['seRW']])


# Calculate the mean period increment in ring width
#----------------------------------------------------------------------------------------
summaryData <- add_column (summaryData, incRW = NA)
for (r in 1:32) {
  if (summaryData [['period']] [r] == as_date ('2017-07-03')) {
    summaryData [['incRW']] [r] <- summaryData [['meanRW']] [r]
  } else {
    summaryData [['incRW']] [r] <- summaryData [['meanRW']] [r] - summaryData [['meanRW']] [r-1]
  }
}

# Calculate the mean number of cells formed per period and its standard error
#----------------------------------------------------------------------------------------
summaryData <- mutate (summaryData, nCells = meanNCells * incRW / 20, 
                       nCells2 = incRW / meanCellSize, seNCells = se (meanNCells * incRW / 20)) 

# Add mean cumulative number of cells per treatment and sampling height to summaryData
#----------------------------------------------------------------------------------------
summaryData <- summaryData %>% group_by (treatment, height) %>%  mutate (cumNCells = cumsum (nCells))

# Convert microns to mm for ring width
#----------------------------------------------------------------------------------------


# Plot estimated ring width for each group over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017RingWidthOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
for (i in 1:4) {
  par (mgp = c (3, 1, 0))
  
  # Determine the panel name
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    descriptor <- 'control'
    par (mar = c (5, 8, 1, 0))
  } else if (i == 2) {
    descriptor <- 'girdled'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 3) {
    descriptor <- 'compressed'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 4) {
    descriptor <- 'double compressed'
    par (mar = c (5, 0, 1, 1))
  }
  
  # Plot new panel
  #--------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['period']] [con],
        y = summaryData [['meanRW']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 3000), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = ifelse (i == 1, 3, 2), 
        col = ifelse (i == 1, tColours [['colour']] [1], '#999999'), cex.lab = 1.8)
  polygon (x = c (summaryData [['period']] [con], 
                  rev (summaryData [['period']] [con])),
           y = c (summaryData [['meanRW']] [con] - summaryData [['seRW']] [con], 
                  rev (summaryData [['meanRW']] [con] + summaryData [['seRW']] [con])),
           col = addOpacity (ifelse (i == 1, tColours [['colour']] [1], '#999999'), ifelse (i == 1, 0.3, 0.2)), 
                             lty = 0)
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 6, cex = 1.5, 'ring width (mm)')
    
    # Add legend 
    #----------------------------------------------------------------------------------------
    legend (x = as_date ('2017-07-20'), y = 2900, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
            legend = c ('control','above','middle','below'), col = '#999999', 
            bg = 'transparent', cex = 2)
  }
  
  # Add treatment group mean and standard error
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    # Figure out unqieu heights
    #------------------------------------------------------------------------------------
    con <- summaryData [['treatment']] == i
    heights <- unique (summaryData [['height']] [con])
    
    # Loop over heights
    #------------------------------------------------------------------------------------
    for (h in heights) {
      con <- summaryData [['treatment']] == i & summaryData [['height']] == h
      polygon (x = c (summaryData [['period']] [con],
                      rev (summaryData [['period']] [con])),
               y = c (summaryData [['meanRW']] [con] - summaryData [['seRW']] [con],
                      rev (summaryData [['meanRW']] [con] + summaryData [['seRW']] [con])),
               col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
      lines (x = summaryData [['period']] [con],
             y = summaryData [['meanRW']] [con],
             col = tColours [['colour']] [i], lwd = 3, 
             lty = ifelse (h == 'A', 2, ifelse (h == 'B', 3, 4)))
    }
  }
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 3000, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')

}
dev.off ()

# Plot estimated number of cells over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017CellNumberOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
for (i in 1:4) {
  par (mgp = c (3, 1, 0))
  
  # Determine the panel name
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    descriptor <- 'control'
    par (mar = c (5, 8, 1, 0))
  } else if (i == 2) {
    descriptor <- 'girdled'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 3) {
    descriptor <- 'compressed'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 4) {
    descriptor <- 'double compressed'
    par (mar = c (5, 0, 1, 1))
  }
  
  # Plot new panel
  #--------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['period']] [con],
        y = summaryData [['cumNCells']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 70), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = ifelse (i == 1, 3, 2), 
        col = ifelse (i == 1, tColours [['colour']] [1], '#999999'), cex.lab = 1.8)
  polygon (x = c (summaryData [['period']] [con], 
                  rev (summaryData [['period']] [con])),
           y = c (summaryData [['cumNCells']] [con] - summaryData [['seNCells']] [con], 
                  rev (summaryData [['cumNCells']] [con] + summaryData [['seNCells']] [con])),
           col = addOpacity (ifelse (i == 1, tColours [['colour']] [1], '#999999'), ifelse (i == 1, 0.3, 0.2)), 
           lty = 0)
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 6, cex = 1.5, 'number of cells (n)')
    
    # Add legend 
    #----------------------------------------------------------------------------------------
    legend (x = as_date ('2017-07-20'), y = 25, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
            legend = c ('control','above','middle','below'), col = '#999999', 
            bg = 'transparent', cex = 2)
  }
  
  # Add treatment group mean and standard error
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    # Figure out unqieu heights
    #------------------------------------------------------------------------------------
    con <- summaryData [['treatment']] == i
    heights <- unique (summaryData [['height']] [con])
    
    # Loop over heights
    #------------------------------------------------------------------------------------
    for (h in heights) {
      con <- summaryData [['treatment']] == i & summaryData [['height']] == h
      polygon (x = c (summaryData [['period']] [con],
                      rev (summaryData [['period']] [con])),
               y = c (summaryData [['cumNCells']] [con] - summaryData [['seNCells']] [con],
                      rev (summaryData [['cumNCells']] [con] + summaryData [['seNCells']] [con])),
               col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
      lines (x = summaryData [['period']] [con],
             y = summaryData [['cumNCells']] [con],
             col = tColours [['colour']] [i], lwd = 3, 
             lty = ifelse (h == 'A', 2, ifelse (h == 'B', 3, 4)))
    }
  }
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 70, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
}
dev.off ()

# Plot estimated cell size for each group over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017CellSizeOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
for (i in 1:4) {
  
  # Determine the panel name
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    descriptor <- 'control'
    par (mar = c (5, 8, 1, 0))
  } else if (i == 2) {
    descriptor <- 'girdled'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 3) {
    descriptor <- 'compressed'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 4) {
    descriptor <- 'double compressed'
    par (mar = c (5, 0, 1, 1))
  }
  
  # Plot new panel
  #--------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['period']] [con],
        y = summaryData [['meanCellSize']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (0, 45), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = ifelse (i == 1, 3, 2), 
        col = ifelse (i == 1, tColours [['colour']] [1], '#999999'), cex.lab = 1.8)
  polygon (x = c (summaryData [['period']] [con], 
                  rev (summaryData [['period']] [con])),
           y = c (summaryData [['meanCellSize']] [con] - summaryData [['seCellSize']] [con], 
                  rev (summaryData [['meanCellSize']] [con] + summaryData [['seCellSize']] [con])),
           col = addOpacity (ifelse (i == 1, tColours [['colour']] [1], '#999999'), ifelse (i == 1, 0.3, 0.2)), 
           lty = 0)
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 6, cex = 1.5, 'mean radial cell size (microns)')
    
    # Add legend 
    #----------------------------------------------------------------------------------------
    legend (x = as_date ('2017-07-20'), y = 20, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
            legend = c ('control','above','middle','below'), col = '#999999', 
            bg = 'transparent', cex = 2)
  }
  
  # Add treatment group mean and standard error
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    # Figure out unqieu heights
    #------------------------------------------------------------------------------------
    con <- summaryData [['treatment']] == i
    heights <- unique (summaryData [['height']] [con])
    
    # Loop over heights
    #------------------------------------------------------------------------------------
    for (h in heights) {
      con <- summaryData [['treatment']] == i & summaryData [['height']] == h
      polygon (x = c (summaryData [['period']] [con],
                      rev (summaryData [['period']] [con])),
               y = c (summaryData [['meanCellSize']] [con] - summaryData [['seCellSize']] [con],
                      rev (summaryData [['meanCellSize']] [con] + summaryData [['seCellSize']] [con])),
               col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
      lines (x = summaryData [['period']] [con],
             y = summaryData [['meanCellSize']] [con],
             col = tColours [['colour']] [i], lwd = 3, 
             lty = ifelse (h == 'A', 2, ifelse (h == 'B', 3, 4)))
    }
  }
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 45, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
}
dev.off ()


# Plot estimated cell size for each group over time
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017CellWallthicknessOverDate.png', width = 1200, height = 380)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1.05))
for (i in 1:4) {
  
  # Determine the panel name
  #--------------------------------------------------------------------------------------
  if (i == 1) {
    descriptor <- 'control'
    par (mar = c (5, 8, 1, 0))
  } else if (i == 2) {
    descriptor <- 'girdled'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 3) {
    descriptor <- 'compressed'
    par (mar = c (5, 0, 1, 0))
  } else if (i == 4) {
    descriptor <- 'double compressed'
    par (mar = c (5, 0, 1, 1))
  }
  
  # Plot new panel
  #--------------------------------------------------------------------------------------
  con <- summaryData [['treatment']] == 1
  plot (x = summaryData [['period']] [con],
        y = summaryData [['meanCWT']] [con], 
        xlim = as_date (c ('2017-06-20', '2017-11-10')), ylim = c (1.5, 5.5), axes = FALSE, 
        xlab = '', ylab = '', typ = 'l', lwd = ifelse (i == 1, 3, 2), 
        col = ifelse (i == 1, tColours [['colour']] [1], '#999999'), cex.lab = 1.8)
  polygon (x = c (summaryData [['period']] [con], 
                  rev (summaryData [['period']] [con])),
           y = c (summaryData [['meanCWT']] [con] - summaryData [['seCWT']] [con], 
                  rev (summaryData [['meanCWT']] [con] + summaryData [['seCWT']] [con])),
           col = addOpacity (ifelse (i == 1, tColours [['colour']] [1], '#999999'), ifelse (i == 1, 0.3, 0.2)), 
           lty = 0)
  # Add  line to separate panels
  #----------------------------------------------------------------------------------------
  if (i != 4) abline (v = as_date ('2017-11-15'), col = '#999999')
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  return <- criticalDates (descriptor) 
  
  # Add axis and labels
  #----------------------------------------------------------------------------------------
  axis (side = 1, labels = c ('jul','aug','sep','oct','nov'),
        at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')),
        cex.axis = 2.2, mgp = c (3, 2, 0))
  if (i == 1) {
    axis (side = 2, cex.axis = 2.2, las = 1)
    mtext (side = 2, line = 6, cex = 1.5, 'mean cell wall thickness (microns)')
    
    # Add legend 
    #----------------------------------------------------------------------------------------
    legend (x = as_date ('2017-07-20'), y = 3, box.lty = 0, lwd = 3, lty = c (1, 2, 4, 3), 
            legend = c ('control','above','middle','below'), col = '#999999', 
            bg = 'transparent', cex = 2)
  }
  
  # Add treatment group mean and standard error
  #--------------------------------------------------------------------------------------
  if (i != 1) {
    # Figure out unqieu heights
    #------------------------------------------------------------------------------------
    con <- summaryData [['treatment']] == i
    heights <- unique (summaryData [['height']] [con])
    
    # Loop over heights
    #------------------------------------------------------------------------------------
    for (h in heights) {
      con <- summaryData [['treatment']] == i & summaryData [['height']] == h
      polygon (x = c (summaryData [['period']] [con],
                      rev (summaryData [['period']] [con])),
               y = c (summaryData [['meanCWT']] [con] - summaryData [['seCWT']] [con],
                      rev (summaryData [['meanCWT']] [con] + summaryData [['seCWT']] [con])),
               col = addOpacity (tColours [['colour']] [i], 0.3), lty = 0)
      lines (x = summaryData [['period']] [con],
             y = summaryData [['meanCWT']] [con],
             col = tColours [['colour']] [i], lwd = 3, 
             lty = ifelse (h == 'A', 2, ifelse (h == 'B', 3, 4)))
    }
  }
  
  # Add panel descriptor
  #----------------------------------------------------------------------------------------
  text (x = as_date ('2017-06-20'), y = 5.5, pos = 4, labels = descriptor, cex = 2.7, 
        col = '#333333')
  
}
dev.off ()

# Plot cell wall area against ring width for each tree and group
#----------------------------------------------------------------------------------------
png ('../fig/Exp2017radialPositionVsCWT.png', width = 1000, height = 400)
layout (matrix (1:4, nrow = 1, byrow = TRUE), widths  = c (1.2, 1, 1, 1))
par (mar = c (5, 5, 1, 0))
con <- data [['tree']] == 1 & data [['year']] == 2017
plot (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
      y = rollmean (data [['CWTALL']] [con], 5), axes = FALSE, typ = 'l', lwd = 0.5,
      xlab = 'radial distance (microns)', ylab = expression (paste ('cell wall thickness (',microns**2,')')),
      xlim = c (0, 3300), ylim = c (0, 8), col = tColours [['colour']] [1])
axis (side = 1, at = seq (0, 3200, by = 800), cex.lab = 1.5); axis (side = 2, las = 1, cex = 1.5)
for (i in c (3, 4, 6, 7, 9, 18, 30, 31, 36)){
  con <- data [['tree']] == i & data [['year']] == 2017
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), 
         lwd = 0.5, col = tColours [['colour']] [1])
}

# Add treatment mean and standard error
#----------------------------------------------------------------------------------------
con <- radMeans [['treatment']] == 1 & radMeans [['nCWT']] >= 5
polygon (x = c (radMeans [['RADDISTR.BAND']] [con], 
                rev (radMeans [['RADDISTR.BAND']] [con])), 
         y = c (radMeans [['meanCWT']] [con] - radMeans [['seCWT']] [con], 
                rev (radMeans [['meanCWT']] [con] + radMeans [['seCWT']] [con])),
         col = addOpacity (tColours [['colour']] [1], 0.3), lty = 0)
lines (x = radMeans [['RADDISTR.BAND']] [con],
       y = radMeans [['meanCWT']] [con], 
       lwd = 3, col = tColours [['colour']] [1])

# Add descriptor
#----------------------------------------------------------------------------------------
text (x = 0, y = 8, labels = 'control', cex = 2, pos = 4, col = '#333333')

# Plot cell wall thickness for girdled trees
#----------------------------------------------------------------------------------------
par (mar = c (5, 0, 1, 0))
con <- data [['tree']] == 5 & data [['year']] == 2017 & data [['height']] == 'A'
plot (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
      y = rollmean (data [['CWTALL']] [con], 5), axes = FALSE, typ = 'l', lwd = 0.5,
      xlab = 'radial distance (microns)', 
      ylab = expression (paste ('cell wall thickness (',microns**2,')')),
      xlim = c (0, 3300), ylim = c (0, 8), col = tColours [['colour']] [2])
axis (side = 1, at = seq (0, 3200, by = 800), cex.lab = 1.5)
for (i in c (11, 15, 16, 19, 23, 29, 35, 39, 40)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'A'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), 
         col = tColours [['colour']] [2], lwd = 0.5)
}
for (i in c (5, 11, 15, 16, 19, 23, 29, 35, 39, 40)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'B'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), lty = 2,
         col = tColours [['colour']] [2], lwd = 0.5)
}

# Add treatment mean and standard error for girdled trees
#----------------------------------------------------------------------------------------
for (h in c ('A','B')) {
  con <- radMeans [['treatment']] == 2 & radMeans [['height']] == h & radMeans [['nCWT']] >= 5
  polygon (x = c (radMeans [['RADDISTR.BAND']] [con], 
                  rev (radMeans [['RADDISTR.BAND']] [con])), 
           y = c (radMeans [['meanCWT']] [con] - radMeans [['seCWT']] [con], 
                  rev (radMeans [['meanCWT']] [con] + radMeans [['seCWT']] [con])),
           col = addOpacity (tColours [['colour']] [2], 0.4), lty = 0)
  lines (x = radMeans [['RADDISTR.BAND']] [con],
         y = radMeans [['meanCWT']] [con], 
         lwd = 3, col = tColours [['colour']] [2], lty = ifelse (h == 'A', 1, 2))
}

# Add descriptor
#----------------------------------------------------------------------------------------
text (x = 0, y = 8, labels = 'girdled', cex = 2, pos = 4, col = '#333333')

# Add compressed trees panel
#----------------------------------------------------------------------------------------
par (mar = c (5, 0, 1, 0))
con <- data [['tree']] == 10 & data [['year']] == 2017 & data [['height']] == 'A'
plot (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
      y = rollmean (data [['CWTALL']] [con], 5), axes = FALSE, typ = 'l', lwd = 0.5,
      xlab = 'radial distance (microns)', 
      ylab = expression (paste ('cell wall thickness (',microns**2,')')),
      xlim = c (0, 3300), ylim = c (0, 8), col = tColours [['colour']] [3])
axis (side = 1, at = seq (0, 3200, by = 800), cex.lab = 1.5)
for (i in c (10,12,13,17,20,21,28,32,33,38)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'A'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), 
         col = tColours [['colour']] [3], lwd = 0.5)
}
for (i in c (10,12,13,17,20,21,28,32,33,38)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'B'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), lty = 2,
         col = tColours [['colour']] [3], lwd = 0.5)
}

# Add treatment mean and standard error for compresseed trees
#----------------------------------------------------------------------------------------
for (h in c ('A','B')) {
  con <- radMeans [['treatment']] == 3 & radMeans [['height']] == h & radMeans [['nCWT']] >= 5
  polygon (x = c (radMeans [['RADDISTR.BAND']] [con], 
                  rev (radMeans [['RADDISTR.BAND']] [con])), 
           y = c (radMeans [['meanCWT']] [con] - radMeans [['seCWT']] [con], 
                  rev (radMeans [['meanCWT']] [con] + radMeans [['seCWT']] [con])),
           col = addOpacity (tColours [['colour']] [3], 0.4), lty = 0)
  lines (x = radMeans [['RADDISTR.BAND']] [con],
         y = radMeans [['meanCWT']] [con], 
         lwd = 3, col = tColours [['colour']] [3], lty = ifelse (h == 'A', 1, 2))
}

# Add descriptor
#----------------------------------------------------------------------------------------
text (x = 0, y = 8, labels = 'compressed', cex = 2, pos = 4, col = '#333333')

# Add double compressed trees panel
#----------------------------------------------------------------------------------------
par (mar = c (5, 0, 1, 0))
con <- data [['tree']] == 2 & data [['year']] == 2017 & data [['height']] == 'A'
plot (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
      y = rollmean (data [['CWTALL']] [con], 5), axes = FALSE, typ = 'l', lwd = 0.5,
      xlab = 'radial distance (microns)', 
      ylab = expression (paste ('cell wall thickness (',microns**2,')')),
      xlim = c (0, 3300), ylim = c (0, 8), col = tColours [['colour']] [4])
axis (side = 1, at = seq (0, 3200, by = 800), cex.lab = 1.5)
for (i in c (8,14,22,24,25,26,27,34,37)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'A'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), 
         col = tColours [['colour']] [4], lwd = 0.5)
}
for (i in c (2,8,14,22,24,25,26,27,34,37)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'M'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), lty = 3,
         col = tColours [['colour']] [4], lwd = 0.5)
}
for (i in c (2,8,14,22,24,25,26,27,34,37)){
  con <- data [['tree']] == i & data [['year']] == 2017 & data [['height']] == 'B'
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWTALL']] [con], 5), lty = 2,
         col = tColours [['colour']] [4], lwd = 0.5)
}

# Add treatment mean and standard error for compresseed trees
#----------------------------------------------------------------------------------------
for (h in c ('A','M','B')) {
  con <- radMeans [['treatment']] == 4 & radMeans [['height']] == h & radMeans [['nCWT']] >= 5
  polygon (x = c (radMeans [['RADDISTR.BAND']] [con], 
                  rev (radMeans [['RADDISTR.BAND']] [con])), 
           y = c (radMeans [['meanCWT']] [con] - radMeans [['seCWT']] [con], 
                  rev (radMeans [['meanCWT']] [con] + radMeans [['seCWT']] [con])),
           col = addOpacity (tColours [['colour']] [4], 0.4), lty = 0)
  lines (x = radMeans [['RADDISTR.BAND']] [con],
         y = radMeans [['meanCWT']] [con], 
         lwd = 3, col = tColours [['colour']] [4], lty = ifelse (h != 'A', ifelse (h == 'B', 2, 3), 1))
}

# Add descriptor
#----------------------------------------------------------------------------------------
text (x = 0, y = 8, labels = 'double compressed', cex = 2, pos = 4, col = '#333333')

# add legend 
legend (x = 1900, y = 1.2, box.lty = 0, lwd = 2, lty = c (1, 1, 3, 2), 
        legend = c ('control','above','middle','below'), col = '#999999', 
        bg = 'transparent')
dev.off ()

# define tree labels for each group
#----------------------------------------------------------------------------------------
controlTrees      <- c ('01M','03M','04M','06M','07M','09M','18M','30M','31M','36M')
girdledTreesAbove <- c ('05A','11A','15A','16A','19A','23A','29A','35A','39A','40A')
girdledTreesBelow <- c ('05B','11B','15B','16B','19B','23B','29B','35B','39B','40B')
compresTreesAbove <- c ('10A','12A','13A','17A','20A','21A','28A','32A','33A','38A')
compresTreesBelow <- c ('10B','12B','13B','17B','20B','21B','28B','32B','33B','38B')
douCompTreesAbove <- c ('02A','08A','14A','22A','24A','25A','26A','27A','34A','37A')
douCompTreesInBet <- c ('02M','08M','14M','22M','24M','25M','26M','27M','34M','37M')
douCompTreesBelow <- c ('02B','08B','14B','22B','24B','25B','26B','27B','34B','37B')

# set alpha and offset
ALPHA <- 0.3
offset <- 6


# create tibble with cell numbers per ring
cellNumber <- tibble (tree = NA, treatment = NA, height = NA, n = NA)

# determine the approximate number of cells in each ring
for (iTree in 1:40) { # loop over trees
  
  # get treatment and determine heights
  if (unique (data [['PLOT']] [as.numeric (substr (data [['TREE']], 1, 2)) == iTree]) == 'T1') {
    heights <-  c ('M')
    treatment <- 1
  } else if (unique (data [['PLOT']] [as.numeric (substr (data [['TREE']], 1, 2)) == iTree]) == 'T2') {
    heights <-  c ('A', 'B')
    treatment <- 2
  } else if (unique (data [['PLOT']] [as.numeric (substr (data [['TREE']], 1, 2)) == iTree]) == 'T3') {
    heights <-  c ('A', 'B')
    treatment <- 3
  } else if (unique (data [['PLOT']] [as.numeric (substr (data [['TREE']], 1, 2)) == iTree]) == 'T4') {
    heights <-  c ('A', 'M', 'B')
    treatment <- 4
  }
  
  # loop over sampling heights
  for (iHeight in heights) {
    
    if (iTree < 10) {
      treeID <- paste0 ('0',iTree,iHeight)
    } else {
      treeID <- paste0 (iTree, iHeight)
    }
    
    # determine average number of cells in sector and sum them
    condition <- data [['YEAR']] == 2017 & data [['TREE']] == treeID
    nCells <- floor (sum (20 / data [['cellRadWidth']] [condition], na.rm = TRUE))
    iH <- iHeight
    if (treatment == 1) iH <- 'C'
    cellNumber <- add_row (cellNumber, tree = iTree, treatment = treatment, height = iH, n = nCells)
    
  } # end height loop
} # end tree loop

# delete first row, which is empty and 06.1M because data is no good
cellNumber <- cellNumber [-1, ]

# mean and standard deviation of cell number by treatment and height 
cellNMeans <- aggregate (cellNumber [['n']], 
                         by = c (list (cellNumber [['height']]), list (cellNumber [['treatment']])), 
                         FUN = mean)
cellNSD    <- aggregate (cellNumber [['n']], 
                         by = c (list (cellNumber [['height']]), list (cellNumber [['treatment']])), 
                         FUN = function (x) sd (x, na.rm = TRUE))

ALPHA <- 0.4

# plot the estimated mean end of growth for each treatment
#png (filename = '../fig/Exp2017EndOfGrowthAndCellNumbers.png', width = 600)
png (filename = '../fig/Exp2017CellNumbers.png', width = 600)
  # add new plot of cell number
  #--------------------------------------------------------------------------------------
  par (mfrow = c (1, 1))
  par (mar = c (5, 5, 1, 1))
  plot (y = rep (xPositions [1], sum (cellNumber [['treatment']] == 1, na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 1], 
        xlab = '', ylab = '', axes = FALSE,cex = 2,
        pch = 21, col = 'white', bg = 'white', 
        ylim = c (0, 6), xlim = c (0, 120))
  
  # add x axis and margin text
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 120, by  = 20), cex = 2)
  mtext (side = 1, line = 3, at = 60, cex = 1.5, 
         text = 'mean number of cells in final ring (n)')
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = xPositions,
        labels = c ('M','B','A','B','A','B','M','A'),
        tick = 1, las = 1, cex = 2)
  
  # add treatment as y-axis labels 
  #--------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control', at = xPositions [1], cex = 1.2)
  mtext (side = 2, line = 2, text = 'girdled', at = mean (xPositions [c(2,3)]), cex = 1.2)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (xPositions [c(4,5)]), cex = 1.2)
  mtext (side = 2, line = 3, text = 'double', at = mean (xPositions [c(6,7,8)]), cex = 1.2)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (xPositions [c(6,7,8)]), cex = 1.2)
  
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [2], sum (cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'B'], 
  #         pch = 25, 
  #         col = addOpacity (colours [2], ALPHA), 
  #         bg  = addOpacity (colours [2], ALPHA), cex = 2)
  # 
  # # add above the girdling
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [3], sum (cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'A'], 
  #         pch = 24, bg = addOpacity (colours [2], ALPHA), 
  #         col = addOpacity (colours [2], ALPHA), cex = 2)
  # 
  # # add below the compression
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [4], sum (cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'B'], 
  #         pch = 25, 
  #         bg  = addOpacity (colours [3], ALPHA),
  #         col = addOpacity (colours [3], ALPHA), cex = 2)
  # 
  # # add above the compression
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [5], sum (cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'A'], 
  #         pch = 24, 
  #         bg  = addOpacity (colours [3], ALPHA), 
  #         col = addOpacity (colours [3], ALPHA), cex = 2)
  # 
  # # add below the double compression
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [6], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'B'], 
  #         pch = 25, 
  #         bg  = addOpacity (colours [4], ALPHA),
  #         col = addOpacity (colours [4], ALPHA), cex = 2)
  # 
  # # add in between the double compression
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [7], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'M', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'M'], 
  #         pch = 22, 
  #         bg  = addOpacity (colours [4], ALPHA),
  #         col = addOpacity (colours [4], ALPHA), cex = 2)
  # 
  # # add above the double compression
  # #--------------------------------------------------------------------------------------
  # points (y = rep (xPositions [8], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
  #         x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'A'], 
  #         pch = 24, 
  #         bg  = addOpacity (colours [4], ALPHA),
  #         col = addOpacity (colours [4], ALPHA), cex = 2)
  
  # add standard deviation
  #--------------------------------------------------------------------------------------
  arrows (y0 = xPositions,
          x0 = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)] - cellNSD [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)], 
          x1 = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)] + cellNSD [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)], 
          angle = 90, length = 0.05,
          col = '#333333', 
          code = 3, lwd = 2)
  
  # add means of cell numbers
  #--------------------------------------------------------------------------------------
  points (y = xPositions,
          x = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)],
          col = colours [c (1, 2, 2, 3, 3, 4, 4, 4)], 
          pch = c (21, 25, 24, 25, 24, 25, 22, 24),
          lwd = 2, bg = 'white', cex = 2)
dev.off ()

#========================================================================================
