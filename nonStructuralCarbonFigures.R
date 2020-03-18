#========================================================================================
# Script to make figures with regard to nonstructural carbon in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
colours <- c ('#91b9a4','#C0334D','#F18904','#5C4A72','#23345C')

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

# function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}

ALPHA  <- 0.6 # add transparency to better see all symbols

# create yPosition and plotting symbols for the stemData
yPositions <- c (0.8, 1.8, 2.3, 3.3, 3.8, 4.8, 5.3, 5.8)
stemData2017 [['y']] <- NA ; stemData2017 [['pch']] <- NA
con <- which (stemData2017 [['treatment']] == 1)
stemData2017 [['y']] [con] <- yPositions [1]
stemData2017 [['p']] <- 15

con <- which (stemData2017 [['treatment']] == 2 & stemData2017 [['sampleHeight']] == 1)
stemData2017 [['y']] [con] <- yPositions [2]
stemData2017 [['p']] [con] <- 16

con <- which (stemData2017 [['treatment']] == 2 & stemData2017 [['sampleHeight']] == 2)
stemData2017 [['y']] [con] <- yPositions [3]
stemData2017 [['p']] [con] <- 18

con <- which (stemData2017 [['treatment']] == 3 & stemData2017 [['sampleHeight']] == 1)
stemData2017 [['y']] [con] <- yPositions [4]
stemData2017 [['p']] [con] <- 16

con <- which (stemData2017 [['treatment']] == 3 & stemData2017 [['sampleHeight']] == 2)
stemData2017 [['y']] [con] <- yPositions [5]
stemData2017 [['p']] [con] <- 18

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 0.5)
stemData2017 [['y']] [con] <- yPositions [6]
stemData2017 [['p']] [con] <- 16

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 1.5)
stemData2017 [['y']] [con] <- yPositions [7]
stemData2017 [['p']] [con] <- 15

con <- which (stemData2017 [['treatment']] == 4 & stemData2017 [['sampleHeight']] == 2.5)
stemData2017 [['y']] [con] <- yPositions [8]
stemData2017 [['p']] [con] <- 18
  
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
png (filename = '../fig/Exp2017LeafSugarConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (5, 9, 6, 0))
  plot (y = 1:4,
        x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', 
#        xaxt = 'n', yaxt = 'n',
        axes = FALSE,
        xlim = c (-1, 13), ylim = c (0, 5))

  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))

  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'girdled', 'compressed', expression ('    double \ncompressed')),
        tick = 1)
  
  # add month label
  #--------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # add tissues label
  #--------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'leaves', cex = 2)
  
  # individual leaf sugar measurements as points
  #--------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']] [leafData2017 [['date']] == '2017-07-05'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation of leaf sugar by treatment
  #--------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-07-05'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-07-05'] + 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-07-05'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
  points (x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-07-05'],
          y = leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05'],
          col = colours [leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # plot august leaf sugar concentrations
  #--------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual leaf sugar measurements form august  as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']]     [leafData2017 [['date']] == '2017-08-10'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation for august leaf sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'] + 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-08-10'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # add title
  #----------------------------------------------------------------------------------------
  mtext (side = 3, line = 2, text = 'sugar', at = 12.5, cex = 2)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 12.5, cex = 1)
  
  # plot october leaf sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']]     [leafData2017 [['date']] == '2017-10-09'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'] +  
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-10-09'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 13.5, col = '#666666')
  
  # plot november leaf sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 1))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
        x = leafMeans [['sugar']]  [leafMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-1, 13), ylim = c (0, 5))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 12, by = 3))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of leaf sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['sugar']] [leafData2017 [['date']] == '2017-11-03'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation for leaf sugar in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'] - 
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          x1 = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'] +  
               leafSDs [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['sugar']] [leafMeans [['date']] == '2017-11-03'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
dev.off ()

# plot leafstarch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafStarchConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (5, 5, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
        x = leafMeans [['starch']]    [leafMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'gridled', 'compressed', expression ('    double \ncompressed')), 
        tick = 1)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 1.5, pos = 4)

  # individual measurements of leaf starch in July as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-07-05'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation for leaf starch in July
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-07-05'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-07-05'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-07-05'] + 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-07-05'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-07-05'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-07-05'],
          col = colours [leafMeans [['treatment']]  [leafMeans [['date']] == '2017-07-05']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # plot august concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 0))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of leaf starch in august as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-08-10'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation for leaf starch in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-08-10'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'] + 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-08-10'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-08-10'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
  
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # add title
  #----------------------------------------------------------------------------------------
  mtext (side = 3, line = 2, text = 'starch', at = 7.5, cex = 2)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 7.5, cex = 1)
  
  # plot october concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 0))
  plot (y =  leafMeans [['treatment']] [leafMeans [['date']] == '2017-08-10'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of leaf starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-10-09'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation for elaf starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-10-09'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'] +  
               leafSDs [['starch']] [leafMeans [['date']] == '2017-10-09'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-10-09'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-10-09']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
 
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 7.8, col = '#666666')
  
  # plot november concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 6, 1))
  plot (y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
        x = leafMeans [['starch']]  [leafMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.5, 7.5), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:7)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = leafData2017 [['starch']] [leafData2017 [['date']] == '2017-11-03'], 
          y = leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [leafData2017 [['treatment']] [leafData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = 18, cex = 1.5)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'] - 
               leafSDs [['starch']] [leafMeans [['date']] == '2017-11-03'],
          x1 = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'] +  
               leafSDs [['starch']] [leafMeans [['date']] == '2017-11-03'],
          y0 = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = leafMeans [['starch']] [leafMeans [['date']] == '2017-11-03'],
          y = leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03'],
          col = colours [leafMeans [['treatment']] [leafMeans [['date']] == '2017-11-03']], 
          pch = 23, bg = 'white', lwd = 2, cex = 2)
dev.off ()

# plot wood sugar concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemSugarConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (5, 9, 1, 0))
  plot (y = stemMeans [['y']]     [stemMeans [['date']] == '2017-07-05'],
        x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', 
        axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, 
        labels = c ('M','B','A','B','A','B','M','A'), 
        tick = 1, las = 1)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # add treatments
  #----------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control', at = yPositions [1], cex = 0.8)
  mtext (side = 2, line = 2, text = 'girdled', at = mean (yPositions [c(2,3)]), cex = 0.8)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]), cex = 0.8)
  mtext (side = 2, line = 3, text = 'double', at = mean (yPositions [c(6,7,8)]), cex = 0.8)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]), cex = 0.8)
  mtext (side = 2, line = 6, text = 'wood', cex = 2)
  
  # individual measurements of wood sugar in july as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-07-05'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-07-05'], cex = 1.5)
  
  # add mean and standard deviation for july wood sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-07-05'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-07-05'] + 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-07-05'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-07-05'],
          y = yPositions,
          col = colours [stemMeans [['treatment']]  [stemMeans [['date']] == '2017-07-05']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot august wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements of wood sugar in august as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-08-10'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-08-10'],
          cex = 1.5)
  
  # add mean and standard deviation for wood sugar in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'] + 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-08-10'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-08-10']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 2.5, cex = 1)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot october wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements of wood sugar in october as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-10-09'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-10-09'], cex = 1.5)
  
  # add mean and standard deviation for wood sugar in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'] +  
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-10-09'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-10-09']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 2.6, col = '#666666')
  
  # plot november wood sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  plot (y = yPositions,
        x = stemMeans [['sugar']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.1, 2.6), ylim = c (0, 6.8))
  
  # add x-axis 
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = 0:2)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements of wood sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['sugar']] [stemData2017 [['date']] == '2017-11-03'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-11-03'], cex = 1.5)
  
  # add mean and standard deviation of november wood sugar concentrations
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'] - 
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          x1 = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'] +  
               stemSDs [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['sugar']] [stemMeans [['date']] == '2017-11-03'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-11-03']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
dev.off ()

# plot wood starch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemStarchConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (5, 5, 1, 0))
  plot (y = stemMeans [['y']]     [stemMeans [['date']] == '2017-07-05'],
        x = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
        
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, 
        labels = c ('M','B','A','B','A','B','M','A'), 
        tick = 1, las = 1)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # add treatments
  #----------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control', at = yPositions [1], cex = 0.8)
  mtext (side = 2, line = 2, text = 'girdled', at = mean (yPositions [c(2,3)]), cex = 0.8)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]), cex = 0.8)
  mtext (side = 2, line = 3, text = 'double', at = mean (yPositions [c(6,7,8)]), cex = 0.8)
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]), cex = 0.8)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-07-05'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-07-05'], cex = 1.5)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-07-05'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-07-05'] + 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-07-05'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-07-05'],
          y = yPositions,
          col = colours [stemMeans [['treatment']]  [stemMeans [['date']] == '2017-07-05']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)

  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot august wood starch concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = yPositions,
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements for august wood starch as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-08-10'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-08-10'], cex = 1.5)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-08-10'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'] + 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-08-10'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-08-10'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-08-10']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 1, cex = 1)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot october concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = rev (yPositions),
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes =FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements of wood starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-10-09'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-10-09'], cex = 1.5)
  
  # add mean and standard deviation for wood starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-10-09'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'] +  
               stemSDs [['starch']] [stemMeans [['date']] == '2017-10-09'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-10-09'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-10-09']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.1, col = '#666666') 
  
  # plot november concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  plot (y = rev (yPositions),
        x = stemMeans [['starch']]  [stemMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.1), ylim = c (0, 6.8))
  
  # add y axis
  #----------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1, by = 0.5)) 
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 6.8, cex = 1.5, pos = 4)
  
  # individual measurements of wood starch in november as points
  #----------------------------------------------------------------------------------------
  points (x = stemData2017 [['starch']] [stemData2017 [['date']] == '2017-11-03'], 
          y = stemData2017 [['y']]     [stemData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [stemData2017 [['treatment']] [stemData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = stemData2017 [['p']] [stemData2017 [['date']] == '2017-11-03'], cex = 1.5)
  
  # add mean and standard deviation for wood starch in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'] - 
               stemSDs [['starch']] [stemMeans [['date']] == '2017-11-03'],
          x1 = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'] +  
               stemSDs [['starch']] [stemMeans [['date']] == '2017-11-03'],
          y0 = yPositions,
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = stemMeans [['starch']] [stemMeans [['date']] == '2017-11-03'],
          y = yPositions,
          col = colours [stemMeans [['treatment']] [stemMeans [['date']] == '2017-11-03']], 
          pch = c (22, 21, 23, 21, 23, 21, 22, 23), 
          bg = 'white', lwd = 2, cex = 2)
dev.off ()

# plot root sugar concentrations for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017rootSugarConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.5, 1, 1, 1.1))
  par (mar = c (5, 9, 1, 0))
  plot (y = 1:4,
        x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
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
        tick = 1)
  
  # add month label
  #--------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # add tissues label
  #--------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'roots', cex = 2)
  
  # individual root sugar measurements as points
  #--------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']] [rootData2017 [['date']] == '2017-07-05'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation of root sugar by treatment
  #--------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'] - 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-07-05'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-07-05'] + 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-07-05'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
  points (x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-07-05'],
          y = rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05'],
          col = colours [rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # plot august root sugar concentrations
  #--------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual root sugar measurements form august  as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']]     [rootData2017 [['date']] == '2017-08-10'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for august root sugar concentrations by treatment
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'] - 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'] + 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-08-10'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 5.5, cex = 1)
  
  # plot october root sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']]     [rootData2017 [['date']] == '2017-10-09'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'] - 
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'] +  
            rootSDs [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-10-09'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 5.6, col = '#666666')
  
  # plot november root sugar concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
        x = rootMeans [['sugar']]  [rootMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #xaxt = 'n', yaxt = 'n',
        xlim = c (-0.5, 5.5), ylim = c (0, 5))
  
  # add x axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = 0:5)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of root sugar in november as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['sugar']] [rootData2017 [['date']] == '2017-11-03'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for root sugar in November
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'] - 
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          x1 = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'] +  
               rootSDs [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['sugar']] [rootMeans [['date']] == '2017-11-03'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
dev.off ()

# plot rootstarch concentrations of 1 cm for each period
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017rootStarchConcentrations.png', width = 800, height = 500)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1, 1.1))
  par (mar = c (5, 5, 1, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
        x = rootMeans [['starch']]    [rootMeans [['date']] == '2017-07-05'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add y axis
  #--------------------------------------------------------------------------------------
  axis (side = 2, at = 1:4, 
        labels = c ('control', 'gridled', 'compressed', expression ('    double \ncompressed')), 
        tick = 1)
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('july', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of root starch in July as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-07-05'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-07-05']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for root starch in July
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-07-05'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-07-05'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-07-05'] + 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-07-05'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-07-05'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-07-05'],
          col = colours [rootMeans [['treatment']]  [rootMeans [['date']] == '2017-07-05']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')

  # plot august concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('august', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of root starch in august as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-08-10'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-08-10']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for root starch in august
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-08-10'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'] + 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-08-10'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-08-10'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')
  
  # add x-axis description
  #----------------------------------------------------------------------------------------
  mtext (side = 1, line = 3, text = 'concentration (% dry weight)', at = 1.7, cex = 1)
  
  # plot october concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 0))
  plot (y =  rootMeans [['treatment']] [rootMeans [['date']] == '2017-08-10'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-08-10'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('october', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements of root starch in october as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-10-09'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-10-09']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for elaf starch in october
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-10-09'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'] +  
               rootSDs [['starch']] [rootMeans [['date']] == '2017-10-09'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-10-09'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-10-09']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
  
  # add line to separate the plots
  #--------------------------------------------------------------------------------------
  abline (v = 1.7, col = '#666666')
  
  # plot november concentrations
  #----------------------------------------------------------------------------------------
  par (mar = c (5, 0, 1, 1))
  plot (y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
        x = rootMeans [['starch']]  [rootMeans [['date']] == '2017-11-03'],
        las = 1, typ = 'p', 
        col = 'white', 
        pch = 22, cex = 1.5, lwd = 2,
        xlab = '', ylab = '', axes = FALSE, #yaxt = 'n',
        xlim = c (-0.1, 1.7), ylim = c (0, 5))
  
  # add x axis 
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0, 1.5, by = 0.5))
  
  # add month label
  #----------------------------------------------------------------------------------------
  text ('november', x = 0, y = 5, cex = 1.5, pos = 4)
  
  # individual measurements november root starch as points
  #----------------------------------------------------------------------------------------
  points (x = rootData2017 [['starch']] [rootData2017 [['date']] == '2017-11-03'], 
          y = rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03'], 
          col = addOpacity (colours [rootData2017 [['treatment']] [rootData2017 [['date']] == '2017-11-03']], ALPHA),
          pch = 19, cex = 1.5)
  
  # add mean and standard deviation for november root starch
  #----------------------------------------------------------------------------------------
  arrows (x0 = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'] - 
               rootSDs [['starch']] [rootMeans [['date']] == '2017-11-03'],
          x1 = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'] +  
               rootSDs [['starch']] [rootMeans [['date']] == '2017-11-03'],
          y0 = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = '#999999', code = 3, length = 0.05, angle = 90, lwd = 2)
  points (x = rootMeans [['starch']] [rootMeans [['date']] == '2017-11-03'],
          y = rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03'],
          col = colours [rootMeans [['treatment']] [rootMeans [['date']] == '2017-11-03']], 
          pch = 21, bg = 'white', lwd = 2, cex = 2)
dev.off ()