#========================================================================================
# Script to make figures with regard to structural carbon in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# Load colour scheme and anatomical data 
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')
source ('processAnatomicalData.R')

# Plot cell wall area against ring width for each tree and group
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
con <- data [['tree']] == 1 & data [['YEAR']] == 2017
plot (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
      y = rollmean (data [['CWA']] [con], 5), axes = FALSE, typ = 'l',
      xlab = 'radial distance (microns)', 
      ylab = expression (paste ('cell wall area (',microns**2,')')),
      xlim = c (0, 4000), ylim = c (0, 1200), col = tColours [['colour']] [1])
axis (side = 1); axis (side = 2)
for (i in c (3, 4, 6, 7, 9, 18, 30, 31, 36)){
  con <- data [['tree']] == i & data [['YEAR']] == 2017
  lines (x = rollmean (data [['RADDISTR.BAND']] [con], 5),
         y = rollmean (data [['CWA']] [con], 5), 
         xlim = c (0, 4000), ylim = c (0, 1200), col = tColours [['colour']] [1])
}


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
