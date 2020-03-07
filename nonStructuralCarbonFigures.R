#========================================================================================
# Script to make figures with regard to nonstructural carbon in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
colours <- c ('#91b9a4','#C0334D','#F18904','#5C4A72','#23345C')

# Function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}

# define tree labels for each group
#----------------------------------------------------------------------------------------
controlTrees <- c ( 1,  3,  4,  6,  7,  9, 18, 30, 31, 36)
girdledTrees <- c ( 5, 11, 15, 16, 19, 23, 29, 35, 39, 40)
compresTrees <- c (10, 12, 13, 17, 20, 21, 28, 32, 33, 38)
douCompTrees <- c ( 2,  8, 14, 22, 24, 25, 26, 27, 34, 37)

ALPHA  <- 0.5 # add transparency to better see all symbols

# plot wood sugar concentrations  of 1 cm for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemSugarConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees],
      y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 2.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 2.4, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% controlTrees], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% controlTrees], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements above the girdl
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0] - days (4),
      y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 2.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 2.4, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (4),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements below the girdl
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0] + days (4),
        y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0],
        col = addOpacity (colours [2], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (4),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [2], lwd = 2)

# add measurements above the compression
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0] - days (4),
      y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 2.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y =2.4, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (4),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements below the compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0] + days (4),
        y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0],
        col = addOpacity (colours [3], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (4),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [3], lwd = 2)

# add measurements above the double compression
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5] - days (7),
      y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 2.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 2.4, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (7),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (7),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

# add measurements in betwee the double compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5],
        y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5],
        col = addOpacity (colours [4], ALPHA), pch = 0)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#777777', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#777777', pch = 22, bg = colours [4], lwd = 2)

# add measurements below the double compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5] + days (7),
        y = stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5],
        col = addOpacity (colours [4], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['sugar']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (7),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (7),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [4], lwd = 2)
dev.off ()

# plot wood starch concentrations of 1 cm for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemStarchConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees],
      y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 0.9, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% controlTrees], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% controlTrees], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements above the girdl
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0] - days (4),
      y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 0.9, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (4),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements below the girdl
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0] + days (4),
        y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0],
        col = addOpacity (colours [2], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% girdledTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (4),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [2], lwd = 2)

# add measurements above the compression
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0] - days (4),
      y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 0.9, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 2.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (4),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements below the compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0] + days (4),
        y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0],
        col = addOpacity (colours [3], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% compresTrees & stemData2017 [['sampleHeight']] == 1.0]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (4),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (4),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [3], lwd = 2)

# add measurements above the double compression
#----------------------------------------------------------------------------------------
plot (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5] - days (7),
      y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 0.9, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 2.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - days (7),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - days (7),
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

# add measurements in betwee the double compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5],
        y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5],
        col = addOpacity (colours [4], ALPHA), pch = 0)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 1.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#777777', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#777777', pch = 22, bg = colours [4], lwd = 2)

# add measurements below the double compression
#----------------------------------------------------------------------------------------
points (x = stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5] + days (7),
        y = stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5],
        col = addOpacity (colours [4], ALPHA), pch = 1)
# add mean and standard deviation
periodMeans <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5], 
                          by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (stemData2017 [['starch']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5], 
                       by = list (stemData2017 [['date']] [stemData2017 [['treeID']] %in% douCompTrees & stemData2017 [['sampleHeight']] == 0.5]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + days (7),
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + days (7),
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [4], lwd = 2)
dev.off ()

# plot leaf sugar concentrations for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafSugarConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees],
      y = leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 12))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 11, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% controlTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% controlTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements from girdled trees
#----------------------------------------------------------------------------------------
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees],
      y = leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% girdledTrees],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 12))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 11, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% girdledTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% girdledTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements from compressed trees
#----------------------------------------------------------------------------------------
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees],
      y = leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% compresTrees],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 12))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 11, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% compresTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% compresTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements the double compressed trees
#----------------------------------------------------------------------------------------
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees],
      y = leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% douCompTrees],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 12))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 11, cex = 1.5, labels = 'd', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% douCompTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['sugar']] [leafData2017 [['treeID']] %in% douCompTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

dev.off ()

# plot leaf starch concentrations for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafStarchConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees],
      y = leafData2017 [['starch']] [leafData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 7.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 6.7, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% controlTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% controlTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements for the girlded trees
#----------------------------------------------------------------------------------------
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees],
      y = leafData2017 [['starch']] [leafData2017 [['treeID']] %in% girdledTrees],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 7.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 6.7, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% girdledTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% girdledTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% girdledTrees ]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements for the compressed trees
#----------------------------------------------------------------------------------------
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees],
      y = leafData2017 [['starch']] [leafData2017 [['treeID']] %in% compresTrees],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 7.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 6.7, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% compresTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% compresTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% compresTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements for the double compressed trees
plot (x = leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees],
      y = leafData2017 [['starch']] [leafData2017 [['treeID']] %in% douCompTrees],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 7.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 6.7, cex = 1.5, labels = 'd', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% douCompTrees], 
                          by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (leafData2017 [['starch']] [leafData2017 [['treeID']] %in% douCompTrees], 
                       by = list (leafData2017 [['date']] [leafData2017 [['treeID']] %in% douCompTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)
dev.off ()

# plot root sugar concentrations for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootSugarConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees],
      y = rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 5.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 5, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% controlTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% controlTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements from girdled trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees],
      y = rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% girdledTrees],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0,  5.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 5, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% girdledTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% girdledTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements from compressed trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees],
      y = rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% compresTrees],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 5.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 5, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% compresTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% compresTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements the double compressed trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees],
      y = rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% douCompTrees],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'soluble sugar concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 5.5))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 5, cex = 1.5, labels = 'd', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% douCompTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['sugar']] [rootData2017 [['treeID']] %in% douCompTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

dev.off ()

# plot root starch concentrations for each period and tree starting with control trees
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootStarchConcentrations.png', width = 800, height = 500)
par (mar = c (5, 5, 1, 1), mfrow = c (2, 2))
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees],
      y = rootData2017 [['starch']] [rootData2017 [['treeID']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1.6))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 1.5, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% controlTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% controlTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% controlTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements for the girlded trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees],
      y = rootData2017 [['starch']] [rootData2017 [['treeID']] %in% girdledTrees],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1.6))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 1.5, cex = 1.5, labels = 'b', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% girdledTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% girdledTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% girdledTrees ]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements for the compressed trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees],
      y = rootData2017 [['starch']] [rootData2017 [['treeID']] %in% compresTrees],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1.6))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 1.5, cex = 1.5, labels = 'c', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% compresTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% compresTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% compresTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements for the double compressed trees
#----------------------------------------------------------------------------------------
plot (x = rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees],
      y = rootData2017 [['starch']] [rootData2017 [['treeID']] %in% douCompTrees],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'starch concentration (% dry weight)',
      xlab = '',
      xlim = as.POSIXct (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 1.6))
# add panel indicator
text (x = as.POSIXct ('2017-05-05'), y = 1.5, cex = 1.5, labels = 'd', col = '#666666')
# add mean and standard deviation
periodMeans <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% douCompTrees], 
                          by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees]), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (rootData2017 [['starch']] [rootData2017 [['treeID']] %in% douCompTrees], 
                       by = list (rootData2017 [['date']] [rootData2017 [['treeID']] %in% douCompTrees]), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)
dev.off ()

#========================================================================================