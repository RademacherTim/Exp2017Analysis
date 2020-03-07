#========================================================================================
# Script to make figures with regard to structural carbon in the 2017 experiment at 
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

# function for calculate the standard error
#----------------------------------------------------------------------------------------
se <-  function (x) {
  sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x)))
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

# plot cell wall tichkness of the 2017 ring for each period and tree starting with control trees
png (filename = '../fig/Exp2017CellWallThickness.png', width = 800, height = 500)
par (mar = c (3, 5, 1, 1), mfrow = c (2, 2))
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees], 0.5)),
      y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'cell wall thickness (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (1, 8))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 7.7, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements above the girld
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove], 0.5) - offset),
      y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'cell wall thickness (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (1, 8))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 7.7, cex = 1.5, labels = 'b', col = '#666666')
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements below the girld
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in%girdledTreesBelow], 0.5) + offset),
        y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow],
        col = addOpacity (colours [2], ALPHA), pch = 1)
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [2], lwd = 2)

# add measurements above the compression
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove], 0.5) - offset),
      y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'cell wall thickness (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (1, 8))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 7.7, cex = 1.5, labels = 'c', col = '#666666')
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements below the compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow], 0.5) + offset),
        y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow],
        col = addOpacity (colours [3], ALPHA), pch = 1)
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [3], lwd = 2)

# add measurements above the double compression
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove], 0.5) - 1.5* offset),
      y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'cell wall thickness (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (1, 8))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 7.7, cex = 1.5, labels = 'd', col = '#666666')
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - 1.5 * offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - 1.5 * offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

# add measurements in between the double compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet], 0.5)),
        y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet],
        col = addOpacity (colours [4], ALPHA), pch = 0)
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#777777', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#777777', pch = 22, bg = colours [4], lwd = 2)

# add measurements below the double compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow], 0.5) + 1.5 * offset),
        y = data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow],
        col = addOpacity (colours [4], ALPHA), pch = 1)
# add mean
periodMeans <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow], 
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow])), 
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['CWTALL']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow], 
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow])), 
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + 1.5* offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + 1.5 * offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [4], lwd = 2)
dev.off ()

# plot radial lumen diameter of the 2017 ring for each period and tree starting with control trees
png (filename = '../fig/Exp2017LumenDiameter.png', width = 800, height = 500)
par (mar = c (3, 5, 1, 1), mfrow = c (2, 2))
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees], 0.5)),
      y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees],
      las = 1, typ = 'p', col = addOpacity (colours [1], ALPHA), pch = 22,
      ylab = 'radial lumen diameter (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 60))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 55, cex = 1.5, labels = 'a', col = '#666666')
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% controlTrees])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#999999', pch = 22, bg = colours [1], lwd = 2)

# add measurements above the girld
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove], 0.5) - offset),
      y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [2], ALPHA), pch = 23,
      ylab = 'radial lumen diameter (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 60))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 55, cex = 1.5, labels = 'b', col = '#666666')
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesAbove])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [2], lwd = 2)

# add measurements below the girld
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in%girdledTreesBelow], 0.5) + offset),
        y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow],
        col = addOpacity (colours [2], ALPHA), pch = 1)
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% girdledTreesBelow])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [2], lwd = 2)

# add measurements above the compression
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove], 0.5) - offset),
      y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [3], ALPHA), pch = 23,
      ylab = 'radial lumen diameter (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 60))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 55, cex = 1.5, labels = 'c', col = '#666666')
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesAbove])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [3], lwd = 2)

# add measurements below the compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow], 0.5) + offset),
        y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow],
        col = addOpacity (colours [3], 0.2), pch = 1)
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% compresTreesBelow])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [3], lwd = 2)

# add measurements above the double compression
plot (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove], 0.5) - 1.5* offset),
      y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove],
      las = 1, typ = 'p', col = addOpacity (colours [4], ALPHA), pch = 23,
      ylab = 'radial lumen diameter (microns)',
      xlab = '',
      xlim = as_date (c ('2017-05-01','2017-11-30')),
      ylim = c (0, 60))
# add panel indicator
text (x = as_date ('2017-05-05'), y = 55, cex = 1.5, labels = 'd', col = '#666666')
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesAbove])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] - 1.5 * offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#999999', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] - 1.5 * offset,
        y = periodMeans [[2]],
        col = '#999999', pch = 23, bg = colours [4], lwd = 2)

# add measurements in between the double compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet], 0.5)),
        y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet],
        col = addOpacity (colours [4], ALPHA), pch = 0)
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesInBet])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]],
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#777777', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]],
        y = periodMeans [[2]],
        col = '#777777', pch = 22, bg = colours [4], lwd = 2)

# add measurements below the double compression
points (x = as_date (jitter (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow], 0.5) + 1.5 * offset),
        y = data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow],
        col = addOpacity (colours [4], ALPHA), pch = 1)
# add mean
periodMeans <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow],
                          by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow])),
                          mean, na.rm = TRUE)
periodSD <- aggregate (data [['DRAD']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow],
                       by = list (as_date (data [['period']] [data [['YEAR']] == 2017 & data [['TREE']] %in% douCompTreesBelow])),
                       sd, na.rm = TRUE)
arrows (x0 = periodMeans [[1]] + 1.5* offset,
        y0 = periodMeans [[2]] - periodSD [[2]],
        y1 = periodMeans [[2]] + periodSD [[2]],
        col = '#555555', code = 3, length = 0.05, angle = 90,lwd = 2)
points (x = periodMeans [[1]] + 1.5 * offset,
        y = periodMeans [[2]],
        col = '#555555', pch = 21, bg = colours [4], lwd = 2)
dev.off ()

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
cellNSE    <- aggregate (cellNumber [['n']], 
                         by = c (list (cellNumber [['height']]), list (cellNumber [['treatment']])), 
                         FUN = function (x) sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x))))

# plot box plot of the estimated end time of growth in each treatment
xPositions <- c (0.8, 1.8, 2.3, 3.3, 3.8, 4.8, 5.3, 5.8)


ALPHA <- 0.5

# plot the estimated mean end of growth for each treatment
png (filename = '../fig/Exp2017EndOfGrowthAndCellNumbers.png', width = 600)
par (mar = c (5, 5, 5, 1))
treeLabels <- c ('01M','03M','04M','06M','07M','09M','18M','30M','31M','36M')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# plot of the means and standard errors
plot (y = xPositions [1],
      x = as_date (mean (lastDates [[2]])), 
      xlab = '', ylab = '',
      col = 'white', las = 1, axes = FALSE,
      ylim = c (0, 6), xlim = as_date (c ('2017-03-01','2017-11-15')))
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))), 
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))), 
        y0 = xPositions [1], col = '#999999', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [1],
        x = as_date (mean (lastDates [[2]], na.rm = TRUE)), 
        col = colours [1], las = 1, pch = 22, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for below the girdl
treeLabels <- c ('05B','11B','15B','16B','19B','23B','29B','35B','39B','40B')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for below the girdl
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [2], col = '#555555', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [2],
        x = as_date (mean (lastDates [[2]])), 
        col = colours [2], las = 1, pch = 21, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for above the girdl
treeLabels <- c ('05A','11A','15A','16A','19A','23A','29A','35A','39A','40A')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for above the girdl
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))), 
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [3], col = '#999999', lwd = 2, angle = 90, length = 0.1, code = 3)
points (y = xPositions [3],
        x = as_date (mean (lastDates [[2]])), 
        col = colours [2], las = 1, pch = 23, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for below the compression
treeLabels <- c ('10B','12B','13B','17B','20B','21B','28B','32B','33B','38B')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for below the compression
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [4], col = '#555555', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [4],
        x = as_date (mean (lastDates [[2]])), 
        col = colours [3], las = 1, pch = 21, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for above the compression
treeLabels <- c ('10A','12A','13A','17A','20A','21A','28A','32A','33A','38A')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for above the compression
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))), 
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [5], col = '#999999', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [5],
        x = as_date (mean (lastDates [[2]])), 
        col = colours [3], las = 1, pch = 23, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for below the double compression
treeLabels <- c ('02B','08B','14B','22B','24B','25B','26B','27B','34B','37B')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for below the double compression
arrows (x0 = as_date (mean (lastDates [[2]], na.rm = TRUE) - (sd (lastDates [[2]], na.rm = TRUE) / 
                                                                sqrt (sum (!is.na(lastDates [[2]]))))),
        x1 = as_date (mean (lastDates [[2]], na.rm = TRUE) + (sd (lastDates [[2]], na.rm = TRUE) / 
                                                                sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [6], col = '#555555', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [6],
        x = as_date (mean (lastDates [[2]], na.rm = TRUE)), 
        col = colours [4], pch = 21, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for below the double compression
treeLabels <- c ('02M','08M','14M','22M','24M','25M','26M','27M','34M','37M')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for between the double compression
arrows (x0 = as_date (mean (lastDates [[2]]) - (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))), 
        x1 = as_date (mean (lastDates [[2]]) + (sd (lastDates [[2]]) / sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [7], col = '#777777', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [7],
        x = as_date (mean (lastDates [[2]])), 
        col = colours [4], pch = 22, cex = 1.5, lwd = 2, bg = 'white')
# determine the last day of growth for above the double compression
treeLabels <- c ('02A','08A','14A','22A','24A','25A','26A','27A','34A','37A')
lastDates <- aggregate (data [['formationDate']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels], 
                        by = list (data [['TREE']] [data [['YEAR']] == 2017 & data [['TREE']] %in% treeLabels]), 
                        max, na.rm = TRUE)
lastDates [is.infinite (lastDates [, 2]), 2] <- NA
# add mean and standard deviation for below the double compression
arrows (x0 = as_date (mean (lastDates [[2]], na.rm = TRUE) - (sd (lastDates [[2]], na.rm = TRUE) / 
                                                                sqrt (sum (!is.na(lastDates [[2]]))))), 
        x1 = as_date (mean (lastDates [[2]], na.rm = TRUE) + (sd (lastDates [[2]], na.rm = TRUE) / 
                                                                sqrt (sum (!is.na(lastDates [[2]]))))),
        y0 = xPositions [8], col = '#999999', lwd = 2, angle = 90, length = 0.05, code = 3)
points (y = xPositions [8],
        x = as_date (mean (lastDates [[2]], na.rm = TRUE)), 
        col = colours [4], pch = 23, cex = 1.5, lwd = 2, bg = 'white')
# add y axis
axis (side = 2, at = xPositions, 
      labels = c ('M','B','A','B','A','B','M','A'), 
      tick = 1, las = 1)
# add x-axis 
axis (side = 1, 
      at = as_date (c ('2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01')), 
      labels = c ('Jul','Aug','Sep','Oct','Nov'))
# add x-axis description
mtext (side = 1, line = 3, at = as_date ('2017-08-25'), text = 'estimated end of growth')
# add treatments
mtext (side = 2, line = 2, text = 'control', at = xPositions [1], cex = 0.8)
mtext (side = 2, line = 2, text = 'girdled', at = mean (xPositions [c(2,3)]), cex = 0.8)
mtext (side = 2, line = 2, text = 'compressed', at = mean (xPositions [c(4,5)]), cex = 0.8)
mtext (side = 2, line = 2.5, text = 'double', at = mean (xPositions [c(6,7,8)]), cex = 0.8)
mtext (side = 2, line = 2, text = 'compressed', at = mean (xPositions [c(6,7,8)]), cex = 0.8)

# add new plot of cell number
par (new = TRUE)
plot (y = rep (xPositions [1], sum (cellNumber [['treatment']] == 1, na.rm = TRUE )), 
      x = cellNumber [['n']] [cellNumber [['treatment']] == 1], 
      xlab = '', ylab = '', axes = FALSE,
      pch = 15, col = addOpacity (colours [1], ALPHA), ylim = c (0, 6), xlim = c (0, 300))
# add y axis and margin text
axis (side = 3, at = seq (0, 120, by  = 20))
mtext (side = 3, line = 3, at = 60, text = 'mean number of cells in final ring (n)')
# add below the girdling
points (y = rep (xPositions [2], sum (cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'B'], 
        pch = 16, col = addOpacity (colours [2], ALPHA))
# add above the girdling
points (y = rep (xPositions [3], sum (cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 2 & cellNumber [['height']] == 'A'], 
        pch = 18, col = addOpacity (colours [2], ALPHA))
# add below the compression
points (y = rep (xPositions [4], sum (cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'B'], 
        pch = 16, col = addOpacity (colours [3], ALPHA))
# add above the compression
points (y = rep (xPositions [5], sum (cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 3 & cellNumber [['height']] == 'A'], 
        pch = 18, col = addOpacity (colours [3], ALPHA))
# add below the double compression
points (y = rep (xPositions [6], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'B', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'B'], 
        pch = 16, col = addOpacity (colours [4], ALPHA))
# add in between the double compression
points (y = rep (xPositions [7], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'M', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'M'], 
        pch = 15, col = addOpacity (colours [4], ALPHA))
# add above the double compression
points (y = rep (xPositions [8], sum (cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'A', na.rm = TRUE )), 
        x = cellNumber [['n']] [cellNumber [['treatment']] == 4 & cellNumber [['height']] == 'A'], 
        pch = 18, col = addOpacity (colours [4], ALPHA))
# add standard deviation
arrows (y0 = xPositions,
        x0 = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)] - cellNSE [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)], 
        x1 = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)] + cellNSE [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)], 
        angle = 90, length = 0.05,
        col = c ('#999999', rep (c ('#555555', '#999999'), 2), '#555555', '#777777', '#999999'), 
        code = 3, lwd = 2)
# add means of cell numbers
points (y = xPositions,
        x = cellNMeans [[3]] [c (1, 3, 2, 5, 4, 7, 8, 6)],
        col = colours [c (1, 2, 2, 3, 3, 4, 4, 4)], 
        pch = c (22, 21, 23, 21, 23, 21, 22, 23),
        lwd = 2, bg = 'white', cex = 1.5)
dev.off ()

#========================================================================================
