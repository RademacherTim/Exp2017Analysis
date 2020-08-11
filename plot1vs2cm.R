#========================================================================================
# Script to plot shallow wood versus deeper wood tissues
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
source ('/home/tim/projects/PlantGrowth/nonstructuralCarbon/processExpNSCData.R')
source ('plotingFunctions.R')

# Plot scatterplot of sugar in 1cm versus sugar in second cm for July
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                     treatment == 1) [['sugar']],
      y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                     treatment == 1) [['sugar']],
      las = 1, xlab = 'soluble sugar concentration in 0-1 cm (% dry weight)', xlim = c (0, 1.6), ylim = c (0, 1.6),
      ylab = 'soluble sugar concentration in 1-2 cm (% dry weight)', col = tColours [['colour']] [1], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                     treatment == 2) [['sugar']],
      y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                     treatment == 2) [['sugar']], col = tColours [['colour']] [2], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                       treatment == 3) [['sugar']],
        y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                       treatment == 3) [['sugar']], col = tColours [['colour']] [3], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                       treatment == 4) [['sugar']],
        y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                       treatment == 4) [['sugar']], col = tColours [['colour']] [4], pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)
text (x = 1.5, y = 1.5, cex = 2, label = 'July')
legend (x = 0, y = 1.5, legend = c ('control','compression','double compression','girdled'),
        box.lty = 0, col = tColours [['colour']] [c (1,3,4,2)], pch = 19)

# Plot scatterplot of starch in 1cm versus starch in second cm for July
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                     treatment == 1) [['starch']],
      y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                     treatment == 1) [['starch']],
      las = 1, xlab = 'starch concentration in 0-1 cm (% dry weight)', xlim = c (0, 0.8), ylim = c (0, 0.8),
      ylab = 'starch concentration in 1-2 cm (% dry weight)', col = tColours [['colour']] [1], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                       treatment == 2) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                       treatment == 2) [['starch']], col = tColours [['colour']] [2], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                       treatment == 3) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                       treatment == 3) [['starch']], col = tColours [['colour']] [3], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1 & 
                       treatment == 4) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2 & 
                       treatment == 4) [['starch']], col = tColours [['colour']] [4], pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)
text (x = 0.75, y = 0.75, cex = 2, label = 'July')
legend (x = 0, y = 0.72, legend = c ('control','compression','double compression','girdled'),
        box.lty = 0, col = tColours [['colour']] [c (1,3,4,2)], pch = 19)

# Plot scatterplot of sugar in 1cm versus sugar in second cm for November
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                     treatment == 1) [['sugar']],
      y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                     treatment == 1) [['sugar']],
      las = 1, xlab = 'soluble sugar concentration in 0-1 cm (% dry weight)', xlim = c (0, 1.6), ylim = c (0, 1.6),
      ylab = 'soluble sugar concentration in 1-2 cm (% dry weight)', col = tColours [['colour']] [1], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 2) [['sugar']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 2) [['sugar']], col = tColours [['colour']] [2], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 3) [['sugar']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 3) [['sugar']], col = tColours [['colour']] [3], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 4) [['sugar']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 4) [['sugar']], col = tColours [['colour']] [4], pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)
text (x = 1.5, y = 1.5, cex = 2, label = 'July')
legend (x = 0, y = 1.5, legend = c ('control','compression','double compression','girdled'),
        box.lty = 0, col = tColours [['colour']] [c (1,3,4,2)], pch = 19)

# Plot scatterplot of starch in 1cm versus starch in second cm for November
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                     treatment == 1) [['starch']],
      y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                     treatment == 1) [['starch']],
      las = 1, xlab = 'starch concentration in 0-1 cm (% dry weight)', xlim = c (0, 0.8), ylim = c (0, 0.8),
      ylab = 'starch concentration in 1-2 cm (% dry weight)', col = tColours [['colour']] [1], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 2) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 2) [['starch']], col = tColours [['colour']] [2], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 3) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 3) [['starch']], col = tColours [['colour']] [3], pch = 19)
points (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1 & 
                       treatment == 4) [['starch']],
        y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2 & 
                       treatment == 4) [['starch']], col = tColours [['colour']] [4], pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)
text (x = 0.75, y = 0.75, cex = 2, label = 'July')
legend (x = 0, y = 0.72, legend = c ('control','compression','double compression','girdled'),
        box.lty = 0, col = tColours [['colour']] [c (1,3,4,2)], pch = 19)
#========================================================================================
