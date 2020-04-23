
# Previous respiration plots
#----------------------------------------------------------------------------------------
plot (respData [['airt.C']] [respData [['treatment']] == 1 & 
                               respData [['species']] == 'Pinus strobus'],
      respData [['flux']] [respData [['treatment']] == 1 & 
                             respData [['species']] == 'Pinus strobus'],
      ylab = 'stem CO2 efflux', xlab = 'temperature (degC)', xlim = c (0, 30), 
      ylim = c (0, 26), las = 1, pch = 19, col = addOpacity (colourPine, ALPHA))
points (respData [['airt.C']] [respData [['treatment']] == 1 & 
                                 respData [['species']] == 'Pinus strobus' &
                                 respData [['study']]  == 'Exp2017'],
        respData [['flux']] [respData [['treatment']] == 1 & 
                               respData [['species']] == 'Pinus strobus' &
                               respData [['study']] == 'Exp2017'],
        pch = 19, col = addOpacity ('#106470', ALPHA))

# Fit a temperature response curve to all control group white pines
#----------------------------------------------------------------------------------------


# Fit a temperature response curve to Exp2017 control group white pines
#----------------------------------------------------------------------------------------


# Plot stem CO2 efflux for each Exp2017 tree over time
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))

plot (respData [['timestamp']] [respData [['tree']] == 1 & respData [['chamber']] == 1 & 
                                  respData [['study']] == 'Exp2017'],
      respData [['flux']] [respData [['tree']] == 1 & respData [['chamber']] == 1 & 
                             respData [['study']] == 'Exp2017'], col = 'white',
      typ = 'l', xlab = 'date', ylab = 'respiration', las = 1, ylim = c (0, 8.7))
sapply (controlTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017']))
sapply (girdledTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (compresTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))
sapply (compresTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (girdledTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 3))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 3 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 3 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))


plot (respData [['timestamp']] [respData [['tree']] == 1 & respData [['chamber']] == 1 & 
                                  respData [['study']] == 'Exp2017'],
      respData [['flux']] [respData [['tree']] == 1 & respData [['chamber']] == 1 & 
                             respData [['study']] == 'Exp2017'], col = 'white',
      typ = 'l', xlab = 'date', ylab = 'respiration', las = 1, ylim = c (0, 8.7),
      xlim = c (as.POSIXct ('2017-06-15'), as.POSIXct ('2017-11-15')))
sapply (controlTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017']))
sapply (girdledTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (girdledTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))
sapply (compresTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (compresTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 1 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 2))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 2 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 3))
sapply (douCompTrees, function (iTree) lines (respData [['timestamp']] [respData [['tree']] == iTree & respData [['chamber']] == 3 & 
                                                                          respData [['study']] == 'Exp2017'],
                                              respData [['flux']] [respData [['tree']] == iTree & respData [['chamber']] == 3 & 
                                                                     respData [['study']] == 'Exp2017'],
                                              lty = 1))
