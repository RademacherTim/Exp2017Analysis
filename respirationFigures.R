#========================================================================================
# Script to make figures with regard to stem CO2 efflux in the 2017 experiment at 
# Harvard Forest.
#----------------------------------------------------------------------------------------

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
colours <- c ('#91b9a4','#C0334D','#F18904','#5C4A72','#23345C')
colourPine  <- '#66c2a5'
colourMaple <- '#fc8d62'
colourOak   <- '#8da0cb'

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

ALPHA  <- 0.3 # add transparency to better see all symbols

# Read processed respiration data
#----------------------------------------------------------------------------------------
source ('/home/tim/projects/PlantGrowth/stemCO2Efflux/readProcessedRespData.R')

# Plot the respiration rate of control tree white pines versus temperature
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

#========================================================================================