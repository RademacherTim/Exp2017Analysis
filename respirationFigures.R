#========================================================================================
# Script to make figures with regard to stem CO2 efflux in the 2017 experiment at 
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

# plot stem CO2 efflux for each tree over time
#----------------------------------------------------------------------------------------

#========================================================================================