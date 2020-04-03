#========================================================================================
# Script with variables and functions relevant for ploting figuresin R
#----------------------------------------------------------------------------------------

# set colours for treatments: control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
tColours <- tibble (colour = c ('#91b9a4','#C0334D','#F18904','#5C4A72','#23345C'),
                    treatment = c ('control','girdled','compressed','double compressed',
                                   'chilled'))

# set colours for carbon sinks: growth, respiration, change in NSC concentrations. 
#----------------------------------------------------------------------------------------
sColours <- tibble (colour   = c ('#8073ac','#e08214','#5aae61'), 
                    variable = c ('NSC'    ,'resp'   ,'SC'))

# function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}
#========================================================================================