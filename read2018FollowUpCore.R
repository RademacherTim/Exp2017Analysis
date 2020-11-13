

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
#library ('lubridate')
#library ('readxl')
library ('rjson')

# Set working directory to read json files for the follow-up microcores
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2017/ringWidthTRIAD/2018/')

# List all json output file from TRIAD
#----------------------------------------------------------------------------------------
jsonFiles <- list.files (path = './', pattern = '.json')

# Create tibble with ring measurements from the 2018 micrcore 
#----------------------------------------------------------------------------------------
dataFollowUp <- tibble (treeId = numeric (), treatment = numeric (), 
                        sampleHeight = numeric (), Y2018 = numeric (), 
                        Y2017 = numeric (), Y2016 = numeric (), Y2015 = numeric (), 
                        Y2014 = numeric (), Y2013 = numeric (), Y2012 = numeric (), 
                        Y2011 = numeric (), Y2010 = numeric ())
  
# Loop over json files and read them
#----------------------------------------------------------------------------------------
for (j in 1: length (jsonFiles)) {
  
  # Read in TRIAD outputs
  #--------------------------------------------------------------------------------------
  temp <- fromJSON (file = jsonFiles [j]) 
  treeID <- as.numeric (substr (temp [['sampleID']], 1, 2))
  sampleHeight <- temp [['sampleHeight']]
  sampleDate <- lubridate::month (temp [['sampleDate']])
  len <- length (temp [['markerData']])
  t <- as.numeric (temp [['plotID']]) # treatment
  #print (c (len, treeID, sampleHeight))
  
  # Check that sample Height were entered correctly
  #--------------------------------------------------------------------------------------
  sampleH2 <- substr (temp [['sampleID']], 4, 4)
  if ((sampleH2 == 'A' & sampleHeight %in% c (0.5, 1.0, 1.5)) |
      (sampleH2 == 'M' & sampleHeight %in% c (0.5, 1.0, 2.0, 2.5)) | 
      (sampleH2 == 'B' & sampleHeight %in% c (1.5, 2.0, 2.5))) {
        stop (paste0 ('Error with sample height',treeID,sampleH2)) 
  }
  
  # Extract growth measurement, associated years and types of markers
  #--------------------------------------------------------------------------------------
  for (i in 2:len) {
    if (i == 2) {
      types  <- unlist (temp [['markerData']] [2]) [['type']] [[1]]
      years  <- unlist (temp [['markerData']] [2]) [['year']] [[1]]
      growth <- ifelse (types == 'Missing', 0, 
                        unlist (temp [['markerData']] [2]) [['growth']] [[1]])
    } else {
      types  <- c (types,  unlist (temp [['markerData']] [i]) [['type']] [[1]])
      years  <- c (years,  unlist (temp [['markerData']] [i]) [['year']] [[1]])
      growth <- c (growth, ifelse (types [i-1] %in% c ('Linker', 'Density fluctuation'), NA, 
                                   unlist (temp [['markerData']] [i]) [['growth']] [[1]]))
    }
  }
  #print (years)
  
  # Wrangle data
  #--------------------------------------------------------------------------------------
  growth <- as.numeric (growth [types %in% c ('Normal','Missing') & 
                                  years <= 2018 & !is.na (years)]) 
  years  <- as.numeric (years  [types %in% c ('Normal','Missing') & 
                                  years <= 2018 & !is.na (years)])
  growth <- c (growth, rep (NA, 9-length (growth)))
  years  <- c (years,  seq (years [length (years)]-1, 2010))

  # add to tibble with all growth for all years years
  #--------------------------------------------------------------------------------------
  dataFollowUp <- dataFollowUp %>% add_row (treeId = treeID,
                                            treatment = t,
                                            sampleHeight = sampleHeight,
                                            Y2018 = growth [years == 2018],
                                            Y2017 = growth [years == 2017],
                                            Y2016 = growth [years == 2016],
                                            Y2015 = growth [years == 2015],
                                            Y2014 = growth [years == 2014],
                                            Y2013 = growth [years == 2013],
                                            Y2012 = growth [years == 2012],
                                            Y2011 = growth [years == 2011],
                                            Y2010 = growth [years == 2010])
  
}  # end json file loop

# Remove tree 41 and 01A which are not part of this paper
#----------------------------------------------------------------------------------------
dataFollowUp <- dataFollowUp %>% filter (!(treatment == 1 & sampleHeight == 2)) %>%
  filter (treeId != 41)

# Standardise ring width using the 2015 ring
#----------------------------------------------------------------------------------------
dataFollowUp <- dataFollowUp %>% mutate (RWI2018 = Y2018 / Y2015,
                                         RWI2017 = Y2017 / Y2015)

# Summarise growth
#----------------------------------------------------------------------------------------
dataFollowUp %>% filter (treeId != 28) %>% group_by (treatment, sampleHeight) %>% 
  summarise (meanY18 = mean (Y2018, na.rm = TRUE),
             seY18   = se   (Y2018),
             meanY17 = mean (Y2017, na.rm = TRUE),
             seY17   = se   (Y2017),
             meanY16 = mean (Y2016, na.rm = TRUE),
             seY16   = se   (Y2016),
             meanY15 = mean (Y2015, na.rm = TRUE),
             seY15   = se   (Y2015),
             meanRWI2018 = mean (RWI2018, na.rm = TRUE),
             seRWI2018   = se (RWI2018),
             meanRWI2017 = mean (RWI2017, na.rm = TRUE),
             seRWI2017   = se (RWI2017))

# Pivot data into long format
#----------------------------------------------------------------------------------------
data2018 <- dataFollowUp %>% pivot_longer ()

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len)
