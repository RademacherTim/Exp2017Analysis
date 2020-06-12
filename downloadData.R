# Get list of folder on shared PlantGrowth google drive
GDFolders  <- drive_ls (path = 'https://drive.google.com/drive/u/1/folders/0ABA0-TtlzSLPUk9PVA')

# Get id for data folder on shared PlantGrowth google drive
GDDataID <- GDFolders [['id']] [GDFolders [['name']] == 'data']

# Get list of folders in data folder
GDFoldersData <- drive_ls (path = as_id (GDDataID))

# Get microcores folder id
GDmicrocoresID <- GDFoldersData [['id']] [GDFoldersData [['name']] == 'microcores']

# Get list of folders in micrcores folder
GDFoldersMicrocores <- drive_ls (path = as_id (GDmicrocoresID))

# Get woodAnatomy folder id
GDwoodAnatomyID <- GDFoldersMicrocores [['id']] [GDFoldersMicrocores [['name']] == 'woodAnatomy']

# Get list of folders in woodAnatomy folder
GDFoldersWoodAnatomy <- drive_ls (path = as_id (GDwoodAnatomyID))

# Get Exp2017 folder id
GDExp2017ID <- GDFoldersWoodAnatomy [['id']] [GDFoldersWoodAnatomy [['name']] == 'Exp2017']

# Get list of files in Exp2017 folder
GDFilesExp2017 <- drive_ls (path = as_id (GDExp2017ID))

# Get id of file with data for 20 mu band anatomical averages
GDExp2017FileID <- GDFilesExp2017 [['id']] [GDFilesExp2017 [['name']] == '20muband_ALL.xlsx']

# Get id of folder with ring width measurements from TRIAD
GDFolderringWidthTRIADID <- GDFilesExp2017 [['id']] [GDFilesExp2017 [['name']] == 'ringWidthTRIAD']

# Get list of ids for all ringwidth measurements
GDringWidthFilesIDs <- drive_ls (path = as_id (GDFolderringWidthTRIADID))

# Identify allometry folder
GDallometryID <- GDFoldersData [['id']] [GDFoldersData [['name']] == 'allometry']

# List files in allometry folder
GDFilesAllometry <- drive_ls (path = as_id (GDallometryID))

# Identify id of file with allometry data for the 2017 Experiment
GDExp2017AllometryFileID <- GDFilesAllometry [['id']] [GDFilesAllometry [['name']] == 'allometricDataExp2017.xlsx']

# Change working directory
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2017/ringWidthTRIAD/')

# Download the files for ring width measurements of the Exp 2017 microcores
# for (i in 1:dim (GDringWidthFilesIDs) [1]) {
#   fileExp2017 <- drive_download (file = as_id (GDringWidthFilesIDs [['id']] [i]), verbose = FALSE, overwrite = TRUE)
#   rm (fileExp2017)
#   print (i)
# }
