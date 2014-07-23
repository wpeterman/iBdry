###############################################################
# R code to execute optimization of iButton
# analysis parameters
#                                                             
# Written by Bill Peterman                                    
# November 2013                                                     
#
##############################################################
# Note: This code will not make folders/directories for you. Please make sure all
# specified folders already exist prior to running the code

# Where is the "iButton_Optimization_SourceCode.R" located?
iButton.optimize <- "C:/iButton/iButton_Optimization_SourceCode.R"

# Where are your raw iButton data files located? All files should have a ".csv" extension and placed in a
# folder together with no other ".csv" files
FOLDER<- "C:/iButton/iButtonFiles/"

# Where is your validation data file located? This file must be a comma-separated file with three columns:
# (1) Site name
# (2) Date of observation (as mm/dd/yyyy)
# (3) Status, either "Wet" or "Dry" (case sensitive)
# Note that the site names listed need to match identically to the file name given for each iButton data file 
#    e.g. iButton file "Pond123.csv" would need a record of "Pond123" in the VALIDATION file.
VALIDATION <- "C:/iButton/Validation.csv"
 
# Where do you want to export results table following optimization?
OUT.DIR <- "C:/iButton/results/"

# What was the first full day that iButtons were deployed? (Enter as "mm/dd/yy")
Day.Out <- "02/01/13"

# What was the last full day that iButtons were deployed? (Enter as "mm/dd/yy")
Day.In <- "07/20/13"

# Define a range of values for each parameter below (change the numbers following "from=" and "to=")
# The variance level to set the threshold
variance <- seq(from=10, to=11, by=1)

# Over what range of sizes do you want to asses the moving window? (Must be greater than 1)
window <- seq(from=12, to=12, by=1)

# Over what range of continuous days do you want to assess dry-fill events over?
cd <- seq(from=2, to=2,by=1)

# Run the line below to determine how many total combinations are being assessed on each file
length(variance)*length(window)*length(cd)*2 

# After modifying and running the lines of code above, run the line of code below
source(iButton.optimize)
