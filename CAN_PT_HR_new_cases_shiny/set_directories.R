### SET_DIRECTORIES

# Need a special ad hoc version here because we are running in Shiny server
# Create list to store the information
DIRS = list()
# Set code directory
DIRS$CODE = "/home/ubuntu/github/covid-19-importation-risk"
# Directories outside Github repo: DATA, others perhaps
DIRS$OUTSIDE = "/home/ubuntu"
# A few directories to store stuff
DIRS$OUTPUT = sprintf("%s/OUTPUT", DIRS$OUTSIDE)
DIRS$FIGS = sprintf("%s/FIGS", DIRS$OUTSIDE)
DIRS$COVID_DATA = sprintf("%s/DATA", DIRS$OUTSIDE)

