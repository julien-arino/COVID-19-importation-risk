### SET_DIRECTORIES

# Create list to store the information
DIRS = list()
# Set code directory
DIRS$CODE = here::here()
# Directories outside Github repo: DATA, others perhaps
if (Sys.info()["sysname"] == "Windows") {
  # Assuming your data is on D. Further tailor to your situation using the
  # output of Sys.info()["nodename"], if need be
  DIRS$OUTSIDE = "D:/Documents"
} else if (Sys.info()["nodename"] %in% c("jarino-compute1",
                                         "jarino-compute2",
                                         "jarino-compute3",
                                         "jarino-compute4",
                                         "raspberry")) {
  # Example of a nonstandard unix config: my home compute cluster's shared NAS drive
  DIRS$OUTSIDE = "/mnt/NFS_LaCie"
} else {
  # On unix, typically this would work. Adapt to your setting
  DIRS$OUTSIDE = sprintf("/home/%s", Sys.info()["user"])
}
# A few directories to store stuff
DIRS$OUTPUT = sprintf("%s/OUTPUT", DIRS$OUTSIDE)
DIRS$FIGS = sprintf("%s/FIGS", DIRS$OUTSIDE)
DIRS$COVID_DATA = sprintf("%s/DATA", DIRS$OUTSIDE)

