# Run this code to create the .secrets in the 
# app folder needed to connect to google files 
# through the `googledrive` and `googlesheets4` package

# designate working directory (change for directory)
setwd("~/Dropbox/ShinyProjects/teachvatory/")

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
googledrive::drive_auth()
googlesheets4::gs4_auth() 

# see your token file in the cache, if you like
list.files(".secrets/")