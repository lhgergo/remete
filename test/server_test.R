# In this file I aim to test the server side of the package
load("test/ifobj_gdrive.RData")
source("R/functions.R")
RunServer(interface = ifobj_gdrive)


RunServerAsync(interface = ifobj_gdrive)
