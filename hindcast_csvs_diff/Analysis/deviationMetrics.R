library(rgcam)
library(dplyr)

pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "C:/Users/ignacio.delatorre/Documents/GCAM/rgcam/myQueries_crop.xml"

scenariosAnalyze<-c( "Reference","crop_tomatoes")

prj1 <- addScenario(conn = conn, proj = 'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/crop/myProject.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)
print(queries)