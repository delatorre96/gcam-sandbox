library(rgcam)

pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "allQueries.xml"

scenariosAnalyze<-c( "Reference","ChangeBaseYear")

prj1 <- addScenario(conn = conn, proj = 'ChangeBaseYear.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)