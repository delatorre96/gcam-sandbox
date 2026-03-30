library(rgcam)

pathToDbs <- "C:/GCAM/Nacho/gcam_8.2/output"
my_gcamdb_basexdb <- "coche_elect"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "queries.xml"

scenariosAnalyze<- listScenariosInDB(conn)$name

prj1 <- addScenario(conn = conn, proj = 'electCar.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)


queries2 <- listQueries(prj1)


aa <- getQuery(prj1,"resource production"  )
