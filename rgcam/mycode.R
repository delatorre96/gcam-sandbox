library(rgcam)


pathToDbs <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/gcam-core/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

scenariosAnalyze<-c("newElectricCar, Reference")


myQueryfile  <- "C:/Users/ignacio.delatorre/Documents/Understanding GCAM/rgcam/myQueries_transports.xml"



prj1 <- addScenario(conn = conn, proj = 'myProject.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)




scenarios <- listScenarios(prj1)


queries2 <- listQueries(prj1, 'Reference')

transEnergy_byTech <- getQuery(prj1, "transport final energy by tech and vintage")
transOutput_byTech <- getQuery(prj1, "transport service output by tech" )
fuel_prices <- getQuery(prj1, "fuel prices to transport")

data <- get_sdg15_land_indicator(prj1)


