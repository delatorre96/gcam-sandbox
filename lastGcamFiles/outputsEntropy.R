library(rgcam)
source('entropyCalculation.R')

pathToDbs <- "../../gcam_8.2/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "allQueries.xml"

scenariosAnalyze<-c( "Reference")
start <- Sys.time()
prj1 <- addScenario(conn = conn, proj = 'allOutputs.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)

end <- Sys.time()
duration <- end - start
queries <- listQueries(prj1)


entropies_outputs <- list()
df_procesados <- 0
cat(df_procesados, "\r")
for (query in queries){
    df <- getQuery(prj1,  query)
    H_df <- get_dataFrame_Sum_entropy(df)
    entropies_outputs[[query]] <- H_df
    df_procesados <- df_procesados + 1
    cat(df_procesados, "dataframes procesados...",'Procesando ', query,"\r")
    flush.console()
  }


df_entropies_output <- t(data.frame(entropies_outputs))
colnames(df_entropies_output) <- "entropia"



