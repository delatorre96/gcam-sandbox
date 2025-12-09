library(rgcam)
library(dplyr)
library(ggplot2)

pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/output"
my_gcamdb_basexdb <- "crop"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "C:/Users/ignacio.delatorre/Documents/GCAM/rgcam/myQueries_crop.xml"

scenariosAnalyze<-c( "Reference","crop_tomatoes")

prj1 <- addScenario(conn = conn, proj = 'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/crop/myProject.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)
print(queries)

############# Production by basin ############# 
ag_prod_byTech <- getQuery(prj1,  "ag production by tech")

basin = 'Ebro'
ag_prod_byTech_basin = ag_prod_byTech[(ag_prod_byTech$technology %in% c('Vegetables_EbroR_IRR_hi', 'Tomatoes_EbroR_IRR_hi')) & (ag_prod_byTech$output %in% c("Vegetables", "Tomatoes")), ]


veg_output_newCrop <- ag_prod_byTech_basin[(ag_prod_byTech_basin['output'] == 'Vegetables') & (ag_prod_byTech_basin['scenario'] == "crop_tomatoes"), ]
tom_output_newCrop <- ag_prod_byTech_basin[(ag_prod_byTech_basin['output'] == 'Tomatoes') & (ag_prod_byTech_basin['scenario']  == "crop_tomatoes"), ]
veg_output_ref <- ag_prod_byTech_basin[(ag_prod_byTech_basin['output'] == 'Vegetables') & (ag_prod_byTech_basin['scenario'] == "Reference"), ]

plot(veg_output_newCrop$year,veg_output_newCrop$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Production Veg and Tom in Ebro basin',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(ag_prod_byTech_basin$value),max(ag_prod_byTech_basin$value)))

lines(tom_output_newCrop$year, tom_output_newCrop$value, col = 'red', lwd = 2)
lines(veg_output_ref$year, veg_output_ref$value, col = 'yellow', lwd = 2)

legend('bottomright', legend = c('Vegetables newScen', 'Tomatoes newScen', 'Vegetables refScen'),
       col = c('blue', 'red', 'yellow'), lty = 1, lwd = 2)


############# Production by crop ############# 

ag_prodByCrop <- getQuery(prj1, "ag production by crop type")
region ='USA'

### Vegetables production before and after ### 

ag_prodByCrop_region <- ag_prodByCrop[(ag_prodByCrop['region'] == region) & 
                                        (ag_prodByCrop['output'] == 'Vegetables'),]

scen_ref <- ag_prodByCrop_region[ag_prodByCrop_region['scenario'] == 'Reference', ]
scen_tom <- ag_prodByCrop_region[ag_prodByCrop_region['scenario'] == 'crop_tomatoes', ]

plot(scen_ref$year,scen_ref$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Vegetables production under scenarios',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(ag_prodByCrop_region$value),max(ag_prodByCrop_region$value)))

lines(scen_tom$year, scen_tom$value, col = 'red', lwd = 2)
legend('bottomright', legend = c('reference', 'disaggregatedCrop'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

### Tomatoes and Vegetables in same scenario ### 

ag_prodByCrop_region <- ag_prodByCrop[
  ag_prodByCrop$region == region  &
    ag_prodByCrop$output %in% c("Vegetables", "Tomatoes"), ]

veg_output_newCrop <- ag_prodByCrop_region[(ag_prodByCrop_region['output'] == 'Vegetables') & (ag_prodByCrop_region['scenario'] == "crop_tomatoes"), ]
tom_output_newCrop <- ag_prodByCrop_region[(ag_prodByCrop_region['output'] == 'Tomatoes') & (ag_prodByCrop_region['scenario']  == "crop_tomatoes"), ]
veg_output_ref <- ag_prodByCrop_region[(ag_prodByCrop_region['output'] == 'Vegetables') & (ag_prodByCrop_region['scenario'] == "Reference"), ]

plot(veg_output$year,veg_output$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Production Veg and Tom',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(ag_prodByCrop_region$value) -10,max(ag_prodByCrop_region$value)))

lines(tom_output$year, tom_output$value, col = 'red', lwd = 2)
lines(veg_output_ref$year, veg_output_ref$value, col = 'yellow', lwd = 2)

legend('bottomright', legend = c('Vegetables newScen', 'Tomatoes newScen', 'Vegetables refScen'),
       col = c('blue', 'red', 'yellow'), lty = 1, lwd = 2)

####### Get iberian basins

ag_prod_byTech_eu <- ag_prod_byTech[ag_prod_byTech['region'] == 'EU-15',]
lista_subsector <- unique(ag_prod_byTech_eu['subsector'])$subsector

cuencas <- sapply(strsplit(lista_subsector, "_"), function(x) tail(x, 1))

cuencas_unicos <- unique(cuencas)
cuencas_ibericas <- c("DouroR", "EbroR", "GuadalqR", "GuadianaR", "TagusR", "SpainCstSE", "IberiaCst")
patron <- paste(cuencas_ibericas, collapse = "|")
#Tomatoes por cuenca
ag_prod_byTech_iberian <- ag_prod_byTech_eu[grepl(patron, ag_prod_byTech_eu$subsector) &
                                              (ag_prod_byTech_eu$output == 'Tomatoes'), ]
ag_prod_byTech_iberian <- ag_prod_byTech_iberian %>%
  mutate(cuenca = sapply(subsector, function(x) {
    match <- cuencas_ibericas[sapply(cuencas_ibericas, function(c) grepl(c, x))]
    if(length(match) == 0) return(NA) else return(match)
  }))

colores <- rainbow(length(cuencas_ibericas))  # colores distintos para cada cuenca

# Establecer límites del gráfico
ylim <- range(ag_prod_byTech_iberian_sum$total_value, na.rm = TRUE)
xlim <- range(ag_prod_byTech_iberian_sum$year)

# Crear gráfico vacío
plot(NA, xlim = xlim, ylim = ylim, xlab = "Año", ylab = "Valor total",
     main = "Producción total de tomates por cuenca hidrográfica")

# Dibujar líneas para cada cuenca
for (i in seq_along(cuencas_ibericas)) {
  c <- cuencas_ibericas[i]
  df_sub <- ag_prod_byTech_iberian_sum[ag_prod_byTech_iberian_sum$cuenca == c, ]
  lines(df_sub$year, df_sub$total_value, col = colores[i], lwd = 2)
}

# Añadir leyenda
legend("topright", legend = cuencas_ibericas, col = colores, lwd = 2, cex = 0.7)

#### Una cuenca solo

ag_prod_byTech_iberian_duero <- ag_prod_byTech_iberian[ag_prod_byTech_iberian$cuenca == 'DouroR', ]

technologies = unique(ag_prod_byTech_iberian_duero$technology)

plot(NA, xlim = xlim, ylim = c(0,0.7), xlab = "Año", ylab = "Valor total",
     main = "Producción tomates por tecnologia en el Duero")

# Dibujar líneas para cada technology
for (i in seq_along(technologies)) {
  techno <- technologies[i]
  df_sub <- ag_prod_byTech_iberian_duero[ag_prod_byTech_iberian_duero$technology == techno, ]
  lines(df_sub$year, df_sub$value, col = colores[i], lwd = 2)
}
legend("topright", legend = technologies, col = colores, lwd = 2, cex = 0.7)

############# land allocation by crop ############# 
land_allocation <- getQuery(prj1, "land allocation by crop")
region ='USA'

### Vegetables land_allocation before and after ### 

land_allocation_region <- land_allocation[(land_allocation['region'] == region) & 
                                        (land_allocation['landleaf'] == 'Vegetables'),]

scen_ref <- land_allocation_region[land_allocation_region['scenario'] == 'Reference', ]
scen_tom <- land_allocation_region[land_allocation_region['scenario'] == 'crop_tomatoes', ]

plot(scen_ref$year,scen_ref$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Vegetables production under scenarios',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(land_allocation_region$value),max(land_allocation_region$value)))

lines(scen_tom$year, scen_tom$value, col = 'red', lwd = 2)
legend('topright', legend = c('reference', 'disaggregatedCrop'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

### Tomatoes and Vegetables in same scenario ### 

land_allocation_region <- land_allocation[
  land_allocation$region == region  &
    land_allocation$landleaf %in% c("Vegetables", "Tomatoes"), ]

veg_landleaf_newCrop <- land_allocation_region[(land_allocation_region['landleaf'] == 'Vegetables') & (land_allocation_region['scenario'] == "crop_tomatoes"), ]
tom_landleaf_newCrop <- land_allocation_region[(land_allocation_region['landleaf'] == 'Tomatoes') & (land_allocation_region['scenario']  == "crop_tomatoes"), ]
veg_landleaf_ref <- land_allocation_region[(land_allocation_region['landleaf'] == 'Vegetables') & (land_allocation_region['scenario'] == "Reference"), ]

plot(veg_landleaf_newCrop$year,veg_landleaf_newCrop$value, type = 'l', col = 'blue', lwd = 2,
     main = 'landleaf Veg and Tom',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(land_allocation_region$value) -10,max(land_allocation_region$value)))

lines(tom_landleaf_newCrop$year, tom_landleaf_newCrop$value, col = 'red', lwd = 2)
lines(veg_landleaf_ref$year, veg_landleaf_ref$value, col = 'yellow', lwd = 2)

legend('bottomright', legend = c('Vegetables newScen', 'Tomatoes newScen', 'Vegetables refScen'),
       col = c('blue', 'red', 'yellow'), lty = 1, lwd = 2)



