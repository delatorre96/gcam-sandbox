library(rgcam)


pathToDbs <- "C:/Users/ignacio.delatorre/Documents/GCAM/gcam-core/output"
my_gcamdb_basexdb <- "database_basexdb"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "C:/Users/ignacio.delatorre/Documents/GCAM/rgcam/myQueries_transports.xml"

scenariosAnalyze<-c( "Reference","newElectricCar",
                     "new_electricCar_diffCosts_try2",
                     "new_electricCar_diffCosts_try3",
                     "new_electricCar_diffCosts_try4")
#Reference: Primer escenario sin cambios
#newElectricCar: inclusión de coche eléctrico sin costos adicionales (idéntico al anterior)
#new_electricCar_diffCosts_try2: sólo cambio en shareWeights (MORDOR 0.7 y BEV 0.3)
#new_electricCar_diffCosts_try3: cambio en shareWeights y costes (MORDOR capital costs 20% más barato que BEV)
#new_electricCar_diffCosts_try4: Cambio en costes y shareweights y shareweights convergen en momento diferentes

prj1 <- addScenario(conn = conn, proj = 'C:/Users/ignacio.delatorre/Documents/GCAM/desarrollos/cocheElectrico/myProject3.dat', scenario  = scenariosAnalyze, queryFile = myQueryfile)




#scenarios <- listScenarios(prj1)


queries2 <- listQueries(prj1)


transOutput_byTech_new <- getQuery(prj1, "transport final energy by tech (new)")
transOutput_byTech <- getQuery(prj1, "transport service output by tech" )
costTransportTech <- getQuery(prj1, "costs of transport techs")
transport_ShWs<- getQuery(prj1, "transport tech share-weights" )

################################1. transOutput_byTech################################

##########1.1 BEV############
region = 'USA'
transOutput_byTech_BEV <- transOutput_byTech[(transOutput_byTech['technology'] == 'BEV') & 
                                               (transOutput_byTech['region'] == region) & 
                                               (transOutput_byTech['sector'] == 'trn_pass_road_LDV_4W') &
                                               (transOutput_byTech['subsector'] == 'Car'), ]
transOutput_byTech_MORDOR <- transOutput_byTech[(transOutput_byTech['technology'] == 'MORDOR') &
                                                  (transOutput_byTech['region'] == region) & 
                                                  (transOutput_byTech['sector'] == 'trn_pass_road_LDV_4W') &
                                                  (transOutput_byTech['subsector'] == 'Car'), ] 

BEV_0 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'Reference',]
BEV_1 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'newElectricCar',]
BEV_2 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'new_electricCar_diffCosts_try2',]
BEV_3 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'new_electricCar_diffCosts_try3',]
BEV_4 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'new_electricCar_diffCosts_try4',]
plot(BEV_0$year,BEV_0$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Transport service output BEV',
     xlab = 'Años',
     ylab = 'Output',ylim = c(min(c(BEV_0$value,BEV_1$value,BEV_2$value,BEV_3$value)),
                              max(c(BEV_0$value,BEV_1$value,BEV_2$value,BEV_3$value))))


lines(BEV_1$year,BEV_1$value, type = 'l', col = 'red', lwd = 2)
lines(BEV_2$year,BEV_2$value, type = 'l', col = 'yellow', lwd = 2)
lines(BEV_3$year,BEV_3$value, type = 'l', col = 'purple', lwd = 2)
lines(BEV_4$year,BEV_4$value, type = 'l', col = 'orange', lwd = 2)
legend('topright', legend = c('ref', 'newSametech', 'diff_shw','cambio_costos','hist_shw'),
       col = c('blue', 'red','yellow','purple','orange'), lty = 1, lwd = 2)


##########1.2. MORDOR############

#ref_values <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'Reference',]
MORDOR_1 <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'newElectricCar',]
MORDOR_2 <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'new_electricCar_diffCosts_try2',]
MORDOR_3 <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'new_electricCar_diffCosts_try3',]
MORDOR_4 <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'new_electricCar_diffCosts_try4',]
plot(MORDOR_1$year,MORDOR_1$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Transport service output MORDOR',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(c(MORDOR_1$value,MORDOR_2$value,MORDOR_3$value,MORDOR_4$value)),
              max(c(MORDOR_1$value,MORDOR_2$value,MORDOR_3$value,MORDOR_4$value))))


lines(MORDOR_2$year,MORDOR_2$value, type = 'l', col = 'red', lwd = 2)
lines(MORDOR_3$year,MORDOR_3$value, type = 'l', col = 'yellow', lwd = 2)
lines(MORDOR_4$year,MORDOR_4$value, type = 'l', col = 'purple', lwd = 2)
legend('topright', legend = c('newSametech', 'diff_shw','cambio_costos','hist_shw'),
       col = c('blue', 'red','yellow','purple'), lty = 1, lwd = 2)

##########1.3. Suma BEV - MORDOR############
###1.3.1#### LA suma en el primer escenario debería ser mayor que en el escenario de referencia
MORDOR_1  <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'newElectricCar',]
BEV_0 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'Reference',]
BEV_1 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'newElectricCar',]
suma <- MORDOR_1$value + BEV_1$value
plot(MORDOR_1$year,MORDOR_1$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Transport service output newSametech',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(c(MORDOR_1$value,BEV_0$value,BEV_1$value,suma)),
              max(c(MORDOR_1$value,BEV_0$value,BEV_1$value,suma))))


lines(BEV_0$year,BEV_0$value, type = 'l', col = 'red', lwd = 2)
lines(BEV_1$year,suma, type = 'l', col = 'yellow', lwd = 2)
legend('topright', legend = c('MORDOR/BEV_newSametech', 'BEv_ref','Suma'),
       col = c('blue', 'red','yellow'), lty = 1, lwd = 2)

###1.3.2#### LA suma en el escenario de diferentes shaeWeights debería ser igual que en el escenario de referencia

MORDOR_2 <- transOutput_byTech_MORDOR[transOutput_byTech_MORDOR['scenario'] == 'new_electricCar_diffCosts_try2',]
BEV_0 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'Reference',]
BEV_2 <- transOutput_byTech_BEV[transOutput_byTech_BEV['scenario'] == 'new_electricCar_diffCosts_try2',]
suma <- MORDOR_2$value + BEV_2$value
plot(MORDOR_2$year,MORDOR_1$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Transport service output diff_shw',
     xlab = 'Años',
     ylab = 'Output',
     ylim = c(min(c(MORDOR_2$value,BEV_0$value,BEV_2$value,suma)),
              max(c(MORDOR_2$value,BEV_0$value,BEV_2$value,suma))))

lines(BEV_2$year,BEV_2$value, type = 'l', col = 'red', lwd = 2)
lines(BEV_0$year,BEV_0$value, type = 'l', col = 'yellow', lwd = 2)

lines(BEV_2$year,suma, type = 'l', col = 'orange', lwd = 2)
legend('topright', legend = c('MORDOR_diff_shw','BEV_diff_shw', 'BEv_ref','Suma'),
       col = c('blue', 'red','yellow','orange'), lty = 1, lwd = 2)

################################2. costs of transport techs################################


costTransportTech_ <- costTransportTech[(costTransportTech['region'] == region) &  
                                          (costTransportTech['technology'] == c('BEV', 'MORDOR'))&  
                                          (costTransportTech['sector'] == 'trn_pass_road_LDV_4W') &
                                          (costTransportTech['subsector'] == 'Car'), ]
####BEV costs among scenarios####
BEV_reference <- costTransportTech_[(costTransportTech_['scenario'] == 'Reference') & (costTransportTech_['technology'] == 'BEV'),]
BEV_sameCosts <- costTransportTech_[(costTransportTech_['scenario'] == 'newElectricCar')& (costTransportTech_['technology'] == 'BEV'),]
BEV_diffCosts <- costTransportTech_[(costTransportTech_['scenario'] == 'new_electricCar_diffCosts_try2')& (costTransportTech_['technology'] == 'BEV'),]
BEV_diffCosts2 <- costTransportTech_[(costTransportTech_['scenario'] == 'new_electricCar_diffCosts_try3') & (costTransportTech_['technology'] == 'BEV'),]


plot(BEV_reference$year,BEV_reference$value, type = 'l', col = 'blue', lwd = 2,
     main = 'costs of transport BEV',
     xlab = 'Years',
     ylab = 'Costs')

lines(BEV_sameCosts$year,BEV_sameCosts$value,type = 'l', col = 'red', lwd = 2)

lines(BEV_diffCosts2$year,BEV_diffCosts$value,type = 'l', col = 'yellow', lwd = 2)


legend('topright', legend = c('Reference', 'newElectricCar','new_electricCar_diffCosts'),
       col = c('blue', 'red','yellow'), lty = 1, lwd = 2)

####MORDOR costs among scenarios####
MORDOR_sameCosts <- costTransportTech_[(costTransportTech_['scenario'] == 'newElectricCar')& (costTransportTech_['technology'] == 'MORDOR'),]
MORDOR_diffCosts <- costTransportTech_[(costTransportTech_['scenario'] == 'new_electricCar_diffCosts_try3')& (costTransportTech_['technology'] == 'MORDOR'),]

plot(MORDOR_sameCosts$year,MORDOR_sameCosts$value, type = 'l', col = 'blue', lwd = 2,
     main = 'costs of transport MORDOR',
     xlab = 'Years',
     ylab = 'Costs',ylim = c(0.1,0.2))

lines(MORDOR_diffCosts$year,MORDOR_diffCosts$value,type = 'l', col = 'red', lwd = 2)

legend('topright', legend = c('newElectricCar','new_electricCar_diffCosts'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

####MORDOR and BEV costs among scenarios####
##Same Costs##

plot(MORDOR_sameCosts$year,MORDOR_sameCosts$value, type = 'l', col = 'blue', lwd = 2,
     main = 'costs of transport BEV vs MORDOR same costs',
     xlab = 'Years',
     ylab = 'Costs')

lines(BEV_sameCosts$year,BEV_sameCosts$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

##Diff Costs##
plot(MORDOR_diffCosts$year,MORDOR_diffCosts$value, type = 'l', col = 'blue', lwd = 2,
     main = 'costs of transport BEV vs MORDOR different costs',
     xlab = 'Years',
     ylab = 'Costs', 
     ylim = c(min(MORDOR_diffCosts$value),max(BEV_diffCosts$value)))

lines(BEV_diffCosts$year,BEV_diffCosts$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

################################3. transport_ShWs################################
transport_ShWs_ <- transport_ShWs[(transport_ShWs['region'] == region) &   
                                    (transport_ShWs['sector'] == 'trn_pass_road_LDV_4W') &
                                    (transport_ShWs['subsector'] == 'Car'), ]

transport_ShWs_scenNewTewchno <- transport_ShWs_[transport_ShWs_['scenario'] == 'newElectricCar',]
transport_ShWs_scenDiffParams2 <- transport_ShWs_[transport_ShWs_['scenario'] == 'new_electricCar_diffCosts_try2',]
transport_ShWs_scenDiffParams3 <- transport_ShWs_[transport_ShWs_['scenario'] == 'new_electricCar_diffCosts_try3',]
transport_ShWs_scenDiffParams4 <- transport_ShWs_[transport_ShWs_['scenario'] == 'new_electricCar_diffCosts_try4',]

####Escenario 1#####
BEV_1 <- transport_ShWs_scenNewTewchno[transport_ShWs_scenNewTewchno['technology'] == 'BEV',]
MORDOR_1 <- transport_ShWs_scenNewTewchno[transport_ShWs_scenNewTewchno['technology'] == 'MORDOR',]

plot(BEV_1$year,BEV_1$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Scen1',
     xlab = 'Years',
     ylab = 'ShareWheit', 
     ylim = c(0,1))
lines(MORDOR_1$year,MORDOR_1$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

####Escenario 2#####
BEV_2 <- transport_ShWs_scenDiffParams2[transport_ShWs_scenDiffParams2['technology'] == 'BEV',]
MORDOR_2 <- transport_ShWs_scenDiffParams2[transport_ShWs_scenDiffParams2['technology'] == 'MORDOR',]

plot(BEV_2$year,BEV_2$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Scen2',
     xlab = 'Years',
     ylab = 'ShareWheit', 
     ylim = c(0,1))
lines(MORDOR_2$year,MORDOR_2$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

####Escenario 3#####
BEV_3 <- transport_ShWs_scenDiffParams3[transport_ShWs_scenDiffParams3['technology'] == 'BEV',]
MORDOR_3 <- transport_ShWs_scenDiffParams3[transport_ShWs_scenDiffParams3['technology'] == 'MORDOR',]

plot(BEV_3$year,BEV_3$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Scen 3',
     xlab = 'Years',
     ylab = 'ShareWheit', 
     ylim = c(0,1))
lines(MORDOR_3$year,MORDOR_3$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)

####Escenario 4#####
BEV_4 <- transport_ShWs_scenDiffParams4[transport_ShWs_scenDiffParams4['technology'] == 'BEV',]
MORDOR_4 <- transport_ShWs_scenDiffParams4[transport_ShWs_scenDiffParams4['technology'] == 'MORDOR',]

plot(BEV_4$year,BEV_4$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Scen 4',
     xlab = 'Years',
     ylab = 'ShareWheit', 
     ylim = c(0,1))
lines(MORDOR_4$year,MORDOR_4$value,type = 'l', col = 'red', lwd = 2)


legend('topright', legend = c('MORDOR','BEV'),
       col = c('blue', 'red'), lty = 1, lwd = 2)



######################################################

par(mfrow = c(2,2), mar = c(4,4,3,1), oma = c(4,0,2,0))  # Más espacio abajo para la leyenda, arriba para el título

# Título principal arriba, centrado sobre las 4 gráficas
#mtext("Comparación de Escenarios - ShareWheit por tecnología", side = 3, outer = TRUE, line = 1.5, cex = 1.5, font = 2)

# Escenario 1
BEV_1 <- transport_ShWs_scenNewTewchno[transport_ShWs_scenNewTewchno$technology == 'BEV',]
MORDOR_1 <- transport_ShWs_scenNewTewchno[transport_ShWs_scenNewTewchno$technology == 'MORDOR',]
plot(BEV_1$year, BEV_1$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Idénticos',
     xlab = 'Years', ylab = 'ShareWeight', ylim = c(0,1))
lines(MORDOR_1$year, MORDOR_1$value, col = 'red', lwd = 2)
# No legend aquí

# Escenario 2
BEV_2 <- transport_ShWs_scenDiffParams2[transport_ShWs_scenDiffParams2$technology == 'BEV',]
MORDOR_2 <- transport_ShWs_scenDiffParams2[transport_ShWs_scenDiffParams2$technology == 'MORDOR',]
plot(BEV_2$year, BEV_2$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Diferentes shw',
     xlab = 'Years', ylab = 'ShareWeight', ylim = c(0,1))
lines(MORDOR_2$year, MORDOR_2$value, col = 'red', lwd = 2)

# Escenario 3
BEV_3 <- transport_ShWs_scenDiffParams3[transport_ShWs_scenDiffParams3$technology == 'BEV',]
MORDOR_3 <- transport_ShWs_scenDiffParams3[transport_ShWs_scenDiffParams3$technology == 'MORDOR',]
plot(BEV_3$year, BEV_3$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Diferentes shw y costes',
     xlab = 'Years', ylab = 'ShareWeight', ylim = c(0,1))
lines(MORDOR_3$year, MORDOR_3$value, col = 'red', lwd = 2)

# Escenario 4
BEV_4 <- transport_ShWs_scenDiffParams4[transport_ShWs_scenDiffParams4$technology == 'BEV',]
MORDOR_4 <- transport_ShWs_scenDiffParams4[transport_ShWs_scenDiffParams4$technology == 'MORDOR',]
plot(BEV_4$year, BEV_4$value, type = 'l', col = 'blue', lwd = 2,
     main = 'Diferente convergencia de shw',
     xlab = 'Years', ylab = 'ShareWeight', ylim = c(0,1))
lines(MORDOR_4$year, MORDOR_4$value, col = 'red', lwd = 2)

# Leyenda común abajo centrada
par(xpd = NA)  # Permite dibujar fuera del área de los plots
legend_x <- mean(par("usr")[1:2])  # Centro horizontal
legend_y <- par("usr")[3] - 0.15 * diff(par("usr")[3:4])  # Justo debajo del margen inferior

legend(legend_x, legend_y,
       legend = c('BEV', 'MORDOR'),
       col = c('blue', 'red'),
       lty = 1, lwd = 2,
       horiz = TRUE, bty = 'n', cex = 1,
       xjust =1.5,yjust = 2)
