
rm(list = ls())
gc()

start.modules <- c("~/dm_finanzas/loader.recuperatorio.r", # linux location
                   "C:/Users/Luxor/Documents/GitHub/Test_01/dm_finanzas/loader.recuperatorio.r" # windows location
)


# Load existing modules only
load.modules <- function( modulesPath )
{
    for( modulePath in modulesPath)
        if (file.exists(modulePath))
            source(modulePath)
}

## load helper libs ##
load.modules(start.modules)

### Type test code in here.

df <- get.train.df()
dataset <- get.train.df()

# 
# df %>%
#     mutate( mv_cuenta_estado2=mapply( pmax, Master_cuenta_estado,  Visa_cuenta_estado, na.rm=TRUE ) ) %>%
#     mutate( mv_marca_atraso=mapply( pmax, Master_marca_atraso,  Visa_marca_atraso, na.rm=TRUE ) ) %>%
# 
#     mutate( mv_mfinanciacion_limite=mapply( sum, Master_mfinanciacion_limite,  Visa_mfinanciacion_limite, na.rm=TRUE ) )# %>%


dataset[ , mv_cuenta_estado2       := pmax( Master_cuenta_estado,  Visa_cuenta_estado, na.rm = TRUE) ]
dataset[ , mv_marca_atraso         := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]

dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_tconsumos            := rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ) ]
dataset[ , mv_tadelantosefectivo   := rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]


mega.df <- data.table(dataset)


mega.df[ , fe_antiguedad_edad_ratio :=  cliente_antiguedad / 12 / cliente_edad ]
mega.df[ , fe_rentabilidad_anual_ratio :=  mrentabilidad / mrentabilidad_annual ]

mega.df[ , fe_activos_pasivos_ratio :=  mactivos_margen / mpasivos_margen ]
mega.df[ , fe_activos_pasivos_sum :=  rowSums(cbind(mactivos_margen, mpasivos_margen), na.rm=TRUE) ]
mega.df[ , fe_activos_pasivos_sum :=  rowMeans(cbind(mactivos_margen, mpasivos_margen), na.rm=TRUE) ]

mega.df[ , fe_cant_cc_y_caja_ahorro_sum :=  rowSums(cbind(tcuenta_corriente, tcaja_ahorro), na.rm=TRUE) ]
mega.df[ , fe_cuenta_corriente_sum :=  rowSums(cbind( mcuenta_corriente_Nopaquete, mcuenta_corriente_Paquete, mcuenta_corriente_dolares), na.rm=TRUE) ]
mega.df[ , fe_cuenta_corriente_mean :=  rowMeans(cbind(mcuenta_corriente_Nopaquete, mcuenta_corriente_Paquete, mcuenta_corriente_dolares), na.rm=TRUE) ]
mega.df[ , fe_caja_ahorro_sum :=  rowSums(cbind(mcaja_ahorro_Nopaquete, mcaja_ahorro_Paquete, mcaja_ahorro_dolares), na.rm=TRUE) ]
mega.df[ , fe_caja_ahorro_mean :=  rowMeans(cbind(mcaja_ahorro_Nopaquete, mcaja_ahorro_Paquete, mcaja_ahorro_dolares), na.rm=TRUE) ]

mega.df[ , fe_cant_tarjetas_sum :=  rowSums(cbind(ttarjeta_visa, ttarjeta_master, ttarjeta_debito), na.rm=TRUE) ]
mega.df[ , fe_tarjetas_transacciones_sum :=  rowSums(cbind(ctarjeta_visa_transacciones, ctarjeta_master_transacciones, ctarjeta_debito_transacciones), na.rm=TRUE) ]
mega.df[ , fe_tarjetas_consumos_sum :=  rowSums(cbind(mautoservicio, mtarjeta_visa_consumo, mtarjeta_master_consumo), na.rm=TRUE) ]
mega.df[ , fe_tarjetas_consumos_mean :=  rowMeans(cbind(mautoservicio, mtarjeta_visa_consumo, mtarjeta_master_consumo), na.rm=TRUE) ]
mega.df[ , fe_debito_consumos_ratio :=  mautoservicio / ctarjeta_debito_transacciones ]
mega.df[ , fe_visa_consumos_ratio :=  mtarjeta_visa_consumo / ctarjeta_visa_transacciones ]
mega.df[ , fe_master_consumos_ratio :=  mtarjeta_master_consumo / ctarjeta_master_transacciones ]
mega.df[ , fe_tarjetas_consumos_ratio :=  fe_tarjetas_consumos_sum / fe_tarjetas_transacciones_sum ]

mega.df[ , fe_prestamos_cant :=  rowSums(cbind(cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios), na.rm=TRUE) ]
mega.df[ , fe_prestamos_sum :=  rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE) ]
mega.df[ , fe_prestamos_mean :=  rowMeans(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE) ]
mega.df[ , fe_prestamos_ratio :=  fe_prestamos_sum / fe_prestamos_cant ]

mega.df[ , fe_plazo_fijo_sum :=  rowSums(cbind(mplazo_fijo_dolares, mplazo_fijo_pesos), na.rm=TRUE) ]
mega.df[ , fe_plazo_fijo_ratio :=  fe_plazo_fijo_sum / tplazo_fijo ]
mega.df[ , fe_fondos_comunes_inversion_sum :=  rowSums(cbind(mfondos_comunes_inversion_dolares, mfondos_comunes_inversion_pesos), na.rm=TRUE) ]
mega.df[ , fe_fondos_comunes_inversion_ratio :=  fe_fondos_comunes_inversion_sum / tfondos_comunes_inversion ]
mega.df[ , fe_titulos_ratio :=  mtitulos / ttitulos ]
mega.df[ , fe_inversiones_cant :=  rowSums(cbind(tplazo_fijo, tfondos_comunes_inversion, ttitulos), na.rm=TRUE) ]
mega.df[ , fe_inversiones_sum :=  rowSums(cbind(fe_plazo_fijo_sum, fe_fondos_comunes_inversion_sum, mtitulos), na.rm=TRUE) ]
mega.df[ , fe_inversiones_ratio :=  fe_inversiones_sum / fe_inversiones_cant ]

mega.df[ , fe_inversiones2_sum :=  rowSums(cbind(mbonos_corporativos, mmonedas_extranjeras, minversiones_otras, mtitulos), na.rm=TRUE) ]
mega.df[ , fe_inversiones_tot :=  rowSums(cbind(fe_inversiones_sum, fe_inversiones2_sum), na.rm=TRUE) ]

mega.df[ , fe_seguros_cant :=  rowSums(cbind(tseguro_vida_mercado_abierto, tseguro_auto, tseguro_vivienda, tseguro_accidentes_personales), na.rm=TRUE) ]

mega.df[ , fe_sueldo_cant :=  rowSums(cbind(tplan_sueldo, cplan_sueldo_transaccion), na.rm=TRUE) ]
mega.df[ , fe_sueldo_sum :=  rowSums(cbind(mplan_sueldo, mplan_sueldo_manual), na.rm=TRUE) ]
mega.df[ , fe_sueldo_ratio :=  fe_sueldo_sum / fe_sueldo_cant ]

mega.df[ , fe_cuenta_debitos_ratio :=  mcuenta_debitos_automaticos / tcuenta_debitos_automaticos ]
mega.df[ , fe_tarjeta_debitos_cant :=  rowSums(cbind(ttarjeta_visa_debitos_automaticos, ttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
mega.df[ , fe_tarjeta_debitos_sum :=  rowSums(cbind(mttarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
mega.df[ , fe_tarjeta_debitos_mean :=  rowMeans(cbind(mttarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
mega.df[ , fe_tarjeta_debitos_ratio := fe_tarjeta_debitos_sum / fe_tarjeta_debitos_cant]
mega.df[ , fe_debitos_cant :=  rowSums(cbind(tcuenta_debitos_automaticos, fe_tarjeta_debitos_cant), na.rm=TRUE) ]
mega.df[ , fe_debitos_sum :=  rowSums(cbind(mcuenta_debitos_automaticos, fe_tarjeta_debitos_sum), na.rm=TRUE) ]
mega.df[ , fe_debitos_ratio := fe_debitos_sum / fe_debitos_cant]

mega.df[ , fe_pagos_cant :=  rowSums(cbind(tpagodeservicios, tpagomiscuentas), na.rm=TRUE) ]
mega.df[ , fe_pagos_sum :=  rowSums(cbind(mpagodeservicios, mpagomiscuentas), na.rm=TRUE) ]
mega.df[ , fe_pagos_ratio := fe_pagos_sum / fe_pagos_cant]

mega.df[ , fe_cajeros_descuentos_ratio := mcajeros_propios_descuentos / ccajeros_propios_descuentos]
mega.df[ , fe_tarjeta_descuentos_cant :=  rowSums(cbind(ctarjeta_visa_descuentos, ctarjeta_master_descuentos), na.rm=TRUE) ]
mega.df[ , fe_tarjeta_descuentos_sum :=  rowSums(cbind(mtarjeta_visa_descuentos, mtarjeta_master_descuentos), na.rm=TRUE) ]
mega.df[ , fe_tarjeta_descuentos_ratio := fe_tarjeta_descuentos_sum / fe_tarjeta_descuentos_cant]
mega.df[ , fe_cuenta_descuentos_ratio := mcuenta_descuentos / ccuenta_descuentos]
mega.df[ , fe_descuentos_cant :=  rowSums(cbind(fe_tarjeta_descuentos_cant, ccajeros_propios_descuentos, ccuenta_descuentos), na.rm=TRUE) ]
mega.df[ , fe_descuentos_sum :=  rowSums(cbind(fe_tarjeta_descuentos_sum, mcajeros_propios_descuentos, mcuenta_descuentos), na.rm=TRUE) ]
mega.df[ , fe_descuentos_ratio := fe_descuentos_sum / fe_descuentos_cant]

mega.df[ , fe_comisiones_mantenimiento_ratio := mcomisiones_mantenimiento / ccomisiones_mantenimiento]
mega.df[ , fe_comisiones_otras_ratio := mcomisiones_otras / ccomisiones_otras]
mega.df[ , fe_comisiones_cant :=  rowSums(cbind(ccomisiones_mantenimiento, ccomisiones_otras), na.rm=TRUE) ]
mega.df[ , fe_comisiones_sum :=  rowSums(cbind(mcomisiones_mantenimiento, mcomisiones_otras), na.rm=TRUE) ]
mega.df[ , fe_comisiones_ratio := fe_comisiones_sum / fe_comisiones_cant]

mega.df[ , fe_cambio_monedas_sum :=  rowSums(cbind(ccambio_monedas_compra, ccambio_monedas_venta), na.rm=TRUE) ]
mega.df[ , fe_cambio_monedas_compra_ratio := mcambio_monedas_compra / ccambio_monedas_compra]
mega.df[ , fe_cambio_monedas_venta_ratio := mcambio_monedas_venta / ccambio_monedas_venta]
mega.df[ , fe_cambio_monedas_ratio := fe_cambio_monedas_sum / tcambio_monedas]

mega.df[ , fe_transferencias_recibidas_ratio := mtransferencias_recibidas / ctransferencias_recibidas]
mega.df[ , fe_transferencias_emitidas_ratio := mtransferencias_emitidas / ctransferencias_emitidas]
mega.df[ , fe_transferencias_cant :=  rowSums(cbind(ctransferencias_recibidas, ctransferencias_emitidas), na.rm=TRUE) ]
mega.df[ , fe_transferencias_sum :=  rowSums(cbind(mtransferencias_recibidas, mtransferencias_emitidas), na.rm=TRUE) ]
mega.df[ , fe_transferencias_ratio := fe_transferencias_sum / fe_transferencias_cant]

mega.df[ , fe_extraccion_ratio := mextraccion_autoservicio / cextraccion_autoservicio]

mega.df[ , fe_cheques_depositados_ratio := mcheques_depositados / ccheques_depositados]
mega.df[ , fe_cheques_emitidos_ratio := mcheques_emitidos / ccheques_emitidos]
mega.df[ , fe_cheques_depositados_rechazados_ratio := mcheques_depositados_rechazados / ccheques_depositados_rechazados]
mega.df[ , fe_cheques_emitidos_rechazados_ratio := mcheques_emitidos_rechazados / ccheques_emitidos_rechazados]
mega.df[ , fe_cheques_emitidos_vs_rechazados_cant_ratio := ccheques_emitidos_rechazados / ccheques_emitidos]
mega.df[ , fe_cheques_emitidos_vs_rechazados_monto_ratio := mcheques_emitidos_rechazados / mcheques_emitidos]

mega.df[ , fe_autoservicio_ratio := cautoservicio_transacciones / tautoservicio]

# Analicé hacer cosas sobre tcajas, tcajas_consultas, tcajas_depositos, tcajas_extracciones, tcajas_otras
# pero no parecían aportar nada

mega.df[ , fe_cajeros_propio_ratio := mcajeros_propio / ccajeros_propio_transacciones]
mega.df[ , fe_cajeros_ajeno_ratio := mcajeros_ajenos / ccajeros_ajenos_transacciones]
mega.df[ , fe_cajeros_cant :=  rowSums(cbind(ccajeros_propio_transacciones, ccajeros_ajenos_transacciones), na.rm=TRUE) ]
mega.df[ , fe_cajeros_sum :=  rowSums(cbind(mcajeros_propio, mcajeros_ajenos), na.rm=TRUE) ]
mega.df[ , fe_cajeros_ratio := fe_cajeros_sum / fe_cajeros_cant]



autoTestAndScore <- function( full.df, seed=102191, partition=0.7, cutoff=0.025 )
{
    set.seed( seed )
    
    datasets <- ( full.df %>% split.train.test.df(partition, clase) )
    train.df <- globalenv()$DfHolder$new(datasets$train)
    validate.df <- globalenv()$DfHolder$new(datasets$test)
    

    globalenv()$log.debug('training...')
    model = xgb.train( 
        data= train.df$as.xgb.train(),
        objective= "binary:logistic",
        tree_method= "hist",
        max_bin= 31,
        base_score= train.df$mean,
        eta= 0.04,
        nrounds= 300, 
        colsample_bytree= 0.6
    )
    
    
    predictions <- model %>% predict( validate.df$as.xgb.predict())
    score <- globalenv()$score.prediction(predictions, validate.df$as.results(), cutoff )
    return(score)
}

seed = 102191

score.no.fe <- autoTestAndScore(df, seed=seed)
score.std.fe <- autoTestAndScore(dataset, seed=seed)
score.mega.fe <- autoTestAndScore(mega.df, seed=seed)



