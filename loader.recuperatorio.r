library(fst, warn.conflicts = FALSE, quietly=TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(gsubfn, warn.conflicts = FALSE, quietly=TRUE)
library(data.table, warn.conflicts = FALSE, quietly=TRUE)
library(glue, warn.conflicts = FALSE, quietly=TRUE)
library(zeallot, warn.conflicts = FALSE, quietly=TRUE)
library(xgboost, warn.conflicts = FALSE, quietly=TRUE )
library(tictoc, warn.conflicts = FALSE, quietly=TRUE )
library(purrr, warn.conflicts = FALSE, quietly=TRUE )


#### Check environment stuff here:
# http://adv-r.had.co.nz/Environments.html



MASTER_SEED <- 123457


###### Differentiation ######

get.os <- function()
{
   return(Sys.info()['sysname'])
}


get.env <- function()
{
    # windows environment
    if (get.os() == 'Windows') return(list(
                                  code_dir='C:/Users/Luxor/Documents/GitHub/Test_01/dm_finanzas',
                                  data_dir='C:/Users/Luxor/Documents/DM Finanzas/data.recuperatorio')
                                      )
    
    # linux environment
    else return(list(
                  code_dir='~/dm_finanzas',
                  data_dir='~/cloud/cloud1')
                )
}


make.dir <- function( path )
{
  matches <- str_match(path,"^(.*)(?:\\\\|\\/)(.*)$")
  dir.part <- matches[[2]]
  dir.create(dir.part, recursive=TRUE, showWarnings=FALSE)
}


get.data.dir <- function( ..., auto.create=FALSE )
{
    full.path <- file.path( get.env()$data_dir, ...  )
    if (auto.create) make.dir( full.path )
    return( full.path )
}


get.code.dir <- function( ..., auto.create=FALSE )
{
    full.path <- file.path( get.env()$code_dir, ...  )
    if (auto.create) make.dir( full.path )
    return( full.path )
}




################## metrics ################

get.roc.data <- function( predict, target )
{
    # calculo metricas
    roc_pred <- ROCR::prediction(predict, target, label.ordering = c(0, 1))
    
    # extraigo AUC
    auc <- unlist(ROCR::performance(roc_pred, "auc")@y.values)
    
    # extraigo ROC
    curve <- ROCR::performance(roc_pred,"tpr","fpr")
    curve.as.df <- data.table(x=curve@x.values[[1]], y=curve@y.values[[1]])
    colnames(curve.as.df) = c( 'x', 'y' )
    
    return(list( auc=auc, curve=curve.as.df))
}


get.gain.vs.prob <- function( prediction, target )
{
    prob.table <- data.table(prob=prediction, target=target) %>%
        arrange(-prob) %>%
        mutate(single.gain=ifelse(target==1, 19500, -500)) %>%
        mutate(gain=cumsum(single.gain)) %>%
        select(-single.gain)
    
    return(prob.table)
}



############# Scoring Daraframe ###############

score.prediction <- function( pred.probs, actual, cutoff=0.025, relative=TRUE )
{
    normalization <- ifelse( relative==TRUE, length(actual), 1 )
    score.df <- data.table( prob=pred.probs, target=actual ) %>%
        filter( prob>=cutoff ) %>%
        mutate( points=if_else( target==1 | target=='SI', 19500, -500) )
    
    score <- sum( score.df$points ) / normalization
    return(score)
}


################ Prepare training df - xgb specific #################



DfHolder <- setRefClass('DfHolder',
    fields = c('df','mean'),
    
    methods=list(
      
        # initializer (load df, calculate)
        initialize = function( init.df )
        {
            log.debug('initializing matrix...')
          
            # si no tiene clase01, la creamos a partir de clase
            if (!('clase01' %in% (init.df %>% colnames())))
                if ('clase' %in% (init.df %>% colnames()))
                    init.df <- init.df %>% mutate(clase01=if_else(clase=="SI", 1, 0))
            
            log.debug('Removing class...')
            
            # sacamos 'clase' si lo tiene
            init.df <- init.df %>% select( -matches('^clase$') )
            
            log.debug('Calculating mean...')
            mean <<- init.df$clase01 %>% mean()
            
            df <<- init.df
            log.debug('Init DF done.')
        },
        
        as.lgb.train = function()
        {
            return( lgb.Dataset(
                data  = data.matrix( df %>% select( -id_cliente, -clase01 ) ),
                label = df$clase01,
                free_raw_data=FALSE
            ))
        },
        as.lgb.predict = function()
        {
            return( as.matrix( df %>% select( -id_cliente, -matches('^clase'))))
        },        

        # Make the df into an xgb trainable thing
        as.xgb.train = function()
        {
            return( xgb.DMatrix(
                data  = data.matrix( df %>% select( -id_cliente, -clase01 ) ),
                label = df$clase01
            ))
        },
        
        # Make the df into an xgb predictable thing
        as.xgb.predict = function()
        {
            return( xgb.DMatrix(data  = data.matrix( df %>% select( -id_cliente, -matches('^clase')))))
        },        
        
        # Get the results of the df
        as.results = function()
        {
            return( df$clase01 )
        },
        
        # Get the results of the df
        as.clients = function()
        {
            return( df$id_cliente )
        }
    )
)




########### Loading and saving ###################

fst.read <- function(path)
{
    wd<-getwd()
    log.debug('reading df {path}  -- [{wd}]', environment() )
    return( fst::read.fst(path, as.data.table=T) )
}

fst.write <- function(df, path)
{
    wd<-getwd()
    log.debug('writing to df {path}  -- [{wd}]', environment())
    return( fst::write.fst(df, path, compress=100 ) )
}


get.train.df <- function()
{
    dfpath <- get.data.dir('recuperatorio', 'subotovsky_generacion.rds')
    return( fst.read(dfpath) )
}


get.predict.df <- function()
{
    dfpath <- get.data.dir('recuperatorio', 'subotovsky_aplicacion.rds')
    return( fst.read(dfpath) )
}

get.predict.results <- function()
{
  dfpath <- get.data.dir('recuperatorio', 'subotovsky_realidad.rds')
  return( fst.read(dfpath) )
}

get.predict.full.df <- function()
{
  dfpath <- get.data.dir('recuperatorio', 'subotovsky_aplicacion_full.rds')
  return( fst.read(dfpath) )
}


############ (sub)Sampling ###########################

sample.df <- function( df, fraction, ..., replace=F )
{
    # This! : https://dplyr.tidyverse.org/articles/programming.html
    group_var <- enquos(...)
    return( df %>% group_by( !!!group_var ) %>% sample_frac( fraction, replace=replace ) %>% ungroup() %>% as.data.table() )
}


split.train.test.df <- function( df, fraction=0.7, seed=-1 )
{
    # If no parameter specified, set master seed
    if (is.numeric(seed) & seed==-1)
    {
        set.seed(MASTER_SEED)
        log.debug('[WARNING] Environment seed overriden with master seed {MASTER_SEED}!')
    }
    
    # If a numeric seed is specified, set it
    else if (is.numeric(seed))
    {
        set.seed(seed)
        log.debug('[WARNING] Environment seed reset to {seed}', environment())
    }
    else
    {
        log.debug('[WARNING] USing current seed / state')
    }

    
    train <- df %>% sample.df( fraction,clase, replace=F )
    test <- df %>% anti_join(train, by='id_cliente')

    return (list(train=train, test=test))
}


summary.group <- function( df, ... )
{
    group_var <- enquos(...)
    return ( df %>% 
             group_by( !!!group_var ) %>%
             summarize(n=n()) %>%
             mutate(freq=n/sum(n)) )
}


############### Logging ################

log.debug <- function(msg, env=NULL)
{
    if (!is.null(env))
        print(paste(Sys.time(), glue_data(env, msg), sep=' : '))

    else
        print(paste(Sys.time(), glue(msg), sep=' : '))
}


############### Seeds ################

get.seeds <- function( n=20 )
{
    return( data.table(seed=c(
    MASTER_SEED,
    102191,
    253751,
    466787,
    219277,
    529229,
    664133,
    413353,
    418637,
    568201,
    647753,
    326869,
    598691,
    751997,
    592747,
    531287,
    778049,
    289169,
    796151,
    305971,
    977593,
    26449,26459,26479,26489,26497,26501,26513,26539,26557,26561,
    26573,26591,26597,26627,26633,26641,26647,26669,26681,26683,
    26687,26693,26699,26701,26711,26713,26717,26723,26729,26731,
    26737,26759,26777,26783,26801,26813,26821,26833,26839,26849,
    26861,26863,26879,26881,26891,26893,26903,26921,26927,26947,
    33487,33493,33503,33521,33529,33533,33547,33563,33569,33577,
    33581,33587,33589,33599,33601,33613,33617,33619,33623,33629,
    33637,33641,33647,33679,33703,33713,33721,33739,33749,33751,
    33757,33767,33769,33773,33791,33797,33809,33811,33827,33829,
    33851,33857,33863,33871,33889,33893,33911,33923,33931,33937
    )) %>% head(n) )
}


############# Feature Engineering ##############

enrich.fe.std <- function( dataset.original )
{
    # not sure if parameter is copied or by reference. Just in case...
    dataset <- data.table(dataset.original)
  
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

    return( dataset )
}



enrich.fe.small.extended <- function( dataset.original )
{
    dataset <- data.table(dataset.original)
    
    dataset[ , mv_mconsumo:= rowSums( cbind( mtarjeta_master_consumo, mtarjeta_visa_consumo) , na.rm=TRUE ) ]
    dataset[ , mv_mdescuentos := rowSums( cbind(mcajeros_propios_descuentos, mtarjeta_visa_descuentos, mtarjeta_master_descuentos, mcuenta_descuentos) , na.rm=TRUE ) ]
    dataset[ , mv_mcomisiones := rowSums( cbind(mcomisiones_mantenimiento, mcomisiones_otras) , na.rm=TRUE ) ]
    dataset[ , mvr_Master_mconsumo:= mtarjeta_master_consumo / mv_mconsumo ]
    dataset[ , mvr_Visa_mconsumo:= mtarjeta_visa_consumo / mv_mconsumo ]
    dataset[ , mvr_Cajeros_mdescuentos:= mcajeros_propios_descuentos / mv_mdescuentos ]
    dataset[ , mvr_Visa_mdescuentos:= mtarjeta_visa_descuentos / mv_mdescuentos ]
    dataset[ , mvr_Master_mdescuentos:= mtarjeta_master_descuentos / mv_mdescuentos ]
    dataset[ , mvr_Cuenta_mdescuentos:= mcuenta_descuentos / mv_mdescuentos ]
    dataset[ , mvr_mcomisiones_mante:= mcomisiones_mantenimiento / mv_mcomisiones ]
    dataset[ , mvr_mcomisiones_otras:= mcomisiones_otras / mv_mcomisiones ]
    
    return( dataset )
}

enrich.fe.extended <- function( dataset.original )
{
    dataset <- data.table(dataset.original)  
  
    dataset[ , fe_antiguedad_edad_ratio :=  cliente_antiguedad / 12 / cliente_edad ]
    dataset[ , fe_rentabilidad_anual_ratio :=  mrentabilidad / mrentabilidad_annual ]
    
    dataset[ , fe_activos_pasivos_ratio :=  mactivos_margen / mpasivos_margen ]
    dataset[ , fe_activos_pasivos_sum :=  rowSums(cbind(mactivos_margen, mpasivos_margen), na.rm=TRUE) ]
    dataset[ , fe_activos_pasivos_sum :=  rowMeans(cbind(mactivos_margen, mpasivos_margen), na.rm=TRUE) ]
    
    dataset[ , fe_cant_cc_y_caja_ahorro_sum :=  rowSums(cbind(tcuenta_corriente, tcaja_ahorro), na.rm=TRUE) ]
    dataset[ , fe_cuenta_corriente_sum :=  rowSums(cbind( mcuenta_corriente_Nopaquete, mcuenta_corriente_Paquete, mcuenta_corriente_dolares), na.rm=TRUE) ]
    dataset[ , fe_cuenta_corriente_mean :=  rowMeans(cbind(mcuenta_corriente_Nopaquete, mcuenta_corriente_Paquete, mcuenta_corriente_dolares), na.rm=TRUE) ]
    dataset[ , fe_caja_ahorro_sum :=  rowSums(cbind(mcaja_ahorro_Nopaquete, mcaja_ahorro_Paquete, mcaja_ahorro_dolares), na.rm=TRUE) ]
    dataset[ , fe_caja_ahorro_mean :=  rowMeans(cbind(mcaja_ahorro_Nopaquete, mcaja_ahorro_Paquete, mcaja_ahorro_dolares), na.rm=TRUE) ]
    
    dataset[ , fe_cant_tarjetas_sum :=  rowSums(cbind(ttarjeta_visa, ttarjeta_master, ttarjeta_debito), na.rm=TRUE) ]
    dataset[ , fe_tarjetas_transacciones_sum :=  rowSums(cbind(ctarjeta_visa_transacciones, ctarjeta_master_transacciones, ctarjeta_debito_transacciones), na.rm=TRUE) ]
    dataset[ , fe_tarjetas_consumos_sum :=  rowSums(cbind(mautoservicio, mtarjeta_visa_consumo, mtarjeta_master_consumo), na.rm=TRUE) ]
    dataset[ , fe_tarjetas_consumos_mean :=  rowMeans(cbind(mautoservicio, mtarjeta_visa_consumo, mtarjeta_master_consumo), na.rm=TRUE) ]
    dataset[ , fe_debito_consumos_ratio :=  mautoservicio / ctarjeta_debito_transacciones ]
    dataset[ , fe_visa_consumos_ratio :=  mtarjeta_visa_consumo / ctarjeta_visa_transacciones ]
    dataset[ , fe_master_consumos_ratio :=  mtarjeta_master_consumo / ctarjeta_master_transacciones ]
    dataset[ , fe_tarjetas_consumos_ratio :=  fe_tarjetas_consumos_sum / fe_tarjetas_transacciones_sum ]
    
    dataset[ , fe_prestamos_cant :=  rowSums(cbind(cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios), na.rm=TRUE) ]
    dataset[ , fe_prestamos_sum :=  rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE) ]
    dataset[ , fe_prestamos_mean :=  rowMeans(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm=TRUE) ]
    dataset[ , fe_prestamos_ratio :=  fe_prestamos_sum / fe_prestamos_cant ]
    
    dataset[ , fe_plazo_fijo_sum :=  rowSums(cbind(mplazo_fijo_dolares, mplazo_fijo_pesos), na.rm=TRUE) ]
    dataset[ , fe_plazo_fijo_ratio :=  fe_plazo_fijo_sum / tplazo_fijo ]
    dataset[ , fe_fondos_comunes_inversion_sum :=  rowSums(cbind(mfondos_comunes_inversion_dolares, mfondos_comunes_inversion_pesos), na.rm=TRUE) ]
    dataset[ , fe_fondos_comunes_inversion_ratio :=  fe_fondos_comunes_inversion_sum / tfondos_comunes_inversion ]
    dataset[ , fe_titulos_ratio :=  mtitulos / ttitulos ]
    dataset[ , fe_inversiones_cant :=  rowSums(cbind(tplazo_fijo, tfondos_comunes_inversion, ttitulos), na.rm=TRUE) ]
    dataset[ , fe_inversiones_sum :=  rowSums(cbind(fe_plazo_fijo_sum, fe_fondos_comunes_inversion_sum, mtitulos), na.rm=TRUE) ]
    dataset[ , fe_inversiones_ratio :=  fe_inversiones_sum / fe_inversiones_cant ]
    
    dataset[ , fe_inversiones2_sum :=  rowSums(cbind(mbonos_corporativos, mmonedas_extranjeras, minversiones_otras, mtitulos), na.rm=TRUE) ]
    dataset[ , fe_inversiones_tot :=  rowSums(cbind(fe_inversiones_sum, fe_inversiones2_sum), na.rm=TRUE) ]
    
    dataset[ , fe_seguros_cant :=  rowSums(cbind(tseguro_vida_mercado_abierto, tseguro_auto, tseguro_vivienda, tseguro_accidentes_personales), na.rm=TRUE) ]
    
    dataset[ , fe_sueldo_cant :=  rowSums(cbind(tplan_sueldo, cplan_sueldo_transaccion), na.rm=TRUE) ]
    dataset[ , fe_sueldo_sum :=  rowSums(cbind(mplan_sueldo, mplan_sueldo_manual), na.rm=TRUE) ]
    dataset[ , fe_sueldo_ratio :=  fe_sueldo_sum / fe_sueldo_cant ]
    
    dataset[ , fe_cuenta_debitos_ratio :=  mcuenta_debitos_automaticos / tcuenta_debitos_automaticos ]
    dataset[ , fe_tarjeta_debitos_cant :=  rowSums(cbind(ttarjeta_visa_debitos_automaticos, ttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
    dataset[ , fe_tarjeta_debitos_sum :=  rowSums(cbind(mttarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
    dataset[ , fe_tarjeta_debitos_mean :=  rowMeans(cbind(mttarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos), na.rm=TRUE) ]
    dataset[ , fe_tarjeta_debitos_ratio := fe_tarjeta_debitos_sum / fe_tarjeta_debitos_cant]
    dataset[ , fe_debitos_cant :=  rowSums(cbind(tcuenta_debitos_automaticos, fe_tarjeta_debitos_cant), na.rm=TRUE) ]
    dataset[ , fe_debitos_sum :=  rowSums(cbind(mcuenta_debitos_automaticos, fe_tarjeta_debitos_sum), na.rm=TRUE) ]
    dataset[ , fe_debitos_ratio := fe_debitos_sum / fe_debitos_cant]
    
    dataset[ , fe_pagos_cant :=  rowSums(cbind(tpagodeservicios, tpagomiscuentas), na.rm=TRUE) ]
    dataset[ , fe_pagos_sum :=  rowSums(cbind(mpagodeservicios, mpagomiscuentas), na.rm=TRUE) ]
    dataset[ , fe_pagos_ratio := fe_pagos_sum / fe_pagos_cant]
    
    dataset[ , fe_cajeros_descuentos_ratio := mcajeros_propios_descuentos / ccajeros_propios_descuentos]
    dataset[ , fe_tarjeta_descuentos_cant :=  rowSums(cbind(ctarjeta_visa_descuentos, ctarjeta_master_descuentos), na.rm=TRUE) ]
    dataset[ , fe_tarjeta_descuentos_sum :=  rowSums(cbind(mtarjeta_visa_descuentos, mtarjeta_master_descuentos), na.rm=TRUE) ]
    dataset[ , fe_tarjeta_descuentos_ratio := fe_tarjeta_descuentos_sum / fe_tarjeta_descuentos_cant]
    dataset[ , fe_cuenta_descuentos_ratio := mcuenta_descuentos / ccuenta_descuentos]
    dataset[ , fe_descuentos_cant :=  rowSums(cbind(fe_tarjeta_descuentos_cant, ccajeros_propios_descuentos, ccuenta_descuentos), na.rm=TRUE) ]
    dataset[ , fe_descuentos_sum :=  rowSums(cbind(fe_tarjeta_descuentos_sum, mcajeros_propios_descuentos, mcuenta_descuentos), na.rm=TRUE) ]
    dataset[ , fe_descuentos_ratio := fe_descuentos_sum / fe_descuentos_cant]
    
    dataset[ , fe_comisiones_mantenimiento_ratio := mcomisiones_mantenimiento / ccomisiones_mantenimiento]
    dataset[ , fe_comisiones_otras_ratio := mcomisiones_otras / ccomisiones_otras]
    dataset[ , fe_comisiones_cant :=  rowSums(cbind(ccomisiones_mantenimiento, ccomisiones_otras), na.rm=TRUE) ]
    dataset[ , fe_comisiones_sum :=  rowSums(cbind(mcomisiones_mantenimiento, mcomisiones_otras), na.rm=TRUE) ]
    dataset[ , fe_comisiones_ratio := fe_comisiones_sum / fe_comisiones_cant]
    
    dataset[ , fe_cambio_monedas_sum :=  rowSums(cbind(ccambio_monedas_compra, ccambio_monedas_venta), na.rm=TRUE) ]
    dataset[ , fe_cambio_monedas_compra_ratio := mcambio_monedas_compra / ccambio_monedas_compra]
    dataset[ , fe_cambio_monedas_venta_ratio := mcambio_monedas_venta / ccambio_monedas_venta]
    dataset[ , fe_cambio_monedas_ratio := fe_cambio_monedas_sum / tcambio_monedas]
    
    dataset[ , fe_transferencias_recibidas_ratio := mtransferencias_recibidas / ctransferencias_recibidas]
    dataset[ , fe_transferencias_emitidas_ratio := mtransferencias_emitidas / ctransferencias_emitidas]
    dataset[ , fe_transferencias_cant :=  rowSums(cbind(ctransferencias_recibidas, ctransferencias_emitidas), na.rm=TRUE) ]
    dataset[ , fe_transferencias_sum :=  rowSums(cbind(mtransferencias_recibidas, mtransferencias_emitidas), na.rm=TRUE) ]
    dataset[ , fe_transferencias_ratio := fe_transferencias_sum / fe_transferencias_cant]
    
    dataset[ , fe_extraccion_ratio := mextraccion_autoservicio / cextraccion_autoservicio]
    
    dataset[ , fe_cheques_depositados_ratio := mcheques_depositados / ccheques_depositados]
    dataset[ , fe_cheques_emitidos_ratio := mcheques_emitidos / ccheques_emitidos]
    dataset[ , fe_cheques_depositados_rechazados_ratio := mcheques_depositados_rechazados / ccheques_depositados_rechazados]
    dataset[ , fe_cheques_emitidos_rechazados_ratio := mcheques_emitidos_rechazados / ccheques_emitidos_rechazados]
    dataset[ , fe_cheques_emitidos_vs_rechazados_cant_ratio := ccheques_emitidos_rechazados / ccheques_emitidos]
    dataset[ , fe_cheques_emitidos_vs_rechazados_monto_ratio := mcheques_emitidos_rechazados / mcheques_emitidos]
    
    dataset[ , fe_autoservicio_ratio := cautoservicio_transacciones / tautoservicio]
    
    # Analicé hacer cosas sobre tcajas, tcajas_consultas, tcajas_depositos, tcajas_extracciones, tcajas_otras
    # pero no parecían aportar nada
    
    dataset[ , fe_cajeros_propio_ratio := mcajeros_propio / ccajeros_propio_transacciones]
    dataset[ , fe_cajeros_ajeno_ratio := mcajeros_ajenos / ccajeros_ajenos_transacciones]
    dataset[ , fe_cajeros_cant :=  rowSums(cbind(ccajeros_propio_transacciones, ccajeros_ajenos_transacciones), na.rm=TRUE) ]
    dataset[ , fe_cajeros_sum :=  rowSums(cbind(mcajeros_propio, mcajeros_ajenos), na.rm=TRUE) ]
    dataset[ , fe_cajeros_ratio := fe_cajeros_sum / fe_cajeros_cant]
    
    return( dataset )
}

source(get.code.dir('testing.pipeline.r'))

