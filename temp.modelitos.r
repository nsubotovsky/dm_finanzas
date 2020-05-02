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

###########################
#
#  Modelo otro - modelitos
#
###########################








library(data.table)
library(caret)

set.seed(410551)


#raiz del environment
env <- list()
data <- list()
data$campos_separador     <-  "\t"
data$campo_id             <-  "id_cliente"
data$campos_a_borrar      <-  c()
data$archivo_grande       <-  'data_generacion.txt'
data$archivo_test         <-  'maurino_aplicacion.txt'
data$undersampling        <-  0.2
data$ventana              <-  10
data$clase_nomcampo       <- 'clase'
env$data <- data

#sobre el funcionamiento de programa
#en el lightgbm
lgb <- list()
lgb$folds                 <-      3
lgb$semilla               <- 102191
lgb$max_bin               <-    255
lgb$subsample             <-      1.0
lgb$num_iterations_max    <-   1000
lgb$early_stopping_round  <-     30
lgb$num_leaves            <-   2048
env$lightgbm <- lgb

problema <- list()
problema$ganancia_acierto <- 19500
problema$ganancia_noacierto <- -500
env$problema <- problema

#Hacer que la variable env NO se pueda modificar
lockBinding( "env", globalenv() )






Gprob_corte <- 0.025

fganancia_logistic_lightgbm   <- function(probs, clases) 
{
    
    vlabels <- getinfo(clases, "label")
    
    gan <-sum(   (probs > Gprob_corte  ) * 
                     ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
    )
    
    
    return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}



dataset_grande <- get.train.df() #dejo la clase en {0,1}  
dataset_grande[, clase01 := as.integer( get(env$data$clase_nomcampo) == 'SI'  )] 

#agrego variable para el undersampling
dataset_grande[ , sample := runif( nrow(dataset_grande) )]

#Creo el dataset donde almaceno temporariamente las nuevas variables modelo
dataset_salida <- dataset_grande[ , c('id_cliente'), with=FALSE ]


fe <- function(datos)
{
    datos[ , mv_cuenta_estado2 := pmax( Master_cuenta_estado, Visa_cuenta_estado, na.rm = TRUE) ]
    datos[ , mv_marca_atraso := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]
    datos[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite, Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
    datos[ , mv_Fvencimiento := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
    datos[ , mv_Finiciomora := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
    datos[ , mv_msaldototal := rowSums( cbind( Master_msaldototal, Visa_msaldototal) , na.rm=TRUE ) ]
    datos[ , mv_msaldopesos := rowSums( cbind( Master_msaldopesos, Visa_msaldopesos) , na.rm=TRUE ) ]
    datos[ , mv_msaldodolares := rowSums( cbind( Master_msaldodolares, Visa_msaldodolares) , na.rm=TRUE ) ]
    datos[ , mv_mconsumospesos := rowSums( cbind( Master_mconsumospesos, Visa_mconsumospesos) , na.rm=TRUE ) ]
    datos[ , mv_mconsumosdolares := rowSums( cbind( Master_mconsumosdolares, Visa_mconsumosdolares) , na.rm=TRUE ) ]
    datos[ , mv_mlimitecompra := rowSums( cbind( Master_mlimitecompra, Visa_mlimitecompra) , na.rm=TRUE ) ]
    datos[ , mv_madelantopesos := rowSums( cbind( Master_madelantopesos, Visa_madelantopesos) , na.rm=TRUE ) ]
    datos[ , mv_madelantodolares := rowSums( cbind( Master_madelantodolares, Visa_madelantodolares) , na.rm=TRUE ) ]
    datos[ , mv_fultimo_cierre := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
    datos[ , mv_mpagado := rowSums( cbind( Master_mpagado, Visa_mpagado) , na.rm=TRUE ) ]
    datos[ , mv_mpagospesos := rowSums( cbind( Master_mpagospesos, Visa_mpagospesos) , na.rm=TRUE ) ]
    datos[ , mv_mpagosdolares := rowSums( cbind( Master_mpagosdolares, Visa_mpagosdolares) , na.rm=TRUE ) ]
    datos[ , mv_fechaalta := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
    datos[ , mv_mconsumototal := rowSums( cbind( Master_mconsumototal, Visa_mconsumototal) , na.rm=TRUE ) ]
    datos[ , mv_tconsumos := rowSums( cbind( Master_tconsumos, Visa_tconsumos) , na.rm=TRUE ) ]
    datos[ , mv_tadelantosefectivo := rowSums( cbind( Master_tadelantosefectivo, Visa_tadelantosefectivo) , na.rm=TRUE ) ]
    datos[ , mv_mpagominimo := rowSums( cbind( Master_mpagominimo, Visa_mpagominimo) , na.rm=TRUE ) ]
    datos[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
    datos[ , mvr_Visa_mlimitecompra := Visa_mlimitecompra / mv_mlimitecompra ]
    datos[ , mvr_msaldototal := mv_msaldototal / mv_mlimitecompra ]
    datos[ , mvr_msaldopesos := mv_msaldopesos / mv_mlimitecompra ]
    datos[ , mvr_msaldopesos2 := mv_msaldopesos / mv_msaldototal ]
    datos[ , mvr_msaldodolares := mv_msaldodolares / mv_mlimitecompra ]
    datos[ , mvr_msaldodolares2 := mv_msaldodolares / mv_msaldototal ]
    datos[ , mvr_mconsumospesos := mv_mconsumospesos / mv_mlimitecompra ]
    datos[ , mvr_mconsumosdolares := mv_mconsumosdolares / mv_mlimitecompra ]
    datos[ , mvr_madelantopesos := mv_madelantopesos / mv_mlimitecompra ]
    datos[ , mvr_madelantodolares := mv_madelantodolares / mv_mlimitecompra ]
    datos[ , mvr_mpagado := mv_mpagado / mv_mlimitecompra ]
    datos[ , mvr_mpagospesos := mv_mpagospesos / mv_mlimitecompra ]
    datos[ , mvr_mpagosdolares := mv_mpagosdolares / mv_mlimitecompra ]
    datos[ , mvr_mconsumototal := mv_mconsumototal / mv_mlimitecompra ]
    datos[ , mvr_mpagominimo := mv_mpagominimo / mv_mlimitecompra ]
    datos[ , mv_mconsumo:= rowSums( cbind( mtarjeta_master_consumo, mtarjeta_visa_consumo) , na.rm=TRUE ) ]
    
    datos[ , mv_mdescuentos := rowSums( cbind(mcajeros_propios_descuentos, mtarjeta_visa_descuentos, mtarjeta_master_descuentos, mcuenta_descuentos) , na.rm=TRUE ) ]
    datos[ , mv_mcomisiones := rowSums( cbind(mcomisiones_mantenimiento, mcomisiones_otras) , na.rm=TRUE ) ]
    datos[ , mvr_Master_mconsumo:= mtarjeta_master_consumo / mv_mconsumo ]
    datos[ , mvr_Visa_mconsumo:= mtarjeta_visa_consumo / mv_mconsumo ]
    datos[ , mvr_Cajeros_mdescuentos:= mcajeros_propios_descuentos / mv_mdescuentos ]
    datos[ , mvr_Visa_mdescuentos:= mtarjeta_visa_descuentos / mv_mdescuentos ]
    datos[ , mvr_Master_mdescuentos:= mtarjeta_master_descuentos / mv_mdescuentos ]
    datos[ , mvr_Cuenta_mdescuentos:= mcuenta_descuentos / mv_mdescuentos ]
    datos[ , mvr_mcomisiones_mante:= mcomisiones_mantenimiento / mv_mcomisiones ]
    datos[ , mvr_mcomisiones_otras:= mcomisiones_otras / mv_mcomisiones ]
    return(datos)
}



dataset_grande <- data.table(fe(dataset_grande))


cantidad_folds <- 5
folds <- createFolds(dataset_grande$clase01, cantidad_folds)





#Defino z con unos valores iniciales
#Aqui el alumno sagaz condimentara a gusto
z <- list( "feature_fraction"=1.0, "learning_rate"=0.3, "max_depth"=4, "min_data_in_leaf"=10, "min_gain_to_split"=1.0,  "lambda_l1"=10, "lambda_l2"=20)
tb_modelos <-  as.data.table( z )

#Genero parametros muy distintos
for( plearning_rate  in c( 0.3, 0.03 ) )
{
    z$learning_rate <- plearning_rate
    for( pmax_depth  in c( 2, 6, 12 ) )
    {
        z$max_depth <- pmax_depth
        for( pmin_data_in_leaf  in c( 1, 100 ) )
        {
            z$min_data_in_leaf  <- pmin_data_in_leaf
            for( pfeature_fraction  in c( 0.5, 1.0) )
            {
                z$feature_fraction <- pfeature_fraction
                tb_modelos <-  rbind( tb_modelos,  z )
            }
        }
    }
}



#Asigno un ID a la lista de parametros recien generada
tb_modelos[  , id:= 100:(100+nrow(tb_modelos)-1) ]

#En tb_modelos quedo lo que se va a procesar
tb_modelos


for( fold in folds )
{
    #generacion de  dtrain  , que tiene undersampling
    dataset_grande[  , train := 0L ]
    dataset_grande[ (!row.names(dataset_grande) %in% fold) & (sample < env$data$undersampling | clase01==1), train := 1L ]

    #A este dataset es el que le voy a aplicar TODOS los modelos
    dtrain_under <-   lgb.Dataset( data  = as.matrix(dataset_grande[ train==1, 
                                                                     !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                                     with=FALSE]),
                                   label = dataset_grande[ train==1, clase01 ] ,
                                   free_raw_data=FALSE )
    
    #generacion de dtrain_completo , que NO tiene undersampling
    
    dataset_grande[ , train := 0L ]
    dataset_grande[ (!row.names(dataset_grande) %in% fold), train := 1L ]
    
    #A este dataset es el que le voy a aplicar TODOS los modelos
    dtrain_completo <- lgb.Dataset( data = as.matrix(dataset_grande[ train==1,
                                                                     !c('sample','train','clase01', env$data$campo_id, env$data$clase_nomcampo),
                                                                     with=FALSE]),
                                    label = dataset_grande[ train==1, clase01 ] ,
                                    free_raw_data=FALSE )
    
    for( vid  in tb_modelos$id )
    {  
        z <- tb_modelos[ id==vid, ]
        Gprob_corte  <<-   -problema$ganancia_noacierto*(1/env$data$undersampling)/( problema$ganancia_acierto - problema$ganancia_noacierto*(1/env$data$undersampling) )
        
        
        #aqui hago  CROSS VALIDATION, para determinar el  num_iterations  optimo
        modelo = lgb.cv( 
            data= dtrain_under,  
            objective= "binary",
            nfold= env$lightgbm$folds,
            stratified= TRUE,
            eval= fganancia_logistic_lightgbm, 
            metric= "ganancia" ,
            num_leaves= env$lightgbm$num_leaves,
            num_iterations= env$lightgbm$num_iterations_max,
            early_stopping_rounds= env$lightgbm$early_stopping_round,
            max_bin= env$lightgbm$max_bin,
            boost_from_average= TRUE ,
            subsample= env$lightgbm$subsample, 
            feature_fraction= z$feature_fraction, 
            learning_rate= z$learning_rate,
            min_data_in_leaf= z$min_data_in_leaf, 
            max_depth= z$max_depth,
            lambda_l1= z$lambda_l1, 
            lambda_l2= z$lambda_l2, 
            min_gain_to_split= z$min_gain_to_split,
            verbosity= -1,
            verbose= -1
        )
        
        iteraciones <-  modelo$best_iter
        #unlist(modelo$record_evals$valid$ganancia$eval) 
        
        
        #Aqui genero el modelo, con el recien calculado  iteraciones
        mfinal = lgb.train( 
            data= dtrain_completo,  
            objective= "binary",
            num_leaves= env$lightgbm$num_leaves,
            num_iterations= iteraciones,
            max_bin= env$lightgbm$max_bin,
            boost_from_average= TRUE ,
            subsample= env$lightgbm$subsample, 
            feature_fraction= z$feature_fraction, 
            learning_rate= z$learning_rate,
            min_data_in_leaf= z$min_data_in_leaf, 
            max_depth= z$max_depth,
            lambda_l1= z$lambda_l1, 
            lambda_l2= z$lambda_l2, 
            min_gain_to_split= z$min_gain_to_split,
            verbosity= -1,
            verbose= -1
        )
        
        #Aplico mfinal a los datos de mes_cero
        prediccion  <- predict( mfinal,  
                                as.matrix( dataset_grande[ fold,
                                                           !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                           with=FALSE ] ) )
        
        dataset_salida[ fold,  paste0( "modelito_", z$id) :=  prediccion ]
        
        rm( modelo, mfinal, prediccion )
        gc()
    }
    rm( dtrain_under, dtrain_completo)
    gc()
}

    
    
    

    
    


