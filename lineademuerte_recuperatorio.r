#limpio la memoria
#rm(list=ls())
#gc()

run.this<- function()
{
    globalenv()$log.debug('Starting lineaDeMuerte original')
  
    library( "data.table" )
    library( "xgboost" )
  
    alumno_apellido <- "subotovsky"
  
    #cargo los datasets
  
    workingdir <- globalenv()$get.data.dir('recuperatorio')
    setwd(workingdir)
  
    globalenv()$log.debug('Working dir set to {workingdir}', environment())  
  
  
    globalenv()$log.debug('loading datasets...')
    
    
    
    dataset_generacion   <-   globalenv()$DfHolder$new( init.df=globalenv()$get.train.df() )
    dataset_aplicacion   <-   globalenv()$DfHolder$new( init.df=globalenv()$get.predict.df() )
  

    #llamo al XGBoost,  notar lo frugal de los hiperparametros
    set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles
  
  
    globalenv()$log.debug('training...')
    modelo = xgb.train( 
        data= dataset_generacion$as.xgb.train(),
        objective= "binary:logistic",
        tree_method= "hist",
        max_bin= 31,
        base_score= dataset_generacion$mean,
        eta= 0.04,
        nrounds= 300, 
        colsample_bytree= 0.6
    )
  
    
    globalenv()$log.debug('predicting...')
    
    
    #aplico el modelo a datos nuevos
    aplicacion_prediccion  <- predict(  modelo, dataset_aplicacion$as.xgb.predict() )
    
    #uno las columnas de numero_de_cliente y la probabilidad recien calculada
    prediccion_final  <-  data.table(  id_cliente=dataset_aplicacion$as.clients(), prob_positivo=aplicacion_prediccion )
    

    globalenv()$log.debug('outputting data...')
    
    #Genero las TRES salidas
    #grabo todas las probabilidad, simplemente para tenerlo
    fwrite( prediccion_final[ order( -prob_positivo) ], 
            file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_probabilidades.txt"), 
            sep= "\t", 
            eol= "\r\n")
  
    #Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
    #me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
    fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "id_cliente" ] ), 
            file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_entregar.txt"),
            col.names=FALSE, 
            sep= "\t", 
            eol= "\r\n")
  
    #grabo la importancia de las variables
    write.table(  xgb.importance( model = modelo )
                  , file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_importancia.txt")
                  , sep= "\t"
                  , eol= "\r\n"
    )
    
    globalenv()$log.debug('lineaDeMuerte original done')

}