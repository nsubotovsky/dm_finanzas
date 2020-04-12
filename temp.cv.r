
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



globalenv()$log.debug('Starting lineaDeMuerte original')

library( "data.table" )
library( "xgboost" )

alumno_apellido <- "subotovsky"

#cargo los datasets

workingdir <- globalenv()$get.data.dir('recuperatorio')
setwd(workingdir)

globalenv()$log.debug('Working dir set to {workingdir}', environment())  


globalenv()$log.debug('loading datasets...')
dataset_generacion   <-   globalenv()$get.train.df()
dataset_aplicacion   <-   globalenv()$get.predict.df()


c(train.df, validate.df) %<-% ( dataset_generacion %>% split.train.test.df(0.7, clase) )


as.xgbMatrix <- function(df)
{
    df.output.as.01 <- df %>% mutate( clase01=if_else(clase=="SI", 1, 0) )
    xgb.matrix.df <- xgb.DMatrix(
        data  = data.matrix( df.output.as.01 %>% select( -id_cliente, -clase, -clase01 ) ),
        label = df.output.as.01$clase01
    )
    return(xgb.matrix.df)
}



#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles


globalenv()$log.debug('training...')


testFunc <- function(preds, dtrain)
{
    cut<-0.025
    total.rows<-nrow(dtrain)
    
    score.df <- data.table(pred=preds,
                           actual=dtrain %>% getinfo('label')) %>%
        filter( pred >= cut ) %>%
        mutate( points=if_else(actual==1, 19500, -500) )
    
    points <- sum( score.df$points ) / total.rows
    
    return( list(metrics='suboMetrics', value=points ))
}

train.matrix <- train.df %>% as.xgbMatrix()
validate.matrix <- validate.df %>% as.xgbMatrix()

aa <- xgb.cv(data=train.matrix,
             nfold=5,
             objective= "binary:logistic",
             tree_method= "hist",
             max_bin= 31,
             base_score=train.matrix %>% getinfo("label") %>% mean(),
             eta= 0.04,
             nrounds= 500, 
             colsample_bytree= 0.6,
             stratified=TRUE,
             maximize = TRUE,
             #eval_metric='auc',
             
             evals=list(pruebaA=validate.matrix),
             #feval<-testFunc,
             
             feval=testFunc
             #obj = logregobj,
             
             #metrics='auc'
)
# 
# 
# 
# 
# modelo = xgb.train( 
#     data= dgeneracion,
#     objective= "binary:logistic",
#     tree_method= "hist",
#     max_bin= 31,
#     base_score= mean( getinfo(dgeneracion, "label") ),
#     eta= 0.04,
#     nrounds= 300, 
#     colsample_bytree= 0.6
# )
# 
# 
# globalenv()$log.debug('predicting...')
# 
# #aplico a los datos de aplicacion, que NO TIENE CLASE
# daplicacion  <-   xgb.DMatrix( data  = data.matrix( dataset_aplicacion[ , !c("id_cliente"), with=FALSE]) )
# 
# 
# #aplico el modelo a datos nuevos
# aplicacion_prediccion  <- predict(  modelo, daplicacion )
# 
# #uno las columnas de numero_de_cliente y la probabilidad recien calculada
# prediccion_final  <-  cbind(  dataset_aplicacion[ ,c("id_cliente")], aplicacion_prediccion )
# 
# #le doy nombre a las columnas
# colnames( prediccion_final )  <-  c( "id_cliente", "prob_positivo" )
# 
# 
# globalenv()$log.debug('outputting data...')
# 
# #Genero las TRES salidas
# #grabo todas las probabilidad, simplemente para tenerlo
# fwrite( prediccion_final[ order( -prob_positivo) ], 
#         file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_probabilidades.txt"), 
#         sep= "\t", 
#         eol= "\r\n")
# 
# #Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
# #me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
# fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "id_cliente" ] ), 
#         file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_entregar.txt"),
#         col.names=FALSE, 
#         sep= "\t", 
#         eol= "\r\n")
# 
# #grabo la importancia de las variables
# write.table(  xgb.importance( model = modelo )
#               , file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_importancia.txt")
#               , sep= "\t"
#               , eol= "\r\n"
# )
# 
# globalenv()$log.debug('lineaDeMuerte original done')