
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

library( "data.table" )
library( "xgboost" )



workingdir <- globalenv()$get.data.dir('recuperatorio')
setwd(workingdir)

globalenv()$log.debug('Working dir set to {workingdir}', environment())  


full.df <- globalenv()$get.train.df()


c(train.df, test.df) %<-% ( full.df %>% split.train.test.df(0.7, clase) )




train.df.xgbmatrix <- train.df %>% globalenv()$as.xgbMatrix()


set.seed( 102191 )

globalenv()$log.debug('training...')
modelo_300 = xgb.train( 
    data= train.df.xgbmatrix,
    objective= "binary:logistic",
    tree_method= "hist",
    max_bin= 31,
    base_score= mean( getinfo(train.df.xgbmatrix, "label") ),
    eta= 0.04,
    nrounds= 300, 
    colsample_bytree= 0.6
)


globalenv()$log.debug('training...')
modelo_400 = xgb.train( 
    data= train.df.xgbmatrix,
    objective= "binary:logistic",
    tree_method= "hist",
    max_bin= 31,
    base_score= mean( getinfo(train.df.xgbmatrix, "label") ),
    eta= 0.04,
    nrounds= 350, 
    colsample_bytree= 0.6
)



aa <- test.df %>% globalenv()$prepare.class.df()


prediction_300 <- modelo_300 %>% predict( data.matrix( aa %>% select( -clase01, -id_cliente ) ) )

prediction_400 <- modelo_400 %>% predict( data.matrix( aa %>% select( -clase01, -id_cliente ) ) )




score.prediction( prediction_300, aa$clase01 )

score.prediction( prediction_400, aa$clase01 )


