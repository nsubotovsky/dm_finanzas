
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


datasets <- ( globalenv()$get.train.df() %>% split.train.test.df(0.7, clase) )
train.df <- globalenv()$DfHolder$new(datasets$train)
validate.df <- globalenv()$DfHolder$new(datasets$test)


set.seed( 102191 )


globalenv()$log.debug('training...')
modelo_original = xgb.train( 
    data= train.df$as.xgb.train(),
    objective= "binary:logistic",
    tree_method= "hist",
    max_bin= 31,
    base_score=train.df$mean,
    eta= 0.04,
    nrounds= 300, 
    colsample_bytree= 0.6
)


globalenv()$log.debug('training...')
modelo_tuned = xgb.train( 
    data= train.df$as.xgb.train(),
    objective= "binary:logistic",
    tree_method= "hist",
    max_bin= 31,
    base_score=train.df$mean,
    eta= 0.0245,
    nrounds= 450, 
    colsample_bytree= 0.212
)




predictions <- modelo_original %>% predict( validate.df$as.xgb.predict())
score <- globalenv()$score.prediction(predictions, validate.df$as.results() )
print(score)



predictions <- modelo_tuned %>% predict( validate.df$as.xgb.predict())
score <- globalenv()$score.prediction(predictions, validate.df$as.results() )
print(score)




