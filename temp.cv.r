
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




datasets <- ( globalenv()$get.train.df() %>% split.train.test.df(0.7, clase) )
train.df <- globalenv()$DfHolder$new(datasets$train)
validate.df <- globalenv()$DfHolder$new(datasets$test)


set.seed( 102191999 ) #mi querida random seed, para que las corridas sean reproducibles


globalenv()$log.debug('training...')


testFunc <- function(preds, dtrain)
{
    return( list(
        metrics='suboMetrics',
        value=globalenv()$score.prediction(preds, dtrain %>% getinfo('label')) ))
}


aa <- xgb.cv(data=train.df$as.xgb.train(),
             nfold=5,
             objective= "binary:logistic",
             tree_method= "hist",
             max_bin= 31,
             base_score=train.df$mean,
             eta= 0.04,
             nrounds= 300, 
             colsample_bytree= 0.6,
             stratified=TRUE,
             maximize = TRUE,
             #eval_metric='auc',
             
             evals=list(pruebaA=validate.df$as.xgb.train()),
             feval=testFunc
)
