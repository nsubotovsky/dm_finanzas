
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
library(ggplot2)



datasets <- ( globalenv()$get.train.df() %>% split.train.test.df(0.7, clase) )

train.df <- globalenv()$DfHolder$new(datasets$train)
validate.df <- globalenv()$DfHolder$new(datasets$test)



testFunc <- function(preds, dtrain)
{
    return( list(
        metrics='suboMetrics',
        value=globalenv()$score.prediction(preds, dtrain %>% getinfo('label')) ))
}




run.cv <- function(seed)
{
    set.seed( seed )
    aa <- xgb.cv(data=train.df$as.xgb.train(),
                 nfold=5,
                 objective= "binary:logistic",
                 tree_method= "hist",
                 max_bin= 31,
                 base_score=train.df$mean,
                 eta= 0.0245,
                 nrounds= 600, 
                 colsample_bytree= 0.212,
                 stratified=TRUE,
                 maximize = TRUE,
                 feval=testFunc
    )
    return(aa$evaluation_log)
}

seedVector <- data.table( seed=c(12345,154784,12369,12,45,78,65,78451,45,78543,354,7897))

rawResults <- seedVector %>% mutate( result=map( seed, run.cv ))


avgResults <- rawResults %>%
    unnest( result ) %>%
    group_by(iter) %>%
    summarize(mean=mean(test_suboMetrics_mean), sd=mean(test_suboMetrics_std))


print( avgResults %>% ggplot(aes(x=iter) ) + #, color=who
           geom_smooth( aes(y=mean, color='blue' )) +
           geom_smooth( aes(y=sd, color='red' )) +
           xlim(100,600) +
           ylim(56,58) +
#           ylim(5,10) +    
           ggtitle('gain and sd vs iteration')
)


top5Values <- (avgResults %>% arrange(mean) %>% tail(5))

bestIterationAvg <- top5Values$iter %>% mean()
bestIterationGain <- top5Values$mean %>% mean()
bestIterationSd <- top5Values$iter %>% sd()









