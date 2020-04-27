
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


train.df <- ( globalenv()$get.train.df() %>% enrich.fe.std() %>% split.train.test.df(0.7) )$train

params<-list(nrounds          = 600,
             eta              = 0.176,
             colsample_bytree = 0.143)


rawResults <- get.seeds(5) %>%
    mutate( result=map( seed, function(seed) XgBoostCvWorkflow$new(params=params, train.df=train.df)$go(seed)$cv.result$evaluation_log ))


avgResults <- rawResults %>%
    unnest( result ) %>%
    group_by(iter) %>%
    summarize(mean=mean(test_suboMetrics_mean), sd=mean(test_suboMetrics_std))


print( avgResults %>% ggplot(aes(x=iter) ) + #, color=who
           geom_line( aes(y=mean, color='blue' )) +
           geom_smooth( aes(y=sd, color='red' )) +
           xlim(100,600) +
           ylim(59,61) +
#           ylim(5,10) +    
           ggtitle('gain and sd vs iteration')
)



top5Values <- (avgResults %>% arrange(mean) %>% tail(5))

bestIterationAvg <- top5Values$iter %>% mean()
bestIterationGain <- top5Values$mean %>% mean()
bestIterationSd <- top5Values$iter %>% sd()









