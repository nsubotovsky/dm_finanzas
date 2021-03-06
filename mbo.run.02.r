##### Failed experiment to try and parallelize MBO #########

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



library(ggplot2)
library(mlrMBO)
library(tidyverse)




c(tdf, test.df) %<-% ( globalenv()$get.train.df() %>% split.train.test.df(0.7) )
train.df <- globalenv()$DfHolder$new( tdf )


set.seed(123457)


testFunc <- function(preds, dtrain)
{
    return( list(
        metrics='suboMetrics',
        value=globalenv()$score.prediction(preds, dtrain %>% getinfo('label')) ))
}


autoTestAndScore <- function( x )
{
    # full.df
    # seed=102191
    # partition=0.7
    # cutoff=0.025    
    
    #set.seed( seed )
    
    
    globalenv()$log.debug('training...')
    cv.result = xgb.cv( 
        data= train.df$as.xgb.train(),
        nfold=5,
        objective= "binary:logistic",
        tree_method= "hist",
        max_bin= 31,
        base_score= train.df$mean,
        eta= x$eta,
        nrounds= 500,
        colsample_bytree= x$colsample_bytree,
        print_every_n = 10L,
        stratified=TRUE,
        maximize = TRUE,
        feval=testFunc        
    )
    
    
    mean_5 <- cv.result$evaluation_log %>% select( test_suboMetrics_mean ) %>% arrange( test_suboMetrics_mean ) %>% tail(5)
    return( mean_5$test_suboMetrics_mean %>% mean() )
}


obj.fun <- makeSingleObjectiveFunction(
    name='xgb_hiperparams',
    fn=autoTestAndScore,
    par.set=makeParamSet(
        makeNumericParam('eta', lower=.01, upper=.1),
        makeNumericParam('colsample_bytree', lower=.1, upper=1)
    ),
    has.simple.signature = FALSE,
    #global.opt.params = list(x=-2, y=2),
    minimize=FALSE
)       


print(obj.fun)



mbo.file <- globalenv()$get.data.dir( 'mbo_02', 'mbo.txt', auto.create=TRUE )

ctrl = makeMBOControl(propose.points = 4,
                      save.on.disk.at.time=60,
                      save.file.path = mbo.file
) %>%
    setMBOControlTermination( iters = 20L) %>%
    setMBOControlInfill(
        crit = makeMBOInfillCritCB(),
        opt = "focussearch", opt.focussearch.points = 20L
    ) %>%
    setMBOControlMultiPoint(method = "cb")

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)


library(parallelMap)
parallelStartMulticore(cpus = 56, show.info = TRUE)


if (file.exists(mbo.file)==TRUE) {run <- mboContinue(mbo.file)
} else {run <- mbo(fun=obj.fun, design = design, control=ctrl)}

parallelStop()


interpert.mbo <- function(file)
{
    mbo.data <- get(load(file))
    return(mbo.data$opt.path$env$path)
}


print(run)

