################ Experiment - hyperoptimize for extended df ######################

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





train.df <- globalenv()$get.train.df() %>% enrich.fe.std()


set.seed( globalenv()$MASTER_SEED )

# split train - test
c(tdf, test.df) %<-% ( train.df %>% split.train.test.df(0.7, seed=NA) )
train.df <- globalenv()$DfHolder$new( tdf )


##### this tdf (test data frame) is not being used in this script directly ###
##### need to test later on.


testFunc <- function(preds, dtrain)
{
    return( list(
        metrics='suboMetrics',
        value=globalenv()$score.prediction(preds, dtrain %>% getinfo('label')) ))
}


autoTestAndScore <- function( x )
{
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



mbo.file <- globalenv()$get.data.dir( 'mbo_03_fe', 'mbo.txt', auto.create=TRUE )

ctrl = makeMBOControl(propose.points = 1,
                      save.on.disk.at.time=60,
                      save.file.path = mbo.file
                      ) %>%
    setMBOControlTermination( iters = 600L) %>%
    setMBOControlInfill(
        crit = makeMBOInfillCritEI(),
        opt = "focussearch", opt.focussearch.points = 20L
    )

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)



if (file.exists(mbo.file)==TRUE) {run <- mboContinue(mbo.file)
} else {run <- mbo(fun=obj.fun, design = design, control=ctrl)}


interpert.mbo <- function(file)
{
    mbo.data <- get(load(file))
    return(mbo.data$opt.path$env$path)
}


print(run)


ggplot(interpert.mbo(mbo.file) %>% filter(y > 60), aes(x = eta, y = colsample_bytree, color=y))+ geom_point() + xlim(0,.1) + ylim(0,1) + scale_color_gradient(low="blue", high="red")

#
#
#  Results for run...
#
# > print(run)
# Recommended parameters:
#     eta=0.0579; colsample_bytree=0.191
# Objective: y = 61.783
# 
# Optimization path
# 6 + 600 entries in total, displaying last 10 (or less):
#     eta colsample_bytree        y dob eol error.message exec.time           ei error.model train.time prop.type propose.time
# 597 0.05039946        0.4144321 54.33529 591  NA          <NA>    48.331 -0.001891943        <NA>      9.695 infill_ei        0.136
# 598 0.07137908        0.1880172 55.73662 592  NA          <NA>    47.202 -0.001891917        <NA>      7.918 infill_ei        0.137
# 599 0.01806925        0.3960633 55.67461 593  NA          <NA>    48.869 -0.001875873        <NA>      7.474 infill_ei        0.136
# 600 0.07201178        0.7180483 56.99677 594  NA          <NA>    48.605 -0.001860280        <NA>      8.664 infill_ei        0.137
# 601 0.05909428        0.1856778 55.98733 595  NA          <NA>    49.213 -0.003565353        <NA>      9.695 infill_ei        0.142
# 602 0.06907221        0.3835352 59.91555 596  NA          <NA>    47.439 -0.001829380        <NA>      9.065 infill_ei        0.140
# 603 0.05782309        0.3988809 58.39810 597  NA          <NA>    47.674 -0.001874435        <NA>      5.869 infill_ei        0.134
# 604 0.07157012        0.3794043 57.42429 598  NA          <NA>    48.647 -0.001878765        <NA>     10.390 infill_ei        0.134
# 605 0.07838905        0.9248204 53.82464 599  NA          <NA>    48.088 -0.001868093        <NA>      6.210 infill_ei        0.134
# 606 0.06422598        0.5258294 55.15999 600  NA          <NA>    47.494 -0.001878361        <NA>      5.794 infill_ei        0.148
# se     mean
# 597 1.819706 56.85866
# 598 1.821107 56.85443
# 599 1.820157 56.85256
# 600 1.819273 56.85060
# 601 1.537309 58.01233
# 602 1.816594 56.84940
# 603 1.819374 56.85450
# 604 1.818950 56.85706
# 605 1.817590 56.85800
# 606 1.820262 56.85298


