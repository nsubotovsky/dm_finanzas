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




c(tdf, test.df) %<-% ( globalenv()$get.train.df() %>% split.train.test.df(0.7, clase) )
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


ctrl = makeMBOControl(propose.points = 1) %>%
    setMBOControlTermination( iters = 50L) %>%
    setMBOControlInfill(
        crit = makeMBOInfillCritEI(),
        opt = "focussearch", opt.focussearch.points = 20L
    )

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)


run <- mbo(fun=obj.fun, design = design, control=ctrl)



print(run)



########## Results here ##############


# 
# makeNumericParam('eta', lower=.01, upper=.1),
# makeNumericParam('colsample_bytree', lower=.1, upper=1)
# 
# 
# [mbo] 50: eta=0.0224; colsample_bytree=0.591 : y = 62.3 : 81.1 secs : infill_ei
# Recommended parameters:
#     eta=0.0224; colsample_bytree=0.591
# Objective: y = 62.276
# 
# Optimization path
# 6 + 50 entries in total, displaying last 10 (or less):
#     eta colsample_bytree        y dob eol error.message exec.time          ei error.model train.time prop.type
# 47 0.07183819        0.6614048 58.40470  41  NA          <NA>     74.87 -0.13085180        <NA>       0.16 infill_ei
# 48 0.06257542        0.7828236 58.30573  42  NA          <NA>     75.78 -0.01513567        <NA>       0.11 infill_ei
# 49 0.09974718        0.4679222 56.89648  43  NA          <NA>     83.00 -0.01400573        <NA>       0.15 infill_ei
# 50 0.02441558        0.9257518 58.48519  44  NA          <NA>     78.77 -0.02899803        <NA>       0.13 infill_ei
# 51 0.06316242        0.2725764 60.44600  45  NA          <NA>     77.65 -0.01260675        <NA>       0.09 infill_ei
# 52 0.01581581        0.6479078 60.40245  46  NA          <NA>     79.78 -0.01324263        <NA>       0.11 infill_ei
# 53 0.04864037        0.7694360 57.83730  47  NA          <NA>     79.75 -0.01378749        <NA>       0.09 infill_ei
# 54 0.04024448        0.6924647 59.37982  48  NA          <NA>     77.87 -0.01536868        <NA>       0.17 infill_ei
# 55 0.08476740        0.3674733 57.24352  49  NA          <NA>     76.73 -0.01238293        <NA>       0.14 infill_ei
# 56 0.02244481        0.5910988 62.27618  50  NA          <NA>     81.07 -0.01189337        <NA>       0.13 infill_ei
# propose.time       se     mean
# 47         0.03 1.026247 61.30499
# 48         0.05 1.605026 58.94463
# 49         0.04 1.590838 58.93132
# 50         0.04 1.393255 59.79752
# 51         0.05 1.585517 58.88170
# 52         0.04 1.584807 58.91238
# 53         0.03 1.582780 58.94103
# 54         0.05 1.559548 59.05980
# 55         0.03 1.561531 58.92872
# 56         0.04 1.563567 58.90054
# 
# 
# > run$x
# $eta
# [1] 0.02244481
# 
# $colsample_bytree
# [1] 0.5910988
# 
# > run$y
# [1] 62.27618
# 
# 
# > run$opt.path$par.set
# Type len Def      Constr Req Tunable Trafo
# eta              numeric   -   - 0.01 to 0.1   -    TRUE     -
#     colsample_bytree numeric   -   -    0.1 to 1   -    TRUE     -
#     

