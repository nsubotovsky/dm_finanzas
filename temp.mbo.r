

library(ggplot2)
library(mlrMBO)
library(tidyverse)


set.seed(123456)

c(tdf, test.df) %<-% ( globalenv()$get.train.df() %>% split.train.test.df(0.7) )
train.df <- globalenv()$DfHolder$new( tdf )



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