
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

library(data.table)
library(xgboost)
library(ggplot2)
library(tictoc)






generate.model <- function( seed, train.df )
{
    tic(paste0('Training with seed ',seed))
    modelo = xgb.train( 
        data=train.df$as.xgb.train(),
        objective= "binary:logistic",
        tree_method= "hist",
        max_bin= 31,
        base_score=train.df$mean,
        eta= 0.04,
        nrounds= 300, 
        colsample_bytree= 0.6
    )
    toc()
    return(modelo)
}


gain.for.model <- function( model, validate.df )
{
    testvector = cutoffsTestVector
    predictions <- model %>% predict( validate.df$as.xgb.predict())
    return(testvector  %>% mutate( gain=map(cutoff, function(cutoff) globalenv()$score.prediction(predictions, validate.df$as.results(), cutoff )  ) ) )
}



mega.calc <- function( seed )
{
    set.seed(seed)
    
    #split datasets
    datasets <- ( globalenv()$get.train.df() %>% split.train.test.df(0.7) )
    train.df <- globalenv()$DfHolder$new(datasets$train)
    validate.df <- globalenv()$DfHolder$new(datasets$test)
    
    #train model
    model <- generate.model(seed, train.df)
    
    result <- model %>% gain.for.model( validate.df )
    
    return( result )
}


raw.seeds <- c(19381,19387,19391,19403,19417,19421,19423,19427,19429,19433,19441,19447,19457,19463,19469,19471,19477,19483,19489,19501,19507,
19531,19541,19543,19553,19559,19571,19577,19583,19597,19603,19609,19661,19681,19687,19697,19699,19709,19717,19727,19739,19751,
19753,19759,19763,19777,19793,19801,19813,19819
# 87509,87511,87517,87523,87539,87541,87547,87553,87557,87559,87583,87587,87589,87613,87623,87629,87631,87641,87643,87649,87671,
# 87679,87683,87691,87697,87701,87719,87721,87739,87743,87751,87767,87793,87797,87803,87811,87833,87853,87869,87877,87881,87887,
# 87911,87917,87931,87943,87959,87961,87973,87977,87991,88001,88003,88007,88019,88037,88069,88079,88093,88117,88129,88169,88177
)




seedVector <- data.table( seed=raw.seeds)
#seedVector <- data.table( seed=c(12345,154784,12369))
cutoffsTestVector <- data.table(cutoff=seq(.01,.05,.0002))


print('Calculating models...')
models <- seedVector %>% mutate( gain.vs.cutoff=map( seed, mega.calc ))


gain.vs.cutoff <- models %>%
    unnest(gain.vs.cutoff) %>%
    unnest(gain) %>%
    select(-seed) %>%
    group_by(cutoff) %>%
    summarize( avg.gain=mean(gain) )


print( gain.vs.cutoff %>% ggplot(aes(x=cutoff, y=avg.gain) ) + #, color=who
           geom_line(color='blue') +
           ggtitle('gain vs cutoff')
)



