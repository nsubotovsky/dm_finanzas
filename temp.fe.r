
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

def.params <- list(eta=0.04, colsample_bytree=0.6, nrounds=300)



xg.params_200 <- list(eta=0.025, colsample_bytree=0.29, nrounds=200)
xg.params_300 <- list(eta=0.025, colsample_bytree=0.29, nrounds=300)
xg.params_400 <- list(eta=0.025, colsample_bytree=0.29, nrounds=400)

lg.params_200 <- list(learning_rate=0.015, feature_fraction=0.33, max_depth=7, nrounds=200)
lg.params_300 <- list(learning_rate=0.015, feature_fraction=0.33, max_depth=7, nrounds=300)
lg.params_400 <- list(learning_rate=0.015, feature_fraction=0.33, max_depth=7, nrounds=400)



full.calc <- function( seed, full.df, params )
{
    xgbWokflow <- XgBoostWorkflow$new( full.df=full.df,
                                       split.func=partial( globalenv()$standard.split, frac=0.7, seed=seed ),
                                       params=params,
                                       train.seed=seed
    )
    xgbWokflow$train()
    
    return( xgbWokflow$calc_score() )
}


full.calc.lg <- function( seed, full.df, params )
{
    xgbWokflow <- LgbWorkflow$new( full.df=full.df,
                                       split.func=partial( globalenv()$standard.split, frac=0.7, seed=seed ),
                                       params=params,
                                       train.seed=seed
    )
    xgbWokflow$train()
    
    return( xgbWokflow$calc_score() )
}



calc.combined <- function( seed, full.df )
{
    lgb <- LgbWorkflow$new( full.df=full.df,
                                   split.func=partial( globalenv()$standard.split, frac=0.7, seed=seed ),
                                   params=lg.params_400,
                                   train.seed=seed
    )
    lgb$train()
    
    xgb <- XgBoostWorkflow$new( full.df=full.df,
                                       split.func=partial( globalenv()$standard.split, frac=0.7, seed=seed ),
                                       params=xg.params_400,
                                       train.seed=seed
    )
    xgb$train()

    combined <- data.table(
        xgb=xgb$get.prediction.probs(),
        lgb=lgb$get.prediction.probs()
    ) %>% mutate( avg=map2_dbl( xgb, lgb, function(xgb,lgb) (xgb*0.7+lgb*0.3) ) )
    
    
    globalenv()$score.prediction(combined$avg, lgb$test.df$as.results(), 0.025 )
    
}



df.fe.non <- get.train.df()
df.fe.std <- df.fe.non %>% enrich.fe.std()


aa <- globalenv()$get.seeds( 5 )


aa <- aa %>% mutate( base=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.non, params=def.params) ) )

#aa <- aa %>% mutate( score.xg.200=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.std, params=xg.params_200) ) )
#aa <- aa %>% mutate( score.xg.300=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.std, params=xg.params_300) ) )
aa <- aa %>% mutate( score.xg.400=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.std, params=xg.params_400) ) )



#aa <- aa %>% mutate( score.lg.200=map( seed, function(seed) full.calc.lg(seed=seed, full.df=df.fe.std, params=lg.params_200) ) )
#aa <- aa %>% mutate( score.lg.300=map( seed, function(seed) full.calc.lg(seed=seed, full.df=df.fe.std, params=lg.params_300) ) )
aa <- aa %>% mutate( score.lg.400=map( seed, function(seed) full.calc.lg(seed=seed, full.df=df.fe.std, params=lg.params_400) ) )


aa <- aa %>% mutate( score.combined=map( seed, function(seed) calc.combined(seed=seed, full.df=df.fe.std) ) )




aa <- aa %>% unnest()



bb <- aa %>% mutate_at( .vars=vars(matches('score')),
                  .funs=list(~ ./base*100 )  )
