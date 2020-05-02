
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


xg.params_400 <- list(eta=0.025, colsample_bytree=0.29, nrounds=400)
lg.params_400 <- list(learning_rate=0.015, feature_fraction=0.33, max_depth=7, nrounds=400)



df.train <- get.train.df()
df.train.fe <- df.train %>% enrich.fe.std()


df.test.predict <- get.predict.full.df()
df.test.predict.fe <- df.test.predict %>% enrich.fe.std()


full.calc <- function( seed )
{
    xgbWokflow <- XgBoostWorkflow$new( full.df=df.train,
                                       split.func=NA,
                                       params=def.params,
                                       train.seed=seed
    )
    xgbWokflow$train()
    xgbWokflow$test.df <- DfHolder$new(df.test.predict)

    
    return( xgbWokflow$calc_score() )
}



calc.combined <- function( seed )
{
    lgb <- LgbWorkflow$new( full.df=df.train.fe,
                                   split.func=NA,
                                   params=lg.params_400,
                                   train.seed=seed
    )
    lgb$train()
    lgb$test.df <- DfHolder$new(df.test.predict.fe)
    
    xgb <- XgBoostWorkflow$new( full.df=df.train.fe,
                                       split.func=NA,
                                       params=xg.params_400,
                                       train.seed=seed
    )
    xgb$train()
    xgb$test.df <- DfHolder$new(df.test.predict.fe)

    combined <- data.table(
        xgb=xgb$get.prediction.probs(),
        lgb=lgb$get.prediction.probs()
    ) %>% mutate( avg=map2_dbl( xgb, lgb, function(xgb,lgb) (xgb*0.7+lgb*0.3) ) )
    
    
    globalenv()$score.prediction(combined$avg, lgb$test.df$as.results(), 0.025 )
    
}



aa <- globalenv()$get.seeds( 5 )


aa <- aa %>% mutate( base=map( seed, function(seed) full.calc(seed=seed) ) )


aa <- aa %>% mutate( score.combined=map( seed, function(seed) calc.combined(seed=seed) ) )


aa <- aa %>% unnest()



bb <- aa %>% mutate_at( .vars=vars(matches('score')),
                  .funs=list(~ ./base*100 )  )
