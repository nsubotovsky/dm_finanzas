
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




calc.xgb <- function( seed, train.df )
{
    xgbWokflow <- XgBoostWorkflow$new( full.df=train.df,
                                       split.func=NA,
                                       params=list(eta=0.025, colsample_bytree=0.29, nrounds=400),
                                       train.seed=seed
    )
    xgbWokflow$train()
    
    return( xgbWokflow )
}


calc.lgb <- function( seed, train.df )
{
    lgbWokflow <- LgbWorkflow$new( full.df=train.df,
                                   split.func=NA,
                                   params=list(learning_rate=0.015, feature_fraction=0.33, max_depth=7, nrounds=400),
                                   train.seed=seed
    )
    lgbWokflow$train()
    
    return( lgbWokflow )
}



calc.combined <- function( seed, train.df, predict.df, cutoff=0.025 )
{
    train.df.fe <- train.df %>% enrich.fe.std()
    
    lgb <- calc.lgb( seed, train.df.fe )
    xgb <- calc.xgb( seed, train.df.fe )

    predict.df.fe <- predict.df %>% enrich.fe.std()

    combined <- data.table(
        xgb=xgb$get.prediction.probs( predict.df.fe ),
        lgb=lgb$get.prediction.probs( predict.df.fe )
    ) %>% mutate( avg=map2_dbl( xgb, lgb, function(xgb,lgb) (xgb*0.7+lgb*0.3) ) )
    

    predictions <- data.table( id_cliente=predict.df.fe$id_cliente, prob=combined$avg ) %>%
        filter( prob>cutoff ) %>%
        arrange(-prob)

    return(predictions$id_cliente)
}


seed <- 123457

ids <- calc.combined( seed=seed,
               train.df=get.train.df(),
               predict.df=get.predict.df())


fwrite( ids, 
        file= get.data.dir('candidate_02', 'subotovsky_entregar.txt', auto.create=T), 
        sep= "\t", 
        eol= "\r\n")
