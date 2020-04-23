
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

df <- get.train.df()
dataset <- get.train.df() %>% enrich.fe.std()
mega.df <- dataset %>% enrich.fe.extended()



autoTestAndScore <- function( full.df, seed=102191, partition=0.7, cutoff=0.025 )
{
    set.seed( seed )
    
    datasets <- ( full.df %>% split.train.test.df(partition, clase) )
    train.df <- globalenv()$DfHolder$new(datasets$train)
    validate.df <- globalenv()$DfHolder$new(datasets$test)
    

    globalenv()$log.debug('training...')
    model = xgb.train( 
        data= train.df$as.xgb.train(),
        objective= "binary:logistic",
        tree_method= "hist",
        max_bin= 31,
        base_score= train.df$mean,
        eta= 0.04,
        nrounds= 300, 
        colsample_bytree= 0.6
    )
    
    
    predictions <- model %>% predict( validate.df$as.xgb.predict())
    score <- globalenv()$score.prediction(predictions, validate.df$as.results(), cutoff )
    return(score)
}

seed = 102191

score.no.fe <- autoTestAndScore(df, seed=seed)
score.std.fe <- autoTestAndScore(dataset, seed=seed)
score.mega.fe <- autoTestAndScore(mega.df, seed=seed)



