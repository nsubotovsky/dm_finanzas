
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

df.fe.non <- get.train.df()
df.fe.std <- df.fe.non %>% enrich.fe.std()
df.fe.ext <- df.fe.std %>% enrich.fe.extended()



autoTestAndScore <- function( full.df, seed=102191, partition=0.7, cutoff=0.025 )
{
    set.seed( seed )
    
    datasets <- ( full.df %>% split.train.test.df(partition, clase, seed=NA) )
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


a <- globalenv()$get.seeds( 20 )

a <- a %>% mutate( score.base=map( seed, function(seed) autoTestAndScore(df.fe.non, seed=seed) ) )
a <- a %>% mutate( score.fe.std=map( seed, function(seed) autoTestAndScore(df.fe.std, seed=seed) ) )
a <- a %>% mutate( score.fe.ext=map( seed, function(seed) autoTestAndScore(df.fe.ext, seed=seed) ) )


b <- a %>% mutate( score.fe.std=map2( score.base, score.fe.std, function( a,b ) b/a*100 ) )
b <- b %>% mutate( score.fe.ext=map2( score.base, score.fe.ext, function( a,b ) b/a*100 ) )
b <- b %>% as.data.table( b %>% lapply(unlist) )


