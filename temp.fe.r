
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

full.calc <- function( seed, full.df )
{
    xgbWokflow <- XgBoostWorkflow$new( full.df=full.df,
                                       split.func=partial( globalenv()$standard.split, frac=0.7, seed=seed ),
                                       params=list(eta=0.04, colsample_bytree=0.6, nrounds=300),
                                       train.seed=seed
    )
    xgbWokflow$train()
    
    return( xgbWokflow$calc_score() )
}



df.fe.non <- get.train.df()
df.fe.std <- df.fe.non %>% enrich.fe.std()
df.fe.ext <- df.fe.std %>% enrich.fe.extended()



aa <- globalenv()$get.seeds( 3 )
aa <- aa %>% mutate( score.base=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.non) ) )
aa <- aa %>% mutate( score.fe.std=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.std) ) )
aa <- aa %>% mutate( score.fe.ext=map( seed, function(seed) full.calc(seed=seed, full.df=df.fe.ext) ) )
aa <- aa %>% unnest()


b <- aa %>% mutate( score.fe.std=map2( score.base, score.fe.std, function( a,b ) b/a*100 ) )
b <- b %>% mutate( score.fe.ext=map2( score.base, score.fe.ext, function( a,b ) b/a*100 ) )
b <- b %>% as.data.table( b %>% lapply(unlist) )
b <- b %>% unnest()

