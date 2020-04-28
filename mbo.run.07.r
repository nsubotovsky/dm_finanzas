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

df <- (get.train.df() %>% split.train.test.df())$train %>% enrich.fe.std()


a <- LgbMboOptmizer$new(dataframe=df,
                        nrounds=500,
                        output.folder='mbo_07_lgbm',
                        parameters.set=makeParamSet(
                            makeNumericParam('learning_rate', lower=.001, upper=.4),
                            makeNumericParam('feature_fraction', lower=.05, upper=1),
                            makeIntegerParam('max_depth', lower=4, upper=8)
                        ),
                        iterations.per.point=4,
                        total.points=100)


a$go()

a$print.mbo(55)