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

df <- (get.train.df() %>% split.train.test.df( seed=get.seeds(2)$seed %>% last() ))$train %>% enrich.fe.std()


a <- XgbMboOptmizer$new(dataframe=df,
                        output.folder='mbo_test_06_bis_enrich',
                        parameters.set=makeParamSet(
                            makeNumericParam('eta', lower=.001, upper=.1),
                            makeNumericParam('colsample_bytree', lower=.05, upper=1)
                        ),
                        iterations.per.point=4,
                        total.points=100)


a$go()

a$print.mbo(55)