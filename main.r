########### Boilerplate code to auto-load correct module ##############

rm(list = ls())
gc()

start.modules <- c("~/dm_finanzas/loader.recuperatorio.r", # linux location
                   "C:/Users/Luxor/Documents/GitHub/Test_01/dm_finanzas/loader.recuperatorio.r" # windows location
)


load.modules <- function( modulesPath )
{
  for( modulePath in modulesPath)
  {
    tryCatch({
      print(paste0('Attempting to load ', modulePath))
      source(modulePath)
      print('Success')
    },
    warning=function(...){print('Module not found')},
    error=function(...){print('Module not found')}
    )
  }
}

## load helper libs ##
load.modules(start.modules)

## source code and run it ##
source( get.code.dir('lineademuerte_recuperatorio.r'))
run.this()

