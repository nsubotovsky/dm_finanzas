########### Boilerplate code to auto-load correct module ##############

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

## source code and run it ##
source( get.code.dir('lineademuerte_recuperatorio.r'))
#source( get.code.dir('prepare_fastdfs.r'))
run.this()

