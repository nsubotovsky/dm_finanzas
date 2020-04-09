#limpio la memoria
#rm(list=ls())
#gc()

run.this<- function()
{
    globalenv()$log.debug('transcribing DFs')
    
    library( "data.table" )
    library("stringr")
  
    alumno_apellido <- "subotovsky"
    
    #cargo los datasets
    
    workingdir <- globalenv()$get.data.dir('recuperatorio')
    setwd(workingdir)
    
    globalenv()$log.debug('Working dir set to {workingdir}', environment())  
    
    
    globalenv()$log.debug('transcribing datasets...')
    
    filenames = c(paste0( alumno_apellido, "_generacion.txt"),
                  paste0( alumno_apellido, "_aplicacion.txt"))
    
    for (filename in filenames)
    {
        globalenv()$log.debug('processing {filename}', environment())
        dataset <- fread(filename)
        dataset %>% globalenv()$fst.write( filename %>% str_replace('.txt', '.rds') )
    }
    globalenv()$log.debug('transcribing done')

}