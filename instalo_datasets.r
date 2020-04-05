#Objetivo,  bajar el dataset del DROPBOX  y a partir de ahi generar archivos derivados

#limpio la memoria
rm( list=ls() )
gc()


library( "data.table" )


kdirectory_datasets_bucketOri  <-  "~/cloud/cloud1/datasetsOri/"
kdirectory_datasets_bucket     <-  "~/cloud/cloud1/datasets/"


kcampos_separador        <-  "\t"
karchivo_entrada         <-  "paquete_premium.txt"
karchivo_entrada_zip     <-  "paquete_premium.zip"

kcampo_foto              <-  "foto_mes"

kdropbox_link            <-  "https://www.dropbox.com/s/azteom7b4jrmqgs/paquete_premium.zip?dl=0"

#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------

fguardar_foto  = function( pfoto_mes, pprefijo,  psufijo, pdataset )
{
  
  archivo_salida_mes <- paste( pprefijo,  pfoto_mes,  psufijo, sep="" )

  fwrite(  pdataset[ get(kcampo_foto) == pfoto_mes], 
           file=archivo_salida_mes , 
           sep=kcampos_separador, na="", row.names=FALSE,
           nThread=1) 
}
#------------------------------------------------------

dir.create( "~/cloud/cloud1/" )

dir.create( "~/cloud/cloud1/work/" )
dir.create( "~/cloud/cloud1/datasetsOri/" )
dir.create( "~/cloud/cloud1/datasets/" )


#creo la carpeta en el bucket de los datasets
dir.create( kdirectory_datasets_bucketOri )


#bajo el archivo del dropbox
setwd( kdirectory_datasets_bucketOri )
system(  paste( "wget --max-redirect=20 -O ", karchivo_entrada_zip, " ",  kdropbox_link, sep=""  ))



#lectura rapida del dataset  usando fread  de la libreria  data.table
setwd( kdirectory_datasets_bucketOri )
dataset <- fread(  cmd=paste( "gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador ) 

nrow( dataset )
ncol( dataset )

#grabo el dataset completo en la carpeta datasetsOri
setwd( kdirectory_datasets_bucketOri )
fwrite( dataset,  file="paquete_premium.txt", sep=kcampos_separador, na="", row.names=FALSE, nThread=1) 



#extraigo los meses tal cual estan
setwd( kdirectory_datasets_bucket )
fguardar_foto(  "201902", "",  ".txt", dataset )
fguardar_foto(  "201904", "",  ".txt", dataset )



#paso a fechas relativas
dataset[ , Master_Fvencimiento   := fdias_entre( get(kcampo_foto), Master_Fvencimiento )]
dataset[ , Master_Finiciomora    := fdias_entre( get(kcampo_foto), Master_Finiciomora )]
dataset[ , Master_fultimo_cierre := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[ , Master_fechaalta      := fdias_entre( get(kcampo_foto), Master_fechaalta )]
dataset[ , Visa_Fvencimiento     := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )]
dataset[ , Visa_Finiciomora      := fdias_entre( get(kcampo_foto), Visa_Finiciomora )]
dataset[ , Visa_fultimo_cierre   := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )]
dataset[ , Visa_fechaalta        := fdias_entre( get(kcampo_foto), Visa_fechaalta )]


#ordeno el dataset
setorder( dataset, foto_mes, numero_de_cliente )


#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   unique(dataset[ , get(kcampo_foto) ] )


setwd( kdirectory_datasets_bucket )
dir.create( "dias" )

lapply(  fotos_distintas,  fguardar_foto,  pprefijo="./dias/",  psufijo="_dias.txt",  pdataset=dataset ) 

#grabo el archivo completo
setwd( kdirectory_datasets_bucket )
fwrite( dataset,  file="paquete_premium_dias.txt", sep=kcampos_separador, na="", row.names=FALSE, nThread=1) 


#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


