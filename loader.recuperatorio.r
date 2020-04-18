library(fst, warn.conflicts = FALSE, quietly=TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(gsubfn, warn.conflicts = FALSE, quietly=TRUE)
library(data.table, warn.conflicts = FALSE, quietly=TRUE)
library(glue, warn.conflicts = FALSE, quietly=TRUE)
library(zeallot, warn.conflicts = FALSE, quietly=TRUE)
library(xgboost, warn.conflicts = FALSE, quietly=TRUE )


#### Check environment stuff here:
# http://adv-r.had.co.nz/Environments.html



###### Differentiation ######

get.os <- function()
{
   return(Sys.info()['sysname'])
}


get.env <- function()
{
    # windows environment
    if (get.os() == 'Windows') return(list(
                                  code_dir='C:/Users/Luxor/Documents/GitHub/Test_01/dm_finanzas',
                                  data_dir='C:/Users/Luxor/Documents/DM Finanzas/data.recuperatorio')
                                      )
    
    # linux environment
    else return(list(
                  code_dir='~/dm_finanzas',
                  data_dir='~/cloud/cloud1')
                )
}


get.data.dir <- function( ... )
{
    return( file.path( get.env()$data_dir, ...  ) )
}


get.code.dir <- function( ... )
{
    return( file.path( get.env()$code_dir, ...  ) )
}




################## metrics ################

get.roc.data <- function( predict, target )
{
    # calculo metricas
    roc_pred <- ROCR::prediction(predict, target, label.ordering = c(0, 1))
    
    # extraigo AUC
    auc <- unlist(ROCR::performance(roc_pred, "auc")@y.values)
    
    # extraigo ROC
    curve <- ROCR::performance(roc_pred,"tpr","fpr")
    curve.as.df <- data.table(x=curve@x.values[[1]], y=curve@y.values[[1]])
    colnames(curve.as.df) = c( 'x', 'y' )
    
    return(list( auc=auc, curve=curve.as.df))
}


get.gain.vs.prob <- function( prediction, target )
{
    prob.table <- data.table(prob=prediction, target=target) %>%
        arrange(-prob) %>%
        mutate(single.gain=ifelse(target==1, 19500, -500)) %>%
        mutate(gain=cumsum(single.gain)) %>%
        select(-single.gain)
    
    return(prob.table)
}


plot.gain.vs.prob <- function(df)
{
    ggplot( df, aes(x=prob, y=gain)) + geom_line() + xlim(0,0.1) +ylim(5000000,10000000)
}



func.gain <- function(probabilidades, clase, punto_corte = 0.025)
{
    return(sum(
            (probabilidades >= punto_corte) * ifelse(clase == "1", 19500, -500))
    )
}

calc.prob.cutoff <- function( gain )
{
    return( gain %>% select(prob, gain) %>% filter(gain == max(gain)) %>% as.data.table() )
}



################ Prepare training df #################

pick.months <- function(df, month.list)
{
    return( df %>% filter(foto_mes %in% ( month.list %>% sapply( get.month.at ) ) ) )
}


target.func.baja2 <- function(df)
{
   return( as.integer(ifelse(df$clase_ternaria == "BAJA+2", 1, 0)) )
}


target.func.baja12 <- function(df)
{
   return( as.integer(ifelse(df$clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0)) )
}


split.prepare.target <- function(df, target.func=target.func.baja2)
{
    df$clase_ternaria <- target.func(df)
    
    # split traing and target
    df.train <- df %>% select(-c("numero_de_cliente", "clase_ternaria"))
    df.target <- df$clase_ternaria
    
    return(list(train=df.train, target=df.target))
}


############# Scoring Daraframe ###############

score.prediction <- function( pred.probs, actual, cutoff=0.025, relative=TRUE )
{
    normalization <- ifelse( relative==TRUE, length(actual), 1 )
    score.df <- data.table( prob=pred.probs, target=actual ) %>%
        filter( prob>=cutoff ) %>%
        mutate( points=if_else( target==1 | target=='SI', 19500, -500) )
    
    score <- sum( score.df$points ) / normalization
    return(score)
}


################ Prepare training df - xgb specific #################



prepare.class.df <- function(df)
{
    log.debug('preparing dataframe...')
    return( df %>%
        mutate( clase01=if_else(clase=="SI", 1, 0) ) %>%
        select(-clase) )
}



# deprecate this (object below takes on this functionality)
as.xgbMatrix <- function(df)
{
    df.output.as.01 <- df %>% prepare.class.df()
    log.debug('converting to xgbMatrix...')
    xgb.matrix.df <- xgb.DMatrix(
        data  = data.matrix( df.output.as.01 %>% select( -id_cliente, -clase01 ) ),
        label = df.output.as.01$clase01
    )
    return(xgb.matrix.df)
}




DfHolder <- setRefClass('DfHolder',
    fields = c('df','mean'),
    
    methods=list(
      
        # initializer (load df, calculate)
        initialize = function( init.df )
        {
            log.debug('initializing matrix...')
          
            # si no tiene clase01, la creamos a partir de clase
            if (!('clase01' %in% (init.df %>% colnames())))
                if ('clase' %in% (init.df %>% colnames()))
                    init.df <- init.df %>% mutate(clase01=if_else(clase=="SI", 1, 0))
            
            log.debug('Removing class...')
            
            # sacamos 'clase' si lo tiene
            init.df <- init.df %>% select( -matches('^clase$') )
            
            log.debug('Calculating mean...')
            mean <<- init.df$clase01 %>% mean()
            
            df <<- init.df
            log.debug('Init DF done.')
        },
        
        # Make the df into an xgb trainable thing
        as.xgb.train = function()
        {
            return( xgb.DMatrix(
                data  = data.matrix( df %>% select( -id_cliente, -clase01 ) ),
                label = df$clase01
            ))
        },
        
        # Make the df into an xgb predictable thing
        as.xgb.predict = function()
        {
            return( xgb.DMatrix(data  = data.matrix( df %>% select( -id_cliente, -matches('^clase')))))
        },        
        
        # Get the results of the df
        as.results = function()
        {
            return( df$clase01 )
        },
        
        # Get the results of the df
        as.clients = function()
        {
            return( df$id_cliente )
        }
        
))




########### Loading and saving ###################

fst.read <- function(path)
{
    wd<-getwd()
    log.debug('reading df {path}  -- [{wd}]', environment() )
    return( fst::read.fst(path, as.data.table=T) )
}

fst.write <- function(df, path)
{
    wd<-getwd()
    log.debug('writing to df {path}  -- [{wd}]', environment())
    return( fst::write.fst(df, path, compress=100 ) )
}


get.train.df <- function()
{
    dfpath <- get.data.dir('recuperatorio', 'subotovsky_generacion.rds')
    return( fst.read(dfpath) )
}


get.predict.df <- function()
{
    dfpath <- get.data.dir('recuperatorio', 'subotovsky_aplicacion.rds')
    return( fst.read(dfpath) )
}


############ (sub)Sampling ###########################

sample.df <- function( df, fraction, ..., replace=F )
{
    # This! : https://dplyr.tidyverse.org/articles/programming.html
    group_var <- enquos(...)
    return( df %>% group_by( !!!group_var ) %>% sample_frac( fraction, replace=replace ) %>% ungroup() %>% as.data.table() )
}


split.train.test.df <- function( df, fraction, ... )
{
  train <- df %>% sample.df( fraction, ... )
  test <- df %>% anti_join(train, by='id_cliente')
  return (list(train=train, test=test))
}


summary.group <- function( df, ... )
{
    group_var <- enquos(...)
    return ( df %>% 
             group_by( !!!group_var ) %>%
             summarize(n=n()) %>%
             mutate(freq=n/sum(n)) )
}


############### Logging ################

log.debug <- function(msg, env=NULL)
{
    if (!is.null(env))
        print(paste(Sys.time(), glue_data(env, msg), sep=' : '))

    else
      print(paste(Sys.time(), glue(msg), sep=' : '))
}
