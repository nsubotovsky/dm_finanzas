
library(ggplot2)
library(mlrMBO)
library(lightgbm)

standard.split <- function( df, frac, seed=-1 )
{
    return( split.train.test.df(df=df, fraction=frac, seed=seed) )
}


BaseWorkflow <- setRefClass('BaseWorkflow',
    fields = c('train.df','test.df', 'model' ,'params', 'cutoff', 'score', 'train.seed'),
    
    methods=list(
        initialize=function( full.df, split.func, params, cutoff=0.025, train.seed=MASTER_SEED )
        {
            # Model initialized empty
            model <<- NA
            
            params <<- params
            
            cutoff <<- cutoff
            
            train.seed <<- train.seed
            
            score <<- -1
            
            log.debug('initializing workflow')
            
            if (is.na( split.func ))
            {
                log.debug('No split function provided. Will train with full df')
                train.df <<- DfHolder$new(full.df)
            } else {
                log.debug('splitting with provided function')
                splitted.data <- split.func( full.df )
                train.df <<- DfHolder$new(splitted.data$train)
                test.df <<- DfHolder$new(splitted.data$test)
            }
        },
        get.prediction.probs=function( predict.df=NA )
        {
            if (is.na(predict.df)){
                return( model %>% predict( .self$test.df.as.predict() ) )
            } else {
                log.debug('predicting with procided DF')
                return( model %>% predict( .self$test.df.as.predict( predict.df ) ) )
            }
        },
        calc_score=function()
        {
            predictions <- .self$get.prediction.probs()
            score <<- globalenv()$score.prediction(predictions, test.df$as.results(), cutoff )
            return(score)
        }
    )
)


XgBoostWorkflow <- setRefClass('XgBoostWorkflow',
   contains="BaseWorkflow",
   methods=list(
       train=function()
       {
           set.seed( train.seed )
           
           log.debug('training XGB...')
           tic('train XGB complete')
           model <<- xgb.train( 
               data= train.df$as.xgb.train(),
               objective= "binary:logistic",
               tree_method= "hist",
               max_bin= 31,
               base_score=train.df$mean,
               
               eta              = params$eta,
               nrounds          = params$nrounds, 
               colsample_bytree = params$colsample_bytree
           )
           toc()
       },
       test.df.as.predict=function(df=NA)
       {
           if (is.na(df)){
               return( test.df$as.xgb.predict() )
           } else{
               return( DfHolder$new(df)$as.xgb.predict() )
           }
           
       }
   )
)
    
LgbWorkflow <- setRefClass('LgbWorkflow',
   contains="BaseWorkflow",
   methods=list(
       train=function()
       {
           set.seed( train.seed )
           
           log.debug('training LGBM...')
           tic('train LGBM complete')
           model <<- lgb.train( 
               data = train.df$as.lgb.train(),
               objective = "binary",
               seed= train.seed,
               boost_from_average=TRUE,
               bagging_fraction=1,
               nrounds          = .self$params$nrounds,
               feature_fraction = .self$params$feature_fraction,
               learning_rate    = .self$params$learning_rate,
               max_depth=6,
               max_bin=255,
               num_leaves=1024,
               verbose = -1,
               eval_freq=0,
               min_data_in_leaf = 5
           )
           toc()
       },
       test.df.as.predict=function(df=NA)
       {
           if (is.na(df)){
               return( test.df$as.lgb.predict() )
           } else{
               return( DfHolder$new(df)$as.lgb.predict() )
           }
           
       }
   )
)                           


BaseCvWorkflow <- setRefClass('BaseCvWorkflow',
    fields = c('cv.result', 'params', 'train.df', 'cv.score'),
    
    methods=list(
        initialize=function( params, train.df )
        {
            params <<- params
            train.df <<- DfHolder$new(train.df)
        }
    )
)


XgBoostCvWorkflow <- setRefClass('XgBoostCvWorkflow',
     contains="BaseCvWorkflow",
     methods=list(
         test.func=function(preds, dtrain)
         {
             return( list(
                 metrics='suboMetrics',
                 value=globalenv()$score.prediction(preds, dtrain %>% xgboost::getinfo('label')) ))
         },
         go=function( seed=NA )
         {
             if (is.numeric(seed)) set.seed(seed)
             
             globalenv()$log.debug('running CV...')
             tic('CV Run complete')
             cv.result <<- xgb.cv( 
                 data= train.df$as.xgb.train(),
                 nfold=5,
                 objective= "binary:logistic",
                 tree_method= "hist",
                 max_bin= 31,
                 base_score= train.df$mean,
                 nrounds          = params$nrounds,
                 eta              = params$eta,
                 colsample_bytree = params$colsample_bytree,
                 print_every_n = 50L,
                 stratified=TRUE,
                 maximize = TRUE,
                 feval=.self$test.func
             )
             toc()
             
             cv.score <<- .self$getTop5()
             return(.self)
         },
         getTop5=function()
         {
             top_5 <- cv.result$evaluation_log %>%
                 select( test_suboMetrics_mean ) %>%
                 arrange( test_suboMetrics_mean ) %>%
                 tail(5)
             return ( top_5$test_suboMetrics_mean %>% mean() )
             
         }
     )
)



LgbmCvWorkflow <- setRefClass('LgbmCvWorkflow',
     contains="BaseCvWorkflow",
     methods=list(
         test.func=function(preds, dtrain)
         {
             return( list(
                 name='suboMetrics',
                 value=globalenv()$score.prediction(preds, dtrain$getinfo('label')),
                 higher_better=TRUE))
         },
         go=function( seed=NA )
         {
             #if (is.numeric(seed)) set.seed(seed)
             
             globalenv()$log.debug('running CV...')
             tic('CV Run complete')
             cv.result <<- lgb.cv(
                 data = train.df$as.lgb.train(),
                 nfold=5L,
                 objective = "binary",
                 metric="suboMetrics",
                 showsd = TRUE,
                 stratified=TRUE,
                 seed= seed,
                 boost_from_average=TRUE,
                 bagging_fraction=1,
                 nrounds          = .self$params$nrounds,
                 feature_fraction = .self$params$feature_fraction,
                 learning_rate    = .self$params$learning_rate,
                 max_depth=6,
                 max_bin=255,
                 num_leaves=1024,
                 verbose = -1,
                 eval_freq=0,
                 min_data_in_leaf = 5,
                 eval=.self$test.func
             )
             toc()
             
             cv.score <<- .self$cv.result$best_score
             return(.self)
         }
     )
)




BaseOptmizer <- setRefClass('BaseOptmizer',
     fields = c( 'dataframe', 'output.file', 'iterations.per.point', 'total.points', 'parameters.set', 'ouput.run', 'nrounds'),
     
     methods=list(
         initialize=function( dataframe, output.folder, parameters.set, iterations.per.point=5, total.points=50, nrounds=500 )
         {
             output.file <<- globalenv()$get.data.dir( output.folder, 'mbo.txt', auto.create=TRUE )
             iterations.per.point <<- iterations.per.point
             total.points <<- total.points
             parameters.set <<- parameters.set
             dataframe <<- dataframe
             nrounds <<- nrounds
         },
         go=function()
         {
             obj.fun <- makeSingleObjectiveFunction(
                 name='mbo_hiperparams',
                 fn=.self$func.wrapper,
                 par.set=.self$parameters.set,
                 has.simple.signature = FALSE,
                 minimize=FALSE)
             
             ctrl = makeMBOControl(propose.points = 1,
                                   save.on.disk.at.time=60,
                                   save.file.path = .self$output.file) %>%
                    setMBOControlTermination( iters = .self$total.points) %>%
                    setMBOControlInfill(
                        crit = makeMBOInfillCritEI(),
                        opt = "focussearch", opt.focussearch.points = 20L
                    )
             
             lrn = makeMBOLearner(ctrl, obj.fun)
             
             design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)
             
             
             if (file.exists(.self$output.file)==TRUE) {
                 ouput.run <<- mboContinue(.self$output.file)
                 
             } else {
                 ouput.run <<- mbo(fun=obj.fun, design=design, control=ctrl, learner=lrn)}
             
             
         },
         func.wrapper=function( x )
         {
             seeds <- get.seeds(.self$iterations.per.point)
             seeds <- seeds %>% mutate(params=rep(list(x),.self$iterations.per.point))
             seeds.with.results <- seeds %>% mutate( result=map2( params, seed, .self$autoTestAndScore))
             return( seeds.with.results$result %>% unlist() %>% mean() )
         },
         get.mbo.fromfile=function()
         {
             mbo.data <- get(load(output.file))
             return(mbo.data$opt.path$env$path)
         }
     )
)



XgbMboOptmizer <- setRefClass('XgbMboOptmizer',
          contains="BaseOptmizer",
          methods=list(
              autoTestAndScore=function( x, seed=NA )
              {
                  cvparams <- x
                  cvparams$nrounds <- .self$nrounds
                  
                  cv <- XgBoostCvWorkflow$new(params=cvparams, train.df=.self$dataframe)
                  return( cv$go(seed)$cv.score )
              },
              print.mbo=function(limit)
              {
                  print(
                      ggplot(.self$get.mbo.fromfile() %>%filter(y > limit),
                             aes(x = eta, y = colsample_bytree, color=y) ) +
                          geom_point() +
                          xlim(0,.1) +
                          ylim(0,1) +
                          scale_color_gradient(low="blue", high="red"))
              }
          )
)



LgbMboOptmizer <- setRefClass('LgbMboOptmizer',
          contains="BaseOptmizer",
          methods=list(
              autoTestAndScore=function( x, seed=NA )
              {
                  cvparams <- x
                  cvparams$nrounds <- .self$nrounds
                  
                  cv <- LgbmCvWorkflow$new(params=cvparams, train.df=.self$dataframe)
                  return( cv$go(seed)$cv.score )
              },
              print.mbo=function(limit)
              {
                  print(
                      ggplot(.self$get.mbo.fromfile() %>%filter(y > limit),
                             aes(x = learning_rate, y = feature_fraction, color=y) ) +
                          geom_point() +
                          xlim(0,.1) +
                          ylim(0,1) +
                          scale_color_gradient(low="blue", high="red"))
              }
          )
)
