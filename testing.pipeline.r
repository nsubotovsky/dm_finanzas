

standard.split <- function( df, frac, seed=-1 )
{
    return( split.train.test.df(df=df, fraction=frac, seed=seed) )
}


XgBoostWorkflow <- setRefClass('XgBoostWorkflow',
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
            
            log.debug('initializing xgb workflow')
            splitted.data <- split.func( full.df )
            train.df <<- DfHolder$new(splitted.data$train)
            test.df <<- DfHolder$new(splitted.data$test)
        },
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
        calc_score=function()
        {
            predictions <- model %>% predict( test.df$as.xgb.predict())
            score <<- globalenv()$score.prediction(predictions, test.df$as.results(), cutoff )
            return(score)
        }
    )
)



XgBoostCvWorkflow <- setRefClass('XgBoostCvWorkflow',
    fields = c('cv.result', 'params', 'train.df'),
    
    methods=list(
        initialize=function( params, train.df )
        {
            params <<- params
            train.df <<- DfHolder$new(train.df)
        },
        test.func=function(preds, dtrain)
        {
            return( list(
                metrics='suboMetrics',
                value=globalenv()$score.prediction(preds, dtrain %>% getinfo('label')) ))
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
        }
    )
)





