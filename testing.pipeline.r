

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





