


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

### Type test code in here.


both.dfs <- (get.train.df() %>% split.train.test.df())


library(lightgbm)


df.train <- DfHolder$new(both.dfs$train)
df.test <- DfHolder$new(both.dfs$test)



tic('Running lgbm')
model <- lgb.train( 
    data = df.train$as.lgb.train(),
    objective = "binary",
    metric="auc",
    seed= 102191,
    num_iterations=300,
    boost_from_average=TRUE,
    bagging_fraction=1, 
    feature_fraction=1.0, 
    learning_rate=0.0010, 
    max_bin=255, 
    num_leaves=1024,
    verbose = 0,
    min_data_in_leaf = 10  
)
toc()

predictions <- model %>% predict( df.test$as.lgb.predict())
score <<- globalenv()$score.prediction(predictions, df.test$as.results(), cutoff=0.025 )


test.func<-function(preds, dtrain)
{
    return( list(
        name='suboMetrics',
        value=globalenv()$score.prediction(preds, dtrain$getinfo('label')),
        higher_better=TRUE))
}


cv.result <- lgb.cv( 
    data = df.train$as.lgb.train(),
    nfold=5L,
    objective = "binary",
    metric="suboMetrics",
    showsd = TRUE,
    stratified=TRUE,
    nrounds=300,
    seed= 102191,
    boost_from_average=TRUE,
    bagging_fraction=1, 
    feature_fraction=0.6, 
    learning_rate=0.03, 
    max_depth=6,
    max_bin=255, 
    num_leaves=1024,
    verbose = 5,
    eval_freq=5,
    min_data_in_leaf = 5,
    eval=test.func
)







params <- list(eta=0.04, colsample_bytree=0.6, nrounds=100)
cv <- XgBoostCvWorkflow$new(params=params, train.df=df)

cv$go()



a <- XgbMboOptmizer$new(dataframe=df,
                   output.folder='my_output_folder',
                   parameters.set=makeParamSet(
                       makeNumericParam('eta', lower=.01, upper=.1),
                       makeNumericParam('colsample_bytree', lower=.1, upper=1)
                   ),
                   iterations.per.point=4,
                   total.points=100)


a$go()

a$print.mbo(55)
