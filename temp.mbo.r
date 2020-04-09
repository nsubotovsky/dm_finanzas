

rm( list=ls() )
gc()

library(ggplot2)
library(mlrMBO)
library(tidyverse)



funcToOptimize <- function (x)
{
  return ( (x$height+2.05)^2 + (x$len-2.954)^2)
}



obj.fun <- makeSingleObjectiveFunction(
  name='viboraje',
  fn=funcToOptimize,
  par.set=makeParamSet(
    makeNumericParam('height', lower=-5, upper=5),
    makeNumericParam('len', lower=-5, upper=5)
  ),
  has.simple.signature = FALSE,
  #global.opt.params = list(x=-2, y=2),
  minimize=TRUE
)


print(obj.fun)


ctrl = makeMBOControl(propose.points = 1)
ctrl = setMBOControlTermination(ctrl, iters = 50L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 20L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
                 control = ctrl, points.per.dim = 50, show.info = TRUE)

print(run)

