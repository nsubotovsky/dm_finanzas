options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

update.packages( ask=FALSE  )

install.packages('R.utils', dependencies = TRUE)
install.packages('rpart.plot', dependencies = TRUE)
install.packages('xgboost', dependencies = TRUE)

install.packages('DiceKriging', dependencies = TRUE)
install.packages('mlrMBO', dependencies = TRUE)

install.packages('rsvg', dependencies = TRUE)
install.packages('DiagrammeRsvg', dependencies = TRUE)
install.packages('DiagrammeR', dependencies = TRUE)

install.packages('iml', dependencies = TRUE)

library( devtools )
install_github("AppliedDataSciencePartners/xgboostExplainer")
install_github("lantanacamara/lightgbmExplainer")

update.packages( ask=FALSE, repos = "https://cloud.r-project.org/"  )
quit( save="no" )