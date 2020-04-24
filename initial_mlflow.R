# devtools::install_github("mlflow/mlflow", subdir = "mlflow/R/mlflow")
install.packages("mlflow")

mlflow::install_mlflow()

library(mlflow)
library(dplyr)

# read parameters
column <- mlflow_log_param("column", 1) %>% as.numeric

# log total rows
mlflow_log_metric("rows", nrow(iris))

# train model
model <- lm(
  Sepal.Width ~ x,
  data.frame(Sepal.Width = iris$Sepal.Width, x = iris[,column])
)

# log models intercept
mlflow_log_metric("intercept", model$coefficients[["(Intercept)"]])
