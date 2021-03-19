#' Random-Forest Cross Validation method
#'
#' This function predict output \code{body_mass_g} using covariates \code{bill_length_mm},
#' \code{bill_depth_mm}, and \code{flipper_length_mm}.
#'
#' @param k the number of folds
#' @keywords prediction, cross validation
#'
#' @return numeric with cross validation error
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
# Calling the library randomForest
library(randomForest)
library(project3package)
my_rf_cv <- function(k) {
  # Variable fold based on input parameter
  fold <- k
  my_penguins <- penguins %>% drop_na()
  total <- nrow(my_penguins)
  # Randomly assign observation to folds with equal probability
  inds <- sample(rep(1:fold, length = total))
  n_data <- data.frame(my_penguins) %>% mutate("split" = inds)

  # Introduce variable mse_fold to store and track mse
  mse_fold <- 0

  # for loops to iterate every fold for cross validation
  for (i in 1:fold) {

    # In each iteration, the data splitted into folds where we take the fold
    # data as test data and others as training
    x_train <- n_data %>% filter(split != i) %>% select(-"split")
    x_test <- n_data %>% filter(split == i) %>% select(-"split")
    sum_train <- n_data %>% filter(split == i)
    # Set up the model for random forest
    model <- randomForest(body_mass_g ~ bill_length_mm +
                            bill_depth_mm + flipper_length_mm,
                          data = x_train, ntree = 100)
    # predict the mass on data based on model
    prediction <- predict(model, x_test)

    # calculate the mean squared error between true and predict mass
    squared_err <- sum((sum_train[6] - prediction)^2)
    mse_fold <- mse_fold + squared_err
  }
  # return average of cv mse
  return(mse_fold/total)
}
