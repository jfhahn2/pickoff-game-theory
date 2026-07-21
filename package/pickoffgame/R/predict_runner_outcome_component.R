#' Predict component probability for runner outcome model
#' 
#' We need this function because we introduced a \code{glmmTMBreduced} object type, which strips
#' away all but the essentials for prediction from a fitted \code{glmmTMB} object. This function
#' identifies the object class and then calls the predict method (if appropriate) or else manually
#' constucts the predictions from matrix multiplication (if using the reduced object).
#' 
#' @param object a \code{glmmTMB} or \code{glmmTMBreduced} object
#' @param newdata a new data table holding covariates for which to make predictions
#' @param include_ranef logical, should random effects be included in from predcitions?
#'   Defaults to \code{TRUE}.
#' @param allow_new_levels logical, should we allow new levels for random effects? This plugs
#'   directly into \code{allow.new.levels} for \code{glmmTMB} objects. For \code{glmmTMBreduced}
#'   objects, the prediction is NA for new levels if \code{allow_new_levels} is \code{FALSE}.
#'   Defaults to \code{TRUE}.
#' 
#' @returns a vector of probability predictions
#' 
#' @export
predict_runner_outcome_component <- function(object, newdata, include_ranef = TRUE, allow_new_levels = TRUE) {
  if (class(object) == "glmmTMB") {
    re.form <- NULL
    if (!include_ranef) {
      re.form <- NA
    }
    prediction <- predict(
      object = object,
      newdata = newdata,
      type = "response",
      re.form = re.form,
      allow.new.levels = allow_new_levels
    )
  } else if (class(object) == "glmmTMBreduced") {
    eta <- model.matrix(object$formula_fixef, data = newdata) %*% as.matrix(object$fixef)
    if (include_ranef) {
      for (variable in names(object$ranef)) {
        pred_ranef <- object$ranef[[variable]][newdata[[variable]], ]
        if (allow_new_levels) {
          pred_ranef <- dplyr::coalesce(pred_ranef, 0)
        }
        eta <- eta + pred_ranef
      }
    }
    prediction <- c(exp(eta) / (1 + exp(eta)))
  }
  return(prediction)
}