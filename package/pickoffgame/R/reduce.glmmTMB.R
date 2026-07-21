#' Reduce a glmmTMB object
#' 
#' This function reduces a fitted glmmTMB model to keep only the formula, estimated fixed effects,
#' and predicted random effects, all that is needed to generate predictions from the model for new
#' data. Importantly, this does not include the original dataset on which the model was trained,
#' which is necessary for bootstrapping but impossible for restricted data.
#' 
#' @param object a fitted \code{glmmTMB} object
#' 
#' @returns A named list:
#'   \itemize{
#'     \item \code{fixef}: a named vector of estimated fixed effects
#'     \item \code{ranef}: a list of predicted random effects
#'     \item \code{formula_fixef}: a formula object with the fixed-effects-only RHS
#'     \item \code{summary}: a sanitized model summary with only coefficients and varcor
#'   }
#' 
#' @importFrom glmmTMB fixef ranef
#' @importFrom reformulas nobars
#' 
reduce.glmmTMB <- function(object) {
  summary <- list(
    coefficients = summary(object)$coefficients,
    varcor = summary(object)$varcor
  )
  object_reduced <- list(
    fixef = glmmTMB::fixef(object)$cond,
    ranef = glmmTMB::ranef(object)$cond,
    formula_fixef = formula(object) |>
      update(NULL ~ .) |>                   # remove LHS
      reformulas::nobars(),                 # remove random effects
    summary = summary
  )
  class(object_reduced) <- "glmmTMBreduced"
  return(object_reduced)
}

#' Extract random effects from glmmTMB or glmmTMBreduced object
#' 
#' This is just a helper function to facilitate identifying the class of the object and then
#' using the appropriate code to extract the predicted random effects.
#' 
#' @param object a \code{glmmTMB} or \code{glmmTMBreduced} object
#' 
#' @returns a list of random effect tables
#' 
#' @export
extract_ranef <- function(object) {
  if (class(object) == "glmmTMB") {
    return(glmmTMB::ranef(object)$cond)
  } else if (class(object) == "glmmTMBreduced") {
    return(object$ranef)
  }
}

#' Extract fixed effects from glmmTMB or glmmTMBreduced object
#'  
#' This is just a helper function to facilitate identifying the class of the object and then
#' using the appropriate code to extract the estimated fixed effects.
#' 
#' @param object a \code{glmmTMB} or \code{glmmTMBreduced} object
#' 
#' @returns a named vector of fixed effects
#' 
#' @export
extract_fixef <- function(object) {
  if (class(object) == "glmmTMB") {
    return(glmmTMB::fixef(object)$cond)
  } else if (class(object) == "glmmTMBreduced") {
    return(object$fixef)
  }
}