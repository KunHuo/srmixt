#' Fitting Weighted Quantile Sum regression models
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param exposure exposure variable names.
#' @param covariates covariate variable names.
#' @param q An integer to specify how mixture variables will be ranked, e.g. in
#' quartiles (q = 4), deciles (q = 10), or percentiles (q = 100). If q = NULL
#' then the values of the mixture variables are taken (these must be standardized).
#' @param b number of bootstrap samples used in parameter estimation.
#' @param validation percentage of the dataset to be used to validate the model.
#' If validation = 0 then the test dataset is used as validation dataset too.
#' @param family a character value that allows to decide for the glm: gaussian
#' for linear regression, binomial for logistic regression "multinomial" for
#' multinomial regression, poisson for Poisson regression, quasipoisson for
#' quasi-Poisson regression, "negbin" for negative binomial regression.
#' @param seed an integer value to fix the seed, if it is equal to NULL no seed
#' is chosen.
#' @param ... additional arguments to be passed to the [gWQS::gwqs()] function.
#'
#' @return a list.
#' @export
sr_gwqs <- function(data,
                    outcome = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    q = 4,
                    b = 10,
                    validation = 0.3,
                    family = NULL,
                    seed = 2023,
                    ...){

  outcome    <- srmisc::select_variable(data, outcome)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)

  frm <- paste(outcome, "wqs", sep = " ~ ")
  if(!is.null(covariates)){
    frm <- paste(frm, paste(covariates, collapse = " + "), sep = " + ")
  }
  frm <- stats::as.formula(frm)


  if(is.null(family)){
    if(length(unique(data[[outcome]])) == 2L){
      family <- stats::binomial()
    }else{
      family <- stats::gaussian()
    }
  }

  res.positive <- tryCatch({
    gWQS::gwqs(
      frm,
      mix_name = exposure,
      data = data,
      q = q,
      validation = validation,
      b = b,
      b1_pos = TRUE,
      b1_constr = TRUE,
      family = family,
      seed = seed,
      ...
    )
  }, error = function(e) {
    NULL
  })


  res.negative <- tryCatch({
    gWQS::gwqs(
      frm,
      mix_name = exposure,
      data = data,
      q = q,
      validation = validation,
      b = b,
      b1_pos = FALSE,
      b1_constr = TRUE,
      family = family,
      seed = seed,
      ...
    )
  }, error = function(e) {
    NULL
  })

  list(positive = res.positive,
       negative = res.negative,
       data = data,
       outcome = outcome,
       exposure = exposure,
       covariates = covariates,
       family = family,
       b = b)
}
