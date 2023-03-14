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
       b = b)
}
