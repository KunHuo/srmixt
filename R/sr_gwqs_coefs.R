sr_gwqs_coefs <- function(object, ...){

  exec <- function(x, direction){
    res <- srmisc::typeset(x,
                    filter = "wqs",
                    select = c("effect", "p.value"), ...)
    res[1, 1] <- direction
    names(res)[1] <- "WQS index"
    res
  }

  out <- Map(\(x, direction){
    if(!is.null(x)){
      exec(x$fit, direction)
    }
  }, object[1:2], names(object[1:2]))

  out <- srmisc::list_rbind(out, names.as.column = FALSE)
  names(out)[2] <- paste(names(out)[2], letters[nrow(out) + 1])

  note.postive  <- sprintf("WQS positive index was obtained when the effect parameter of WQS model was constrained to the positive direction with %d repeated holdout validation.", object$b)
  note.negative <- sprintf("WQS negative index was obtained when the effect parameter of WQS model was constrained to the negative direction with %d repeated holdout validation.", object$b)

  if(nrow(out) == 2L){
    out[[1]][out[[1]] == "positive"] <- "Positive a"
    out[[1]][out[[1]] == "negative"] <- "Negative b"
    note <- paste(paste("a", note.postive),
                  paste("b", note.negative),
                  sep = "\n")
  }else{
    out[[1]][out[[1]] == "positive"] <- "Positive a"
    out[[1]][out[[1]] == "negative"] <- "Negative a"
    if(out[1, 1] == "Positive a"){
      note <- paste("a", note.postive)
    }else{
      note <- paste("a", note.negative)
    }
  }

  if(srmisc::is_empty(object$covariates)){
    adjusted <- "Models adjusted for nothing."
    adjusted <- paste(letters[nrow(out) + 1], adjusted)
    adjusted <- paste(adjusted, "Estimates were interpreted as the effect of increasing the mixture exposure by one quantile on the outcome.")
    note <- paste(note, adjusted, sep = "\n")
  }else{
    adjusted <- srmisc::get_var_label(object$data, object$covariates, default = ".name", units = FALSE)
    print(adjusted)
    adjusted <- paste(adjusted, collapse = ", ")
    adjusted <- sprintf("Models adjusted for %s.", adjusted)
    adjusted <- paste(letters[nrow(out) + 1], adjusted)
    adjusted <- paste(adjusted, "Estimates were interpreted as the effect of increasing the mixture exposure by one quantile on the outcome.")
    note <- paste(note, adjusted, sep = "\n")
  }

  abbr <- "WQS, weighted quantile sum; CI, confidence interval."
  note <- paste(abbr, note, sep = "\n")

  title <-  "Association between the WQS index and %s"
  title <- sprintf(title, srmisc::get_var_label(object$data, object$outcome, default = ".name", units = FALSE))
  attr(out, "title") <- title
  attr(out,  "note") <- note
  out
}
