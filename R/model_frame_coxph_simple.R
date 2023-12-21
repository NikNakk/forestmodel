model_frame_coxph_simple <- function(model) {
  cph_terms <- stats::terms(model)
  fcall <- model$call
  indx <- match(c("formula", "data", "weights", "subset",
                  "na.action", "cluster", "id", "istate"), names(fcall),
                nomatch = 0)
  if (indx[1] == 0)
    stop("The coxph call is missing a formula!")
  temp <- fcall[c(1, indx)]
  temp[[1]] <- quote(stats::model.frame)
  temp$formula <- cph_terms
  if (!is.null(attr(temp$formula, "specials")$tt)) {
    coxenv <- new.env(parent = environment(temp$formula))
    assign("tt", function(x) x, envir = coxenv)
    environment(temp$formula) <- coxenv
  }
  if (is.null(environment(model$terms)))
    mf <- eval(temp, parent.frame())
  else mf <- eval(temp, environment(model$terms), parent.frame())
  mf
}
