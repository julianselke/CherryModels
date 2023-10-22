#' Exhaustively test liner mixed-effects models
#'
#' Compute thousands of linear mixed-effects models to determine the model that explains your data
#' best. To this end, all combinations of fixed effects with and without interaction are
#' generated and computed with `lmerTest::lmer()`. Subsequently, a step-wise model reduction is
#' performed on every model with `lmerTest::step()`. The process is parallelized an can be run as
#' subprocess in the background. See vignette("CherryModels") for an introduction.
#'
#' @param model_data data.frame or tibble: The data for the model containing all variables passed to
#' other arguments.
#' @param response character of length one: The dependent variable.
#' @param fixed_inter character of length one or more: Independent variables for which interaction
#' terms shall be tested.
#' @param fixed_no_inter character of length one or more: Independent variables for which NO
#' interaction terms shall be tested.
#' @param max_comb integer of length one: Number of fixed effects that go into an interaction term.
#' @param comb_comb integer of length one: Number of interaction terms to combine,
#' @param random character of length one or more: The random effect(s) in `lmer` syntax.
#' @param n.workers integer of length one: Number of threads for parallelization.
#' @param dry logical of length one: If `TRUE` does not compute models but outputs the formulas.
#' @param background logical of length one: Whether to run the computation as a subprocess in the
#' background (a separate R session). Note that in this case a handler is returned instead of a
#' data.frame. Use `get_process_result()` to interact with the background process.
#'
#' @return data.frame or r_process
#' @import callr
#' @import tidyverse
#' @import lmerTest
#' @import magrittr
#' @import furrr
#' @export
#'
#' @examples
#'
#' x <- cherry_models(
#'   model_data = mpg,
#'   response = "cty",
#'   fixed_inter = c("displ", "cyl", "year"),
#'   fixed_no_inter = "hwy",
#'   max_comb = 3,
#'   comb_comb = 2,
#'   random = "(1|model)",
#'   n.workers = 2,
#'   dry = FALSE,
#'   background = FALSE
#' )
#'
#'
#' # show formulas only
#' cherry_models(
#'   model_data = mpg,
#'   response = "cty",
#'   fixed_inter = c("displ", "cyl", "year"),
#'   fixed_no_inter = "hwy",
#'   max_comb = 3,
#'   comb_comb = 2,
#'   random = "(1|model)",
#'   n.workers = 2,
#'   dry = TRUE,
#'   background = FALSE
#' )
#'
#'
#' # run as background process
#' proc <- cherry_models(
#'   model_data = mpg,
#'   response = "cty",
#'   fixed_inter = c("displ", "cyl", "year"),
#'   fixed_no_inter = "hwy",
#'   max_comb = 3,
#'   comb_comb = 2,
#'   random = "(1|model)",
#'   n.workers = 2,
#'   dry = FALSE,
#'   background = TRUE
#' )
#'
#' # get progress and eventually results from process
#' x <- get_process_results(proc)
#'
#'
#' # view results in a shiny app
#' show_report(x)
#'
cherry_models <- function(model_data,
                          response,
                          fixed_inter,
                          fixed_no_inter,
                          max_comb = 3,
                          comb_comb = 2,
                          random,
                          n.workers = 2,
                          dry = FALSE,
                          background = FALSE) {
  if (isTRUE(dry)) {
    formulas <- CherryModels:::make_formula(response,
                                           fixed_inter,
                                           fixed_no_inter,
                                           max_comb,
                                           comb_comb,
                                           random)
    return(formulas)
  }
  if (isFALSE(background)) {
    res <- parallel_models(
      model_data,
      response,
      fixed_inter,
      fixed_no_inter,
      max_comb,
      comb_comb,
      random,
      n.workers)
  } else {
    message("Starting r_process in background and returning handler")
    cherry_temp_file <- tempfile(pattern = "cherry")
    res <- callr::r_bg(CherryModels:::parallel_models,
                       list(model_data,
                            response,
                            fixed_inter,
                            fixed_no_inter,
                            max_comb,
                            comb_comb,
                            random,
                            n.workers),
                       stderr = cherry_temp_file)
    attr(res, "temp_out") <- cherry_temp_file
  }
  return(res)
}


parallel_models <- function(model_data,
                            response,
                            fixed_inter,
                            fixed_no_inter,
                            max_comb,
                            comb_comb,
                            random,
                            n.workers) {
  # load in new session
  require(tidyverse)
  require(magrittr)
  require(lmerTest)
  require(furrr)
  # make data available to lmerTest (model.frame issue)
  .CHERRY_MODEL_DATA <<- model_data
  formulas <- CherryModels:::make_formula(response,
                                          fixed_inter,
                                          fixed_no_inter,
                                          max_comb,
                                          comb_comb,
                                          random)
  # randomize to prevent unequal chunks
  formulas <- formulas[sample(length(formulas))]
  future::plan(multisession, workers = n.workers)
  start_time <- Sys.time()
  res <- furrr::future_map(.x = formulas, .f = CherryModels:::compute_models, .progress = TRUE)
  stop_time <- Sys.time()
  res <- dplyr::bind_rows(res)
  attr(res, "model_data") <- model_data
  attr(res, "response") <- response
  attr(res, "fixed_inter") <- fixed_inter
  attr(res, "fixed_no_inter") <- fixed_no_inter
  attr(res, "random") <- random
  attr(res, "compute_time") <- (stop_time - start_time)
  attr(res, "call") <- match.call()
  return(res)
}


compute_models <- function(formula) {
  model_formula <- as.formula(formula)
  tmp <- lmerTest::lmer(formula = model_formula, data = .CHERRY_MODEL_DATA)
  aic_old <- AIC(tmp)
  tmp <- tmp %>% lmerTest::step(reduce.random = FALSE)
  reduced <- paste(sum(tmp[["fixed"]][["Eliminated"]] > 0),
                   "/",
                   length(tmp[["fixed"]][["Eliminated"]]))
  fixd <- tmp %>% `$`("fixed") %>%
    dplyr::filter(Eliminated == 0) %>%
    tibble::rownames_to_column("fixed") %>%
    dplyr::pull(fixed) %>%
    paste(sep = " + ", collapse = " + ")
  mo <- tmp %>% attr("model") %>% `@`("call") %>% as.character() %>% .[2]
  mo <- ifelse(mo == "model_formula", formula, mo)
  tmp <- lmerTest::lmer(as.formula(mo), data = .CHERRY_MODEL_DATA)
  aic_new <- tmp %>% AIC()
  mod <- data.frame(AIC.init = aic_old,
                    AIC.final = aic_new,
                    Model.init = formula,
                    Model.final = mo,
                    Reduced = reduced,
                    Fixed = fixd)
  return(mod)
}


make_formula <- function(response,
                         fixed_inter,
                         fixed_no_inter,
                         max_comb,
                         comb_comb,
                         random) {
  random_effects <- CherryModels:::make_random_effects(random)
  fixed_effects <- CherryModels:::make_fixed_effects(fixed_inter,
                                                     fixed_no_inter,
                                                     max_comb,
                                                     comb_comb)
  complete_formula <- paste0(response, " ~ ", paste(fixed_effects, random_effects, sep = " + "))
  return(complete_formula)
}


make_fixed_effects <- function(fixed_inter, fixed_no_inter, max_comb, comb_comb) {
  if (max_comb > length(fixed_inter)) {
    warning(sprintf(paste0("max_comb (%s) must not be  larger than length of fixed_inter (%s)",
                           "\n> SETTING max_comb to %s"),
                    max_comb, length(fixed_inter), length(fixed_inter)))
    max_comb <- length(fixed_inter)
  }
  fixed_effects_inter <- CherryModels:::combs(vec = fixed_inter, n = max_comb, sep =  "*") %>%
    CherryModels:::combs(vec = ., n = comb_comb, sep =  "+")
  fixed_effects_no_inter <- CherryModels:::combs(vec = fixed_no_inter,
                                                 n = length(fixed_no_inter),
                                                 sep = "+")
  fixed_effects <- expand.grid(fixed_effects_inter, fixed_effects_no_inter) %$%
    str_c(Var1, Var2, sep = " + ")
  message(sprintf("generating %s models",
                  format(length(fixed_effects), big.mark = ",", scientific = FALSE)
  )
  )
  return(fixed_effects)
}


combs <- function(vec, n, sep) {
  l <- vector(mode = "list", length = n)
  for(i in seq_len(n)) {
    if (i == 1) {
      l[[i]] <- vec
    } else {
      l[[i]] <- combn(x = vec, m = i) %>%
        as.data.frame() %>%
        apply(2, sort, decreasing = FALSE) %>%
        t() %>%
        as.data.frame() %>%
        tidyr::unite("comb", sep = paste0(" ", sep, " ")) %>%
        dplyr::pull(comb)
    }
  }
  l <- unlist(l)
  return(l)
}


make_random_effects <- function(...) paste(..., collapse = " + ")
