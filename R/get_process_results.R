#' Interact with a background process
#'
#' This function retrieves the result of a background process if the process if finished computing.
#' In this case the handler (the 'process object') will be removed from the environment.
#' If it is still running the output of the process, such as messages, are printed.
#'
#' @param rbg_proc an r_process returned by cherry_models(..., background = TRUE)
#'
#' @return data.frame when the `r_process` is finished; `invisible(NULL)` along with message on progress
#' otherwise
#' @import tidyverse
#' @import callr
#' @export
#'
#' @examples
#' proc <- cherry_models(
#'   model_data = mpg,
#'   response = "cty",
#'   fixed_inter = c("displ", "cyl", "year"),
#'   fixed_no_inter = c("hwy"),
#'   max_comb = 3,
#'   comb_comb = 3,
#'   random = "(1|model)",
#'   n.workers = 2,
#'   dry = FALSE,
#'   background = TRUE
#' )
#'
#' get_process_results(proc)
#'
get_process_results <- function(rbg_proc) {
  is_finished <- try(rbg_proc$get_status(), silent = TRUE)
  if (inherits(is_finished, "try-error")) {
    res <- rbg_proc$get_result()
    rbg_proc$finalize()
    proc_name <- deparse(substitute(rbg_proc))
    message(paste("finished\nremoving", proc_name))
    rm(list = proc_name, envir = sys.frame(-1))
    return(res)
  } else {
    message("busy:")
    get_cherry_progress(rbg_proc)
    return(invisible(NULL))
  }
}

get_cherry_progress <- function(rbg_proc) {
  readLines(attr(rbg_proc, "temp_out"), warn = FALSE) %>%
    rev() %>%
    head(1) %>%
    str_replace_all("[\r\n]" , "") %>%
    message()
}

