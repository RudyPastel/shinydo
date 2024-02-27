#' Suspend Execution for a Time Interval
#'
#' These are descriptively renamed wrappers of [base::Sys.sleep()]
#'
#' @param seconds A number of seconds
#'
#' @return Invisible `NULL`.
#'
#' @export
pauseAndThinkAboutIt = function(seconds){base::Sys.sleep(seconds)}
