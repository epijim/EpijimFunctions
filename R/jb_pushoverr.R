#' Send push notifications to your phone from R when script completes
#'
#' Token is a text file in working directory. Pushover account required.
#' @param message the message you wish to add. Time stamp is appended after.
#' @keywords iphone
#' @export
#' @examples
#' Read in token
#' token <- readChar("token",
#'                  file.info("token")$size) # shrink to character length
#' set_pushover_app(
#'   token=token,
#'   user="umTsDu9PSNCSf92NXEY21jUsvz3faT")
#'   jb_pushfinished() # at end of file

jb_pushfinished <- function(
  message='Script completed at '){
  devtools::use_package("pushoverr")
  pushoverr::pushover_high(
    paste(message,date())
  )}

#' Send push notifications to your phone from R for each n iterations of i
#'
#' Token is a text file in working directory. Pushover account required.
#' @param message What you want to be in the text of the message
#' @param iteration_interval Send message for every n iterations
#' @keywords iphone
#' @export
#' @examples
#' # Read in token
#' token <- readChar("token",
#'                  file.info("token")$size) # shrink to character length
#' # Load settings
#' pushoverr::set_pushover_app(
#'   token=token,
#'   user="umTsDu9PSNCSf92NXEY21jUsvz3faT")
#' for(i in 1:1000){
#'  # send a message every 100 iterations
#'  jb_pushiteration(iteration_interval=100)
#'  }

jb_pushiteration <- function(
  message='Script is on iteration: ',
  iteration_interval=10){
  devtools::use_package("pushoverr")
  if(i/iteration_interval==round(i/iteration_interval)){
    pushoverr::pushover(
      paste0("Script is on iteration: ",i)
    )
  }
}
