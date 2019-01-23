#' create_twitter_token
#'
#' @return twitter token
#' @export
#' @importFrom rtweet create_token
#' 
create_twitter_token <- function() {
  
  twitter_vars <- sprintf("twitter_%s",
                          c("app",
                            "consumer_key", 
                            "consumer_secret", 
                            "access_token",
                            "access_secret"))
  
  is_not_defined <- Sys.getenv(twitter_vars) == ""
  
  if(!all(is_not_defined)) {
    rtweet::create_token(
      app = Sys.getenv("twitter_app"),
      consumer_key = Sys.getenv("twitter_consumer_key"),
      consumer_secret = Sys.getenv("twitter_consumer_secret"),
      access_token = Sys.getenv("twitter_access_token"),
      access_secret = Sys.getenv("twitter_access_secret"),
      set_renv = FALSE)
  } else {
    
    msg <- sprintf("Specify the twitter secrets in:\n\nSys.setenv(%s)
                   
                   for the above defined parameters!", 
                   paste(sprintf("'%s' = 'my_secret'", 
                                 twitter_vars[is_not_defined]), 
                         collapse = ", "))
    stop(msg)
  }
}