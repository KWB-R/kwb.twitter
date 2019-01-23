#### NEEDS TO BE REFACTORED IN ORDER TO WORK WITH "rtweet" PACKAGE 
if(FALSE) {
  
# authenticate -----------------------------------------------------------------
authenticate <- function # authenticate
### authenticate
(
  keyFile = "", 
  passwordDir = system.file("extdata", package = "kwb.twitter"), 
  account = ""
)
{
  if (keyFile == "") {
    message("Please choose the file containig the encryption key...")
    keyFile <- file.choose()
  }
  
  passwordFiles <- .passwordFilepaths(passwordDir, account)
  
  passwords <- sapply(passwordFiles, getPassword, keyFile)
  
  setup_twitter_oauth(
    consumer_key = passwords[1],
    consumer_secret = passwords[2],
    access_token = passwords[3],
    access_secret = passwords[4]
  )
}

# .passwordFilepaths -----------------------------------------------------------
.passwordFilepaths <- function(passwordDir, account = "")
{
  filenames <- .passwordFilenames(prefix = account)
  
  filepaths <- as.list(file.path(passwordDir, filenames))
  
  names(filepaths) <- names(filenames)
  
  filepaths
}

# .passwordFilenames -----------------------------------------------------------
.passwordFilenames <- function(prefix = "")
{
  if (prefix != "") {
    prefix <- paste0(prefix, "_")
  }
  
  list(
    consumer_key = paste0(prefix, "consumer_key.txt"),
    consumer_secret = paste0(prefix, "consumer_secret.txt"),
    access_token = paste0(prefix, "access_token.txt"),
    access_secret = paste0(prefix, "access_secret.txt")
  )
}

# createKeyAndPasswordFiles ----------------------------------------------------
createKeyAndPasswordFiles <- function # createKeyAndPasswordFiles
### createKeyAndPasswordFiles
(
  keyFile = file.path(tempdir(), "twitterKey.txt"), 
  passwordDir = tempdir(),
  account = ""
)
{
  generateKeyFile(target = keyFile)
  
  passwordFiles <- .passwordFilepaths(passwordDir, account)
  
  for (account in names(passwordFiles)) {
    createPasswordFile(
      account = account, 
      keyFile = keyFile, 
      passwordFile = passwordFiles[[account]]
    )
  }
}

}
