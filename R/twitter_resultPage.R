#### NEEDS TO BE REFACTORED IN ORDER TO WORK WITH "rtweet" PACKAGE 
if(FALSE) {
# showSearchResults ------------------------------------------------------------
showSearchResults <- function # showSearchResults
### showSearchResults
(
  tweets, 
  curl = getCurlHandle(ssl.verifypeer = FALSE, followlocation = TRUE),
  main = "Search results"
)
{  
  if (!isNullOrEmpty(tweets)) {
    
    imageURLs <- getAllImageURLs(tweets, curl)
    
    showImages(tweets, imageURLs, main = main)
  }
  else {
    warning("No tweets found at all!")
  }  
}

# getAllImageURLs --------------------------------------------------------------
getAllImageURLs <- function # getAllImageURLs
### getAllImageURLs
(
  tweets, curl
)
{
  imageURLs <- list()
  
  for (i in seq_len(nrow(tweets))) {
    
    urls <- containedUrls(
      messages = tweets$text[i], httpOnly = FALSE, as.list = TRUE
    )
    
    imageURLs[[i]] <- browseForImageURLs(urls, curl = curl, as.list = TRUE)
  }
  
  imageURLs
}

# containedUrls ----------------------------------------------------------------
containedUrls <- function # containedUrls
### containedUrls
(
  messages, httpOnly = FALSE, as.list = FALSE
)
{
  pattern <- ifelse(httpOnly, "^http\\:", "^http")
  
  urls.list <- sapply(
    X = strsplit(messages, "\\s+"), 
    FUN = function(x, pattern) {
      grep(pattern, x, value = TRUE)
    }, 
    pattern = pattern 
  )
  
  if (as.list) {
    return(urls.list)
  }
  else {
    return(unique(unlist(urls.list)))
  }  
}

# browseForImageURLs -----------------------------------------------------------
browseForImageURLs <- function # browseForImageURLs
### browseForImageURLs
(
  urls, curl, as.list = FALSE
)
{
  imageURLs <- sapply(
    X = urls, 
    FUN = browseForImageURLsInOneHtmlFile, 
    curl = curl
  )
  
  if (as.list) {
    return(imageURLs)
  }
  else {
    return(unique(unlist(imageURLs)))
  }
}

# browseForImageURLsInOneHtmlFile ----------------------------------------------
browseForImageURLsInOneHtmlFile <- function # browseForImageURLsInOneHtmlFile
### browseForImageURLsInOneHtmlFile
(
  url, curl
) 
{
  if (length(url) > 0 && hsTrim(url) != "") {
    cat("Browsing for images in", url, "...\n")
    content <- tryToGetHtmlContent(url, curl = curl)    
  }
  else {
    content <- character()
  }
  
  if (!isNullOrEmpty(content)) {
    imageURLs <- extractImageURLs(content = content)
    if (length(imageURLs) > 0) {
      cat("Extracted image URLs:\n")  
      print(imageURLs)
    }    
  }
  else {
    imageURLs <- character()
  }
  
  imageURLs
}  

# tryToGetHtmlContent ----------------------------------------------------------
tryToGetHtmlContent <- function # tryToGetHtmlContent
### tryToGetHtmlContent
(
  url, curl
)
{
  result <- try(getURL(url, curl = curl))
  
  if (inherits(result, "try-error")) {
    warning("Could not access ", url)
    result <- NULL
  }
  
  if (isNullOrEmpty(result)) {
    NULL
  }
  else {
    result
  }
}

# extractImageURLs -------------------------------------------------------------
extractImageURLs <- function # extractImageURLs
### extractImageURLs
(
  content
)
{
  html <- htmlTreeParse(
    content, ignoreBlanks = TRUE, trim = TRUE, useInternalNodes = TRUE
  )
  
  images.1 <- as.character(unlist(xpathApply(html, '//img', function(x) {
    xmlAttrs(x)["src"]
  })))
  
  images.2 <- as.character(unlist(xpathApply(html, "//meta", function(x) {
    xmlAttrs(x)["content"]
  })))
  
  images <- grep("^http", c(images.1, images.2), value = TRUE)
  
  grep("\\.(png|jpg)$", images, value = TRUE)
}

# showImages -------------------------------------------------------------------
showImages <- function # showImages
### showImages
(
  tweets, imageURLs, main = "twittered imgages"
)
{
  stopifnot(nrow(tweets) == length(imageURLs))
  
  html.out <- tweetsAndImagesToHTML(tweets, imageURLs, main)
  
  htmlFile <- tempfile(fileext = ".html")
  cssFile <- file.path(tempdir(), "styles.css")
  
  writeLines(html.out, con = htmlFile)
  writeLines(getCss(), con = cssFile)
  
  browseURL(htmlFile)
}

# tweetsAndImagesToHTML --------------------------------------------------------
tweetsAndImagesToHTML <- function # tweetsAndImagesToHTML
### tweetsAndImagesToHTML
(
  tweets, imageURLs, main
)
{
  html.body <- ""
  
  for (i in seq_len(nrow(tweets))) {
    
    html.parts <- sapply(imageURLs[[i]], imagesToHTML, simplify = FALSE)
    
    html.images <- do.call(paste, args = html.parts)
    
    html.tweet <- tweetToHTML(
      tweetInfo = tweets[i, ], 
      html.images = html.images, 
      id = i
    )
    
    html.body <- paste(html.body, html.tweet)
  }
  
  html.body <- paste("<h1>", main, "</h1>\n", html.body)
  
  paste(
    "<html>", 
    "  <head>",
    "    <title>", main, "</title>",
    "    <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" />",
    "  </head>", 
    "  <body>", 
    "",   html.body, 
    "  </body>",
    "</html>",
    sep = "\n"
  )  
}

# imagesToHTML -----------------------------------------------------------------
imagesToHTML <- function # imagesToHTML
### imagesToHTML
(
  images
)
{
  if (length(images) > 0) {
    paste(
      sprintf("<a href=\"%s\"><img src=\"%s\" height = \"100px\"></img></a>", 
              images, images),
      collapse = "\n"
    )    
  }
  else {
    ""
  }
}

# tweetToHTML ------------------------------------------------------------------
tweetToHTML <- function # tweetToHTML
### tweetToHTML
(
  tweetInfo, html.images, id
)
{
  paste(
    "<div class=\"tweet\">",
    "  <h1><span>", id, "</span> ", tweetInfo$created, "</h1>",
    "  <p>", urlInTextToHtmlLink(tweetInfo$text), "</p>",
    "  <div class=\"images\">", html.images, "</div>",
    "</div>",
    sep = "\n"
  )  
}

# urlInTextToHtmlLink ----------------------------------------------------------
urlInTextToHtmlLink <- function # urlInTextToHtmlLink
### urlInTextToHtmlLink
(
  text
)
{
  gsub("(^|\\s+)(http[^ ]+)", "\\1<a href=\"\\2\">\\2</a>", text)
}

# getCss -----------------------------------------------------------------------
getCss <- function # getCss
### getCss
()
{
  "body {
  background: lightsteelblue;
  margin-left: 20px;
}
  
  h1 {
  font-size: 18px;
  }
  
  h1 span {
  background: yellow;
  }
  
  .tweet {
  max-width: 800px;
  border: 1px solid;
  background: white;
  margin-bottom: 5px;
  padding: 5px;
  }
  
  .images {
  border: 1px solid darkgrey;
  }
  "  
}

# openContainedUrls ------------------------------------------------------------
openContainedUrls <- function # openContainedUrls
### openContainedUrls
(
  messages
)
{
  sapply(containedUrls(messages), FUN = browseURL)
}

}