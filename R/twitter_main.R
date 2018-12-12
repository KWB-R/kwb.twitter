# getAllTweets -----------------------------------------------------------------
getAllTweets <- structure(
  function # getAllTweets
### getAllTweets
(
  searchString = "", 
  ### string to be searched for. Only tweets containing this string are
  ### returned.
  latitude = NULL,
  ### (vector of) latitude(s)
  longitude = NULL,
  ### (vector of) longitude(s)
  address = NULL,
  ### (vector of) addresses, such as "Berlin, Germany" defining the locations
  ### around which to look for messages. This argument overrides \emph{latitude}
  ### and \emph{longitude}
  radius.km = 10, 
  ### (vector of) radius(es) around the point(s), either given by
  ### \emph{latitude} and \emph{longitude} or indirectly by \emph{address},
  ### defining the circle(s) in which to search for Twitter messages
  n = 500, 
  ### number of messages to be returned at most (per
  ### \emph{latitude}/\emph{longitude} pair or \emph{address})
  ...
  ### arguments passed to getTweets
)
{
  tweets.all <- NULL
  
  .stopOnInvalidArguments(latitude, longitude, address, radius.km)
  
  if (!is.null(address)) {
    positions <- addressesToGeocodes(address)
  }
  else {
    x <- data.frame(latitude = latitude, longitude = longitude)
    positions <- lapply(
      seq_len(nrow(x)), 
      FUN = function(i, dataFrame) dataFrame[i, ], 
      dataFrame = x
    )
  }
  
  # Recycle radius to length of positions
  radius.km <- recycle(radius.km, n = length(positions))
  
  for (position in positions) {
    
    lat <- as.numeric(position["latitude"])
    long <- as.numeric(position["longitude"])
    
    geocode <- geocodeString(lat = lat, long = long, radius = radius.km)
    
    cat("Getting tweets around (", lat, ",", long, ") ... ")
    
    tweets <- getTweets(searchString, geocode = geocode, n = n, ...)
    
    cat("ok.\n")    
    
    tweets.all <- rbind(tweets.all, tweets)
  }
  
  tweets.all
}, ex = function() {
  ## For authentication, you need to select the file containing the decryption
  ## key
  authenticate()
  
  tweets <- getAllTweets(
    searchString = "water", 
    address = c("Berlin, Germany", "Paris, France", "New York, NY, USA"),
    radius.km = 30,
    n = 10 # limit to 10 messages per location
  )
  
  showSearchResults(tweets)
})

# .stopOnInvalidArguments ------------------------------------------------------
.stopOnInvalidArguments <- function
(
  latitude, longitude, address, radius.km
)
{
  # address must be given if there are no valid latitudes/longitudes
  if (is.null(latitude) || is.null(longitude)) {
    if (is.null(address)) {
      stop("address must be given if no latitude/longitude information are ",
           "given")
    }
  }
  
  # latitude and longitude must be of same length
  if (length(latitude) != length(longitude)) {
    stop("latitude and longitude must be of same length")
  }
  
  # Give a warning if address is given but also latitude/longitude
  if (!is.null(address) && (!is.null(latitude) || !is.null(longitude))) {
    warning("latitude and longitude are ignored since address is given.")
  }
  
  # radius.km must be as long as there are positions
  if (is.null(address)) {
    if (length(latitude) != length(radius.km)) {
      warning("radius.km is recycled to be as long as latitude/longitude")
    }
  }
  else {
    if (length(address) != length(radius.km)) {
      warning("radius.km is recycled to be as long as address")
    }    
  }
}

# geocodeString ----------------------------------------------------------------
geocodeString <- function # geocodeString
### geocodeString
(
  lat, long, radius
)
{
  sprintf("%f,%f,%fkm", lat, long, radius)
}

# getTweets --------------------------------------------------------------------
getTweets <- structure(function # getTweets
### getTweets
(
  searchString = "", geocode = NULL, n = 50, ...
)
{
  tweets <- searchTwitter(searchString, n = n, geocode = geocode, ...)
  
  if (!isNullOrEmpty(tweets)) {
    structure(twListToDF(tweets), geocode = geocode)
  }
  else {
    NULL
  }
}, ex = function(){
  ## For authentication, you need to select the file containing the decryption key
  authenticate()
  
  geocodeCoordinates <- getGeocode("Berlin, Cicerostr. 24")
  
  geocode <- geocodeString(
    lat = geocodeCoordinates["latitude"], 
    long = geocodeCoordinates["longitude"], 
    radius = 10 # km
  )
  
  tweets <- getTweets(searchString = "Spree", geocode = geocode, n = 100)
  
  showSearchResults(tweets)
})

