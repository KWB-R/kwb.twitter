# addressesToGeocodes ----------------------------------------------------------
addressesToGeocodes <- function # addressesToGeocodes
### addressesToGeocodes
(
  addresses
)
{
  sapply(addresses, getGeocode, simplify = FALSE)
}

# getGeocode -------------------------------------------------------------------
getGeocode <- structure(function # getGeocode
### lookup street address at google maps returning latitude and longitude
(
  address,
  ### Street address as you would enter it in the search field of google maps
  curl = getCurlHandle()
)
{
  parameters <- htmltools::urlEncodePath(address)
  
  url <- paste0(
    "http://maps.googleapis.com/maps/api/geocode/xml?address=", parameters
  )
  
  xml <- RCurl::getURLContent(url, curl = curl)
  
  xmlTree <- xmlTreeParse(xml)
  
  nodes.1 <- xmlChildren(xmlTree$doc$children$GeocodeResponse)
  
  nodes.1 <- nodes.1[names(nodes.1) == "result"]
  
  numberOfResults <- length(nodes.1)
  
  if (numberOfResults == 0) {
    warning("Address not found: '", address, "'")
    NULL
  }
  else {
    nodes.2 <- xmlChildren(nodes.1[[1]])

    if (numberOfResults > 1) {
      warning("More than one result found for address: '", address, 
              "'\n  I will use this one: ", xmlValue(nodes.2$formatted_address))
    }
    
    latAndLong <- xmlChildren(xmlChildren(nodes.2$geometry)$location)
    
    c(latitude = as.numeric(xmlValue(latAndLong$lat)), 
      longitude = as.numeric(xmlValue(latAndLong$lng)))    
  }
  ### named numeric vector with two elements: \emph{latitude} and \emph{longitude}
}, ex = function() {
  getGeocode("Berlin, Cicerostr. 24")
  getGeocode("Paris, Tour Eiffel")
  
  ## You get a warning if the address is ambiguous
  getGeocode("New York, World Trade Center")
})
