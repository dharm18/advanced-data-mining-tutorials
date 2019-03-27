# METHOD 1: Using geocode() from {ggmap}
install.packages(ggmap)
library(ggmap)
adr <- "Agra, New Delhi"  # define address
geocode(adr)  # get the latitude and longitude

# Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Agra,+New+Delhi&sensor=false
#     lon      lat
1 77.3126 28.54637

# METHOD 2: CODE TO GET THE LATITUDE AND LONGITUDE OF A STREET ADDRESS WITH GOOGLE API
addr <- '6th Main Rd, New Thippasandra, Bengaluru, Karnataka'  # set your address here
url = paste('http://maps.google.com/maps/api/geocode/xml?address=', addr,'&sensor=false',sep='')  # construct the URL
doc = xmlTreeParse(url) 
root = xmlRoot(doc) 
lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]) 
long = xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
lat
[1] "12.9725020"
long
[1] "77.6510688"

?register_google

geocode("Agra, New Delhi")

revgeocode(c(-97.358112, 37.683829), output = "more")
