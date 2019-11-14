Flickr API request by grid
================
P.Norman
May 13, 2019

The purpose of this script is to allow you to collect a large amount of
Flickr image metadata from a wide area (across a state or small country
size), without exceeding the allowed pages and image amounts imposed
from the API. To to this we will create a grid using QGIS (easiest
option) then use the extent of each grid cell to return the images
within that area, then move onto the next cell.

\#\#Creating the grid If you know how or feel more comforatable getting
the outer extent coordinates of a grid using another program, definitely
use it. I have found the ‘create grid’ function in QGIS the easiest way
to get this data.

QGIS can be downloaded
here(<https://qgis.org/en/site/forusers/download.html>)

Firstly open QGIS -Click the blank page at the top left to open new map
-Then if you haven’t already, go to plugins(middle top), click the
manage and install plugins -Search for OpenLayers Plugin and install.
-From there open the web tab, openlayers plugin and load any of the
baselayer(just a quick way to see your area of interst)

![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/OpenLayers.png)

Now you should see a map of some form. Zoom to your area of interest.
Then click on the change crs(EPSG:something,something)
![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/change_crs.png) Now
change this to WGS84, EPSG:4326
![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/changing_crs.png)

Now we can start to make our grid. Click Vectors -\> Research Tool -\>
Create Grid

![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/create_grid.png)

In the Create Grid dialog box, change the grid type to
Rectangle(polygon). Then for grid extent, click the … on the right and
click Select Extent on Canvas.

![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/Select_on_canvas.png)

Now drag a box over your area of interest. For horizontal and vertical
spacing, if there is a city or popular spot to photograph in your area,
choose a small number like 0.02 degrees. If not 0.1 degrees is normally
alright. The reason for this is to keep the possible number of photos
per grid cell low enough to not exceed the API limit.

After clicking OK, you should now have a grid. Woohoo

Now right click the Grid bar on the left, click export then save feature
as. ![](E:/PhD_files/Scripts/R/API_pulls/Grid_trial/Save_as.png)

Now save as a comma separated value (CSV) and save your new grid to
where your R work directory will be.

\#\#It’s R time Great now we are up to the fun part.

Firstly load in the relevant packages

``` r
# Install and require necessary R packages.
library(RCurl) 
library (rjson)
library(RJSONIO)

# Set working directory with path name in quoation marks and / instead of \
setwd("Your/work/directory")
grid <- read.csv('your_grid.csv')
```

So your output from your grid should look something like this. Make sure
that the left, top, right and bottom are all in decimal degrees as this
is likely to confirm that you have the correct geographic projection.

``` r
head(grid)
```

    ##       left       top    right    bottom  id
    ## 1 153.3778 -28.04015 153.3878 -28.05015 127
    ## 2 153.3778 -28.05015 153.3878 -28.06015 128
    ## 3 153.3778 -28.06015 153.3878 -28.07015 129
    ## 4 153.3778 -28.07015 153.3878 -28.08015 130
    ## 5 153.3878 -27.95015 153.3978 -27.96015 131
    ## 6 153.3878 -27.96015 153.3978 -27.97015 132

Once you have done this and you’re confident that your grid.csv is
loaded in correctly, it is time to create a very large function.

Essentially what we are doing is turning the Flickr geo pull into a
function, then looping this function through your grid.csv, one line at
a time.

This next bit looks scary but don’t be alarmed as your really don’t have
to change much. The only things your need to change are the api\_key and
api\_secret. These can be sourced from
<https://www.flickr.com/services/api/>

You can also change the request date, especially the maxDate to get the
most current photos.

``` r
Flickr_return <-function(x){
# Flicker API access keys 
# Key must have single quotations around it
# Example: 'A8MNR7997LLOP123'
api_key = 'YOUR_API_KEY'
api_secret = 'YOUR_API_SECRET'

# Create function to return URL using flickr.photos.search 
# More information at https://www.flickr.com/services/api/flickr.photos.search.html
getURL = function(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum){
  root = 'https://api.flickr.com/services/rest/?method=flickr.photos.search&'
  u = paste0(root,"api_key=",api_key, "&min_taken_date=",  minDate,"&max_taken_date=", maxDate,"&bbox=", minLon,"%2C+", minLat, "%2C+", maxLon, "%2C+", maxLat,
             "&has_geo=1&extras=description%2C+geo%2C+date_taken%2C+date_upload%2C+views%2C+tags%2c+url_o&per_page=250&page=",
             pageNum, "&format=json&nojsoncallback=1" )
  return(URLencode(u))
}

#Create function to return URL to get Exif data from image metadata
getURL2 = function(api_key, id, secret){
  root2 = 'https://api.flickr.com/services/rest/?method=flickr.photos.getExif&'
  u2 = paste0(root2,"api_key=",api_key,"&photo_id=",id,"&secret=",secret,"&format=json&nojsoncallback=1" )
  return(URLencode(u2))
}


# Set location search parameters. 
minLon = grid$left[k]
minLat = grid$bottom[k]
maxLon = grid$right[k]
maxLat = grid$top[k]

# Set date search parameters.
# Use date format 'YYYY-MM-DD' with single quotations. 
minDate = 'start_date'
maxDate ='end_date'

#First call for specified date and bbox
##This will return the URL that contains the information based on search variables above
##The remainder of the code will then read through the information and write it to a data frame
getURL(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum=1)

#Read data returned from first call 
target = getURL(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum=1)
data = fromJSON(target)

# Get the total number of photo records returned using the current search parameters.
total = as.numeric(data$photos$total) 

# Number of pages of records returned using search parameters.
numPages = data$photos$pages 

# Create empty dataframe to populate with data.
df = NULL 
# For each page of results, from the first to maximum page number extract photo information.
for (j in 1:numPages){  
  pageNum = j 
  target.loop = getURL(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum)
  data1 = fromJSON(target.loop)
  numPhotos = length(data1$photos$photo)
  if (numPhotos==0){
      print(paste0(grid$id[k],'.....empty'))
      next()}
  else{
  # Read photo information for each photo on the current page.
    for (i in 1:numPhotos){ 
      id = data1$photos$photo[[i]]$id
      title = data1$photos$photo[[i]]$title
      lat = data1$photos$photo[[i]]$latitude
      lon = data1$photos$photo[[i]]$longitude
      owner = data1$photos$photo[[i]]$owner
      taken = data1$photos$photo[[i]]$datetaken
      dateupload = data1$photos$photo[[i]]$dateupload
      description = data1$photos$photo[[i]]$description
    
    
    # Convert UNIX epoch to date-time.
      upload = as.character.Date(as.POSIXct(as.numeric(dateupload), origin="1970-01-01"))
      views = data1$photos$photo[[i]]$views
      tags = data1$photos$photo[[i]]$tags
      secret = data1$photos$photo[[i]]$secret
      server = data1$photos$photo[[i]]$server 
      farm = data1$photos$photo[[i]]$farm
      imageURL = paste("https://farm", farm, ".staticflickr.com/", server, "/", id, "_", secret, ".jpg", sep="")
      getURL2(api_key, id, secret)
      target2 = getURL2(api_key, id, secret)
      exifData = fromJSON(target2)
    
      if (exifData$stat!="fail") {
      device = exifData$photo$camera
        } else{
      # If a value is not provided, then skip.
        device='NA'
      }
      row = cbind(lon, lat, id, owner, taken, upload, views, tags, title,description, imageURL, device)
      rbind(df, row)-> df
    
      if (j + 1 < numPages) {
      pageNum = j + 1
    }     
  }
}
  write.csv(df, paste0("grid_",grid$id[k],".csv"))}
}
```

And there it is. You should now be getting a big heap of csv files being
created in your work directory. Having requests in separate files has
been handy for my trials of this code as I’ve had a couple of network
errors. This meant I could check which grid the code last finished,
delete all the rows above this, then re-ran both codes. Hopefully
nothing goes wrong with your data pull but it is handy to know, just in
case.

Eventually you’ll want to join them when the loop has finished. To do
this, get only the Flickr returns into a single folder. Then run this.
It should join them very quickly.

``` r
setwd('/YOUR_EMPTY_FOLDER')

# Get all of the Flickr results file names
library(dplyr)
library(readr)
df <- list.files(full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

#removing any duplicates that may have snuck in
joined_files_clean <- df[!duplicated(df[c('imageURL')]),]
#writing out the 
write.csv(joined_files_clean, "All_flickr_grid_returns.csv")
```

There we have it. Hopefully this code works perfectly for you…….like
that ever happens though :)

Now the big question is, what areas are you interested in getting Flickr
data from?
