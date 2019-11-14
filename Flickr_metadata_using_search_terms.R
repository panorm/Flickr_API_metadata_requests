#SEARCHING FLICKR USING ALL TEXT
# Purpose: Create a spreadsheet of photo data from Flickr photos from a search term using R.
# Results based on search dates and search terms
# Adapted from script created by Chelsey Walden-Schreiner

# Install and require necessary R packages.
library(RCurl) 
library (rjson)
library(RJSONIO)


# Set working directory with path name in quoation marks and / instead of \
setwd("E:/PhD_files/Scripts/R/API_pulls/Flickr_text_fix")




#SELECTING THE DATES
#---------------------------------------------------------------------------------------------------------
#FOR THIS NEXT LITTLE BIT PUT IN YOUR START AND FINISH DATES
#BEST IF YOU MAKE THE END DATES 7 DAYS AFTER THE START DATES
start_dates <- as.data.frame(seq(as.Date("2019/1/1"), as.Date("2019/5/1"), 'weeks'))
end_dates <- as.data.frame(seq(as.Date("2019/1/8"), as.Date("2019/5/8"), 'weeks'))
dates <- cbind(start_dates, end_dates)
id <- rownames(dates)
dates <- cbind(id=id, dates)
colnames(dates) <- c('ID','start_dates', 'end_dates')

#---------------------------------------------------------------------------------------------------------





Flickr_return <-function(x){
# Flicker API access keys 
# Key must have single quotations around it
# Example: 'A8MNR7997LLOP123'
api_key = '149b3a5399f39503f43b5bae4ae2a9d9'
api_secret = '12acd8fe8208905e'

# Create function to return URL using flickr.photos.search 
# More information at https://www.flickr.com/services/api/flickr.photos.search.html
getURL = function(api_key, minDate, maxDate, search_text, pageNum){
  root = 'https://api.flickr.com/services/rest/?method=flickr.photos.search&'
  u = paste0(root,"api_key=",api_key, "&min_taken_date=",  minDate,"&max_taken_date=", maxDate,
             "&text=", search_text,"&extras=geo%2C+date_taken%2C+date_upload%2C+views%2C+tags%2c+url_o+&per_page=500&page",
             pageNum, "&format=json&nojsoncallback=1" )
  return(URLencode(u))
}

#Create function to return URL to get Exif data from image metadata
getURL2 = function(api_key, id, secret){
  root2 = 'https://api.flickr.com/services/rest/?method=flickr.photos.getExif&'
  u2 = paste0(root2,"api_key=",api_key,"&photo_id=",id,"&secret=",secret,"&format=json&nojsoncallback=1" )
  return(URLencode(u2))
}

#setting up search terms for twitter scrape
search_text = "murwillumbah"

# Set date search parameters.
# Use date format 'YYYY-MM-DD' with single quotations. 
minDate = dates$start_dates[k]
maxDate = dates$end_dates[k]

#First call for specified date and text
##This will return the URL that contains the information based on search variables above
##The remainder of the code will then read through the information and write it to a data frame
getURL(api_key, minDate, maxDate, search_text, pageNum=1)

#Read data returned from first call 
target = getURL(api_key, minDate, maxDate, search_text, pageNum=1)
data = fromJSON(target)

# Get the total number of photo records returned using the current search parameters.
total = as.numeric(data$photos$total) 

# Number of pages of records returned using search parameters.
numPages = data$photos$pages
length = as.data.frame(list(1:numPages))
pageNum = 2

# Create empty dataframe to populate with data.
df = NULL 
# For each page of results, from the first to maximum page number extract photo information.
for (j in 1:numPages){  
  pageNum = j 
  target.loop = getURL(api_key, minDate, maxDate, search_text, pageNum)
  data1 = fromJSON(target.loop)
  numPhotos = length(data1$photos$photo)
  
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
write.csv(df, paste0(dates$ID[k], "_Output_data.csv"))
}

#This is where I have been running the function
for (k in 1:nrow(dates)){
  tryCatch(
    {
      (Flickr_return(k))
      readLines(con = dates, warn = FALSE)
    },  
    error = function(cond){
      return(NA)
      next()
    }
  )
}
