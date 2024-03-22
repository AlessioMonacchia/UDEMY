##############################################################
########## Querying and exploring occurence data #############
##############################################################

# Now that we've gone over the basics of R, we will jump right into 
# a bit more advanced content. You will find that a lot of R programming
# doesn't necessrily have to do with writing your own fucntions and massive
# amounts of code, which can scare people away from it upon first sight
# of a large script. Instead, it is more copy/pasting and modifying existing
# code and plugging in your own data

# we will begin by installing and opening necessary packages
install.packages("rgbif") # provides an interface to the Glopbal Biodiversity Information Facility (GBIF) database. This package allows users
                          # to query and access biodiversity data from GBIF directly. Data are like species occurrence records, search for specific taxonomic groups, 
                          # filter data based on various categories (such as geographic location or temporal range) and perform analysis.

install.packages("mapview") # provides interactive mapping capabilities. It allows to create interactive maps with spatial data. Users can plot 
# spatial data such as points, lines, and polygons on interactive maps. 

install.packages("scrubr") # provides functions for cleaning and preprocessing data (manage missing values, outliers, inconsistencies..)

install.packages("sp") # provides classes and methods for handling spatial data (points, lines, polygons and grids in R). It introduces classes
                       # like SpatialPoints, SpatialLines, SpatialPolygons, and SpatialGrids. These classes comes with methods for spatial operations, transformations,
                       # and analyses.

# Now we open the packages we installed, and one from the previous lesson
library(rgbif)
library(mapview)
library(scrubr) # NA, used to 'clean' data
library(sp)  # deals with spatial data (simple features), useful for data type conversion
library(dplyr) # provides functions for data manipulation and transformation. Some key functions: 1) filter() used to subset df rows based on specific
               # conditions. 2) mutate() used to create new variables or modify existing variables in a df. 3) select() enables users to choose specific columns 
               # from a df. 4) arrange() used to reorder rows of a df based on one or more variables. 5) summarize() aggregates data and calculates summary statistics.

setwd("/home/alessio/Udemy_Ecology_in_R/Lesson 2 Occurence and species Density")
# The rgbif package allows us to access and query species occurence data
# from the Global Biodiversity Information Faciity - a database which 
# accumulates species data from literature, museum records, and vetted
# citizen science records (such as those from iNaturalist)

# In order to pull data from GBIF, we need to use the function occ_search
# However, before doing so, it is usefull to find the quantity of records
# and to make sure the taxonomy is correct. Lets start by pulling species
# data from the family Pythonidae as an example

# ~~~~~~~~~~~~~~~~~~~~~GET THE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use the question mark and occ_search to check examples then copy and paste
# relevant code for what we want to do
?occ_search

# Pull records on a family from GBIF
# this code is retrieving a suggested key for the taxonomic name "Pythonidae" at the family level from gbif
key <- name_suggest(q='Pythonidae', rank='family')$data$key[1]
# Show metadata of records for Pythonidae in the database
# this code is retrieving the total count of occurrences for a specific taxonomic key in gbif
occ_search(taxonKey=key, limit=0)$meta$count
# Pulls your data from GBIF, limit to 200 records as an example dataset
# this code fetches (prendere, portare) occurrence data for a specific taxonomic key from gbif setting the limit to 200 occurrences
spdat <- occ_search(taxonKey = key, return = "data", limit = 200)
#pull out the data file
spdat <- spdat$data
#view the data that was returned
View(spdat)

# ~~~~~~~~~~~~~~~~~~~~~CLEAN THE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~


# WOW thats a lot of data! However, all we need right now is the latitude,
# Longitude, and Species name. Lets use the select() function from dplyr
# and store it in a data frame object called dat
# make sure you use the correct column names when using select()
dat <- select(spdat, decimalLatitude, decimalLongitude, species)


# lets change the names of the columns so they are "lat", "lon" and "species"
colnames(dat) <- c("latitude", "longitude", "species")

# We should also clean the data of any useless NA values
dat <- na.omit(dat)


# now we use our "spatial data" (lat long coordinates) to create a
# Spatial Data Frame object that allows R to read and map it
# to do this properly, we need to add a Coordinate Reference System (CRS) 
# value based on the GPS coordinate system. Typically this is WGS84, 
# but lets check anyways! The table function is extremely useful, 
# as it will show you all the valuesin any given column, 
# and show you the frequency at which they occur

table(spdat$geodeticDatum) # this shows all our GPS data is WGS84

# For another example, lets get a count for how many occurence records
# we have per species in our "dat" data frame
table(dat$species) #show how many data records we have for each species


# occasionally, we get unlikely or even impossible coordinates that were modified
# or improperly added into the database. Although GBIF is well-vetted, mistakes
# are always possible - for example, a python  occuring in the arctic would 
# be unlikely/impossible
# to omit these points, we use coords_unlikely() and coord_impossible()
# functions from the scrubr package, using "pipes" %>% from dplyr package
# think of %>% as saying "then" 

#clean your occurence records (search scrubr package for more options)
dat<-dframe(dat) %>% coord_impossible()
dat<-dframe(dat) %>% coord_unlikely()

# if you noticed, this function changed our lon and lat column names to
# "longitude" and "latitude". we could change this back with colnames()
# but lets instead just view our dataframe, and acknowledge what happened
View(dat)

#GREAT! Now we have some pretty clean data to work with!


# ~~~~~~~~~~~~~~~~~~~~~ MAKE AND MAP SPATIAL OBJECTS ~~~~~~~~~~~~~~~~~~


# Back to our spatial data, this is where the "sp" package comes in
# lets create our spatial object and store it in a 
# spatialpoints object called "sp".
# to do this, we combine or "cbind" our coordinates using the 
# function "SpatialPoints()"
# for GPS coordinate system WGS84 in lat long, we use the 
# CRS projection or "proj4string" +init=espg:4326
sp <- SpatialPoints(coords=cbind(dat$longitude, dat$latitude), 
                    proj4string = CRS("+init=epsg:4326"))

sp # this calls sp to see what we've created


# now that we have our spatialdata object, lets add our species date to it
# and store it in a column feature called "species"
sp$species <- dat$species


# NOW FOR THE MOMENT YOU'VE BEEN WAITING FOR! LETS MAP THE SPECIES DATA!
# to do this, we will use the mapview() function from the mapview package.
# this function plots spatial data of all sorts on interactive leaflet maps
# and is a quick, convinient way to explore spatial data.
# NOTE: the default map is CartoDB.Positron, however you can use the layers
# tab under the zoom in/out button to change it to other maps layers.

mapview(sp) #plot our spatialpoints object "sp"


# Did you notice something weird!? THERES PYTHONS IN FLORIDA!
# Pythons are an "old world" (Eastern hemispher) species, and there are no
# pythons native to the United states. However, these occurences are real
# and not a product of bad ID or unclean data. They are invasive species.
# So what if we only want native species occurence data? Well, fortunately,
# GBIF allows you to isolate those records. So lets do all this again, but
# in a more streamlined, fast process!!!

#~~~~~~~~~~~~~~~~~~~ LET'S TRY THAT AGAIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In order to omit invasive/non-native species, we use the argument 
# "establishedMeans = "NATIVE" in our occ_search

key <- name_suggest(q = "Pythonidae", rank = "family")$data$key[1]
occ_search(taxonKey=key, limit=0)$meta$count
# Pulls your data from GBIF, limit to 200 records with NO INVASIVE SPECIES
spdat.new <- occ_search(taxonKey = key, 
                        limit = 200, establishmentMeans = "Native")
spdat.new <- spdat.new$data


# create our new dataframe
dat.new <- select(spdat.new, decimalLatitude, decimalLongitude, species)

# rename our columns
colnames(dat.new) <- c("latitude", "longitude", "species")

# remove NA values
dat.new <- na.omit(dat.new) #wow that a lot of NA values :(

#remove bad coords
dat.new<-dframe(dat.new) %>% coord_impossible()
dat.new<-dframe(dat.new) %>% coord_unlikely()

# make our spatial object
sp.new <- SpatialPoints(coords=cbind(dat.new$longitude, dat.new$latitude), 
                    proj4string = CRS("+init=epsg:4326"))

# add our species data
sp.new$species <- dat.new$species

# view our updated data!
mapview(sp.new) 

# oh no! now instead of burmese pythons there are museum specimens of ball
# pythons in the US! THIS IS IMPORTANT! you need to always be 
# vigillant about your data and you need to clean it accordingly. 
# This is exactly why we do exploratory analysis like this!

# now we will manually remove these occurence records by calling
# and removing a "subset" of our original data, "spdat.new"

# lets locate those records that are in the US using the column
# named "country" using the function "subset()"
# but first, lets create a table showing how many records
# fall within the US
table(spdat.new$country) #7 records! Boooooo! 

# now lets remove that subset of data. the subset symbol
# "==" means "which is" wheras the symbol "!=" means "is not"
# lets make a subset of all the records that 'are not' in 
# the US and store it in a data frame named "spdat.native"
spdat.native <- subset(spdat.new, 
                       country != "United States of America") #all US records

#check the countries remaining in the dataset
table(spdat.native$country)

#---------------------------------------------------------------------------

# lets do a "quick and dirty" plot of them to verify we've removed
# what we wanted to, and kept what we wanted to
spdat.native <- select(spdat.native, decimalLongitude, decimalLatitude, species)

# remove NA
spdat.native <- na.omit(spdat.native)



# convert to spatialpoints object
sp.native <- SpatialPoints(coords = cbind(spdat.native$decimalLongitude,
                                      spdat.native$decimalLatitude),
                       proj4string = CRS("+init=epsg:4326"))

#re-add the species names
sp.native$species <- spdat.native$species

#map the points!
mapview(sp.native) #Beautiful! No more US invasive records!



# ~~~~~~~~~~~~~~~~~~~~~ NOW YOU TRY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Try to do the same with a family of your own choice. Simply
# fill in the quotations after "family"q = " with your choice and run the code
# NOTE: occ_search has amazing capabilities in subsetting data, I encourage
# you to explore the dynamics of the function, such as pulling data from
# only certain elevation ranges, countries, etc. change the limit as necessary

key <- name_suggest(q='Felidae', rank='family')$data$key[1]
occ_search(taxonKey=key, limit=0)$meta$count
spdat.test <- occ_search(taxonKey = key, return = "data", limit = 200)
spdat.test <- spdat.test$data


dat.test <- select(spdat.test, decimalLongitude, decimalLatitude, species)
colnames(dat.test) <- c("longitude", "latitude", "species")

dat.test <- na.omit(dat.test)

# dat.test<-dframe(dat.test) %>% coord_impossible()
# dat.test<-dframe(dat.test) %>% coord_unlikely()

sp.test <- SpatialPoints(coords = cbind(dat.test$longitude, dat.test$latitude),
                         proj4string = CRS("+init=epsg:4326"))

sp.test$species <- dat.test$species

mapview(sp.test)


# To finish this tutorial, let's save the spdat.test data as a csv file 
# In case we want to use it later without having to re-download them all!
write.csv(spdat.test, "GBIF_data.csv")



# END


