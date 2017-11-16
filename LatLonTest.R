
#ZipLatLon<-read.csv(file="C:\\R Local Repository\\LatLonTest\\ZipLatLon.csv",head=TRUE,sep=",")

require(ggmap)
require(zipcode)
require(maps)
require(mapdata)

data("zipcode") # Lat Long in Degrees

### To calculate a geographic midpoint 
### http://www.geomidpoint.com/example.html

### Plot coordinates points
###https://www.darrinward.com/lat-long/?id=5a0db2b8e621c7.19133574

### Lookup a coordinate s
###https://www.latlong.net/

### MID POINT (CENTER OF GRAVITY)
#1. Convert cooridnates to from degrees to  decimal format
#2. Convert coordianates from decimal to Radians (Multiply lat long by PI/180)
#3. Weight factors if needed (Assuming equal wieghts)
#3a. totalwieght = number of cities n = w1 + w2 + w3 +...+wn

#4. Convert Lat Long to carteisan (x,y,z) Coordiantes
#4a. X1 = cos(lat1) *cos(long1)
#4b. Y1=cos(lat1)* sin(long1)
#4c. Z1=sin(lat1)

#5. Compute combined wieghted cartesian coordinate
#5a.     X = (X1*w1 + X2*w2 + X3*w3)/totalweight
#5b.     Y = (Y1*w1 + Y2*w2 + Y3*w3)/totalweight
#5c.     Z = (Z1*w1 + Z2*w2 + Z3*w3)/totalweight

#6. Convert cartesian coordiante to latitude and longitude for the midpoint
#6a. Long = atan2(y,x)
#6b. Hyp=sqrt(x *x + y*y)
#6c. Lat=atan2(z,hyp)

#7 Convert midpoitn lat and long from radians to degrees

#lat=Lat *(180/pi)
#long = Long * (180/pi)


#### Test Case

### Chicago, St. Louis, San Francisco

#City [Lat, Long] Decimal Format
#Chicago[41.878114, -87.629798]
#St. Louis[38.627003,-90,199404]
#San Francisco[37.774929,-122.419416]

# Convert to Radians (Multiply by pi/180)

Chicago_Lat = 41.878144 *(pi/180)
Chicago_Long = -87.629798 * (pi/180)

StLouis_Lat = 38.627003 *(pi/180)
StLouis_Long =-90.199404 *(pi/180)

SanFrancisco_Lat=37.774929 *(pi/180)
SanFrancisco_Long = -122.419416 *(pi/180)

w1=1
w2=1
w3=1
wn=3


#4. Convert Lat Long to carteisan (x,y,z) Coordiantes

XChicago=cos(Chicago_Lat) *cos(Chicago_Long)
YChicago=cos(Chicago_Lat) *sin(Chicago_Long)
ZChicago=sin(Chicago_Lat)

XStLouis=cos(StLouis_Lat) *cos(StLouis_Long)
YStLouis=cos(StLouis_Lat) *sin(StLouis_Long)
ZStLouis=sin(StLouis_Lat)

XSanFrancisco=cos(SanFrancisco_Lat) *cos(SanFrancisco_Long)
YSanFrancisco=cos(SanFrancisco_Lat) *sin(SanFrancisco_Long)
ZSanFrancisco=sin(SanFrancisco_Lat)


X= (XChicago+XStLouis+XSanFrancisco)/3
Y= (YChicago+YStLouis+YSanFrancisco)/3
Z= (ZChicago+ZStLouis+ZSanFrancisco)/3


#Convert cartesian coordiante to latitude and longitude for the midpoint
Long = atan2(Y,X)
Hyp=sqrt(X *X + Y*Y)
Lat=atan2(Z,Hyp)


#Convert midpoint lat and long from radians to degrees

Midlat=Lat *(180/pi)
Midlong = Long * (180/pi)

#Midlat  40.52428
#Midlong -100.2306


usa <- map_data("usa")
gg1<-ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)

#Chicago[41.878114, -87.629798]
#St. Louis[38.627003,-90,199404]
#San Francisco[37.774929,-122.419416]
#Midpoint[40.52428,-100.2306]



labs <- data.frame(
  long = c(-87.629798,-90.199404,-122.419416,-100.2306),
  lat = c(41.878114,38.627003,37.774929,40.52428),
  names = c("Chicago", "St. Louis","San Francisco","Midpoint-Center Gravity"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 4) +
  geom_point(data = labs, aes(x = long, y = lat), color = "blue", size = 4) 

########################################################################
########################################################################
#     _____                   _                                  
#   / ____|                 | |                                 
#  | |  __  ___   ___   __ _| | ___   _ __ ___   __ _ _ __  ___ 
#  | | |_ |/ _ \ / _ \ / _` | |/ _ \ | '_ ` _ \ / _` | '_ \/ __|
#  | |__| | (_) | (_) | (_| | |  __/ | | | | | | (_| | |_) \__ \
#  \_____|\___/ \___/ \__, |_|\___| |_| |_| |_|\__,_| .__/|___/
#                      __/ |                        | |        
#                     |___/                         |_|        
############################################################################
##########################################################################
labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4)


#Google Map API daily limit 2,500 without license
mapdist('St. Louis', 'Chicago', mode = 'driving',output=c("simple")) # By City
dist<-mapdist('62040','63017',mode='driving',output=c("simple")) # By Zip Code








