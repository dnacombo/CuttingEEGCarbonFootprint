install.packages('geosphere')
install.packages('ggmap')
library(ggmap)
library(geosphere)
register_google(key = "") #you need to request your own google maps api key (free): https://developers.google.com/maps/documentation/javascript/get-api-key

##########Assumptions
###As a comprehensive overview of every individual’s travel is impossible, we make a series of simplifying assumptions and decisions. These can be modified as needed at the proposed evaluation. Changes which materially affect the cost will be submitted to GB for approval. The simplifying assumptions used in the below calculations are as follows:
###  -We include only travel beyond Great Britain
###  -We make the simplifying assumption that travel beyond Great Britain is all air travel. Although this is certainly not always true (e.g. Paris), other assumptions below will provide a lower bound on flight distances
###  -All flights depart from London (although this can easily be modified)
###  -We treat all destinations as if they have airports
###  -If a precise city was unknown, we chose the centroid of the country
###  -We compute flight distance ‘as the crow flies’ - The shortest geodesic distance
###   from London to the destination city

#########   Methods
###   -For data visualization, computation of travel distance and all numerical operations we use the software R and selected packages (ggmaps and geocode)
###   -We convert flight distance into flight hours by assuming a cruising speed of 780 km/h
###   -We convert flight hours into carbon tonnage by the website ‘cooleffect’, a not-for-profit carbon offsetting charity: https://www.cooleffect.org/content/travel-offset
###   -We then convert carbon tonnage to a carbon offsetting price by assuming the Cool Effect rate of $8.46 per tonne of CO2, which is then converted to GBP by currency conversion rate at the time of offsetting.


##### Travel destinations
########################
#Here one should load a file, such as a .txt or .csv, with all names of destination cities/countries
destinations <- read.csv("destinations.csv",header=F)        #list of destination cities/countries
destinations<-as.character(destinations[,1])

#Or make a list like this by hand 
#destinations<-c('Boston','Madagascar','Stockholm','Melbourne')

#Then you can use geocode to compute the compute latitude and longitude.
#This requires a google API key. However, if so inclined one could look latitude and
# longitude up manually
location_codes<-geocode(destinations)              
                                                 
#create matrix for latitude/longitude 'from' (always assumed as london) and 'to' computed
lat_from<-rep(51.5074,times=NROW(destinations))  #london as 'from'
lon_from<-rep(0.1278,times=NROW(destinations))   #london as 'from'
lat_to<-location_codes$lat
lon_to<-location_codes$lon
travel_matrix<-data.frame(lat_from,lon_from,lat_to,lon_to)

#if you don't have a google api yet and just want to try, this code generates the matrix 'by hand'
# travel_matrix<-data.frame(matrix(c(51.5074,51.5074,51.5074,51.5074,0.1278,0.1278,0.1278,0.1278,42.36008,-18.76695,59.32932,-37.81363,-71.058880,46.869107,18.06858,144.963058),ncol=4))
# colnames(travel_matrix)<-c('lat_from','lon_from','lat_to','lon_to')

#Create a plot of all trips  
ggmap(get_map(location=c(-150,-40,160,70),zoom=3))+geom_segment(data=travel_matrix, aes(x=lon_from, y=lat_from, xend=lon_to, yend=lat_to),alpha=.9,colour='#1C86EE',lwd=1)+xlab('Longitude')+ylab("Latitude")+theme_grey(base_size = 17)+ggtitle('All travel')
ggsave('plot.jpg',width=10,height=6)

##########compute carbon offsetting##########
#############################################
#Parameters to compute carbon per kilometer 
cruise_speed<-780

#Compute geodesic  distance for 
point_from<-as.matrix(cbind(travel_matrix$lon_from,travel_matrix$lat_from))
point_to<-as.matrix(cbind(travel_matrix$lon_to,travel_matrix$lat_to))
travel_matrix$distance_km<-distCosine(point_from, point_to, r=6378137)/1000
travel_matrix$flighthours<-travel_matrix$distance_km/cruise_speed
travel_matrix$destinations<-destinations

#below we transform the flight hour into tonnages. The ORDER MATTERS. The messiness is because a 6 hour flight
#costs less carbon than 6 one hour flights (take-off)
#first we create a duplicate of flight hours
travel_matrix$co2cost<-travel_matrix$flighthours

#Now we transform flight hours into CO2 tonnages. Travel is 'binned' for simplicity, as per
#Offset pricing: https://www.cooleffect.org/content/travel-offset
travel_matrix$co2cost<-ifelse(travel_matrix$co2cost<4,2.6,travel_matrix$co2cost)
travel_matrix$co2cost<-ifelse(travel_matrix$co2cost>13,17.94,travel_matrix$co2cost)
travel_matrix$co2cost<-ifelse(travel_matrix$co2cost<7&travel_matrix$co2cost>4,6.60,travel_matrix$co2cost)
travel_matrix$co2cost<-ifelse(travel_matrix$co2cost<13&travel_matrix$co2cost>10,15.99,travel_matrix$co2cost)
travel_matrix$co2cost<-ifelse(travel_matrix$co2cost<10&travel_matrix$co2cost>7,11.17,travel_matrix$co2cost)
travel_matrix$co2_tonne<-travel_matrix$co2cost/8.46

#total co2 cost in dollars
sum(travel_matrix$co2cost)
#Convert dollar to GBP
gbp_co2<-0.78*travel_matrix$co2cost
sum(gbp_co2)

#Create summaries:
totaldistance<-sum(travel_matrix$distance_km)

paste('The total distance travelled is',round(totaldistance,2),'km')

paste('The total CO used is',round(sum(travel_matrix$co2_tonne),2),'tonnes')

paste('The total cost to carbon offset all travel is',round(sum(travel_matrix$co2cost*0.78),2),'gbp')






