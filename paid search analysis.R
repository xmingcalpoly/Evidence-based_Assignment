#setting work station
setwd("~/desktop/TechStyle/working/R")

#loading datasets, subsetting paid search channel
response = read.csv("Response (2).csv")
stimulation = read.csv("Stimulation (2).csv")
response = response[which(response$Channel=="Paid Search"),] 
stimulation = stimulation[which(stimulation$Channel=="Paid Search"),] #stimulaiton dataset has no washington, but has unknown

response$Date = as.Date(response$Date)     #standardize the date variable as date format
response$Year = format(response$Date, "%Y")   # isolating year, month, and day from original date
response$Month = format(response$Date, "%m")
response$Day = format(response$Date, "%d")


stimulation$Date = as.Date(stimulation$Date)
stimulation$Year = format(stimulation$Date, "%Y")
stimulation$Month = format(stimulation$Date, "%m")
stimulation$Day = format(stimulation$Date, "%d")

#calcalculating the # of impressions caused by per $ dollar
stimulation$Impressions_per_dollar =  stimulation$Impressions / stimulation$Marketing.Spend

merge_data = merge(stimulation, response, by = c("Geography","store_label","Date","Year","Month","Day","Channel","Sub.Channel"))


#add new region variable
ne.vars = c("BOSTON-MANCHESTER","NEW YORK","PHILADELPHIA")
eastnorcen.vars = c("CHICAGO","DETROIT","REST OF EAST NORTH CENTRAL")
westsocen.vars = c("DALLAS-FT. WORTH","HOUSTON","REST OF WEST SOUTH CENTRAL")
soatl.vars = c("ATLANTA","REST OF SOUTH ATLANTIC","MIAMI-FT. LAUDERDALE","WASHINGTON DC")
eastsocen.vars = c("REST OF EAST SOUTH CENTRAL")
westnocen.vars = c("REST OF WEST NORTH CENTRAL")
mountain.vars = c("DENVER","REST OF MOUNTAIN")
pacific.vars = c("SAN FRANCISCO-OAK-SAN JOSE","LOS ANGELES","REST OF PACIFIC")

Region = character(nrow(merge_data))
Region[which(merge_data$Geography %in% ne.vars)] = "Northeast" 
Region[which(merge_data$Geography %in% eastnorcen.vars)] = "East North Central" 
Region[which(merge_data$Geography %in% westsocen.vars)] = "West South Central" 
Region[which(merge_data$Geography %in% soatl.vars)] = "South Atlantic" 
Region[which(merge_data$Geography %in% eastsocen.vars)] = "East South Central" 
Region[which(merge_data$Geography %in% mountain.vars)] = "Mountain" 
Region[which(merge_data$Geography %in% pacific.vars)] = "Pacific" 
Region[which(merge_data$Geography %in% westnocen.vars)] = "West North Central" 
Region[which(Region =="")] = NA
merge_data$Region = Region

#Using ln function to normalize vip.activation
merge_data$Log_VIP.Activations = log(merge_data$VIP.Activations)


#create another dataset by using the original datasets
#response_paid_search = response[which(response$Channel=="Paid Search"),]

#aggregate vip in response based on geography, date, subchannel
response_vip_aggregate = aggregate(response$VIP.Activations, by=list(geo=response$Geography, date=response$Date, sub_channel=response$Sub.Channel), FUN = sum)
names(response_vip_aggregate)[4] = "VIP.Activations"
head(response_vip_aggregate)

#merge vip.activation with stimulation without considering promos and customer segments
merge_vip_stimulation = merge(stimulation,response_vip_aggregate,by.x=c('Date','Geography','Sub.Channel'),by.y = c('date','geo','sub_channel'))
head(agg.merge)

#calculating spend per vip.activation
spnd.per.vip = data.frame(date = merge_vip_stimulation$Date,geo = merge_vip_stimulation$Geography,sub_channel=merge_vip_stimulation$Sub.Channel ,spend_per_vip = merge_vip_stimulation$Marketing.Spend/merge_vip_stimulation$VIP.Activations)
spnd.per.vip$date = as.Date(spnd.per.vip$date)

# adding spend per vip into the merge_data
merge_data$Spend.per.Vip = 1
geos = as.character(unique(merge_data$Geography))
dates = as.Date(unique(merge_data$Date))
sub_channel = as.character(unique(merge_data$Sub.Channel))
for( i in 1:length(dates)){
  for( g in 1:length(geos)){
    for( k in 1:length(sub_channel)){
      s.v = spnd.per.vip$spend_per_vip[which(spnd.per.vip$date==dates[i]&spnd.per.vip$geo==geos[g]&spnd.per.vip$sub_channel==sub_channel[k])]
      merge_data$Spend.per.Vip[which(merge_data$Date==dates[i]&merge_data$Geography==geos[g]&merge_data$Sub.Channel==sub_channel[k])] = s.v
    }
   
  }
}

merge_data$Marketing.Spend = merge_data$Spend.per.Vip*merge_data$VIP.Activations
merge_data$Impressions = merge_data$Impressions_per_dollar*merge_data$Marketing.Spend
write.csv(merge_data, "paid_search_merged.csv")

#above is how the paid search channel data is cleaned and how to combine two datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(merge_data)
fit = lm(merge_data$Log_VIP.Activations~Impressions + + Sub.Channel+ Marketing.Spend + Region + Customer.Segment.y + Promo.y, data = merge_data)
summary(fit)
