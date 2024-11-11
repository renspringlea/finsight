#Set up environment
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable
library(measurements) #For converting units
library(stringr) #For converting units
library(viridis) #To help graphing
library(gridExtra) #To help graphing
library(gt) #For generating tables
library(stringr) #For string manipulation
library(scales) # For visualisation
library(data.table) # For data manipulation
library(lubridate) # For data manipulation
library(rnaturalearth) #For world map international borders
library(rnaturalearthdata) #For world map international borders
library(terra) #For spatial data analysis
library(sf) #For spatial data analysis
library(tidyterra) #For graphing spatial data
library(flowmapper) #For mapping trade

##################
### Production ###
##################

# Load parameters
# Finfish biological parameters (especially slaughter weight)
parameters_finfish <- read.csv("~/finsight/parameters/Finfish numbers - Data.csv")

# Shrimp biological parameters
# These aren't actually useful for Europe, which isn't a major shrimp producer
# But I'm keeping the code here so that it's easier to expand to shrimp-producing
# countries if we want to in the future
parameters_shrimp <- read.csv("~/finsight/parameters/shrimp_parameters.csv")

# Custom species groupings
species_custom <- read.csv("~/finsight/parameters/custom_species_categorisation.csv")

# The countries that we want to include
production_countries <- read.csv("~/finsight/parameters/production_countries.csv")

# Load FAO's country and species definitions
countries <- read.csv("~/finsight/data/CL_FI_COUNTRY_GROUPS.csv")
species <- read.csv("~/finsight/data/CL_FI_SPECIES_GROUPS.csv")


# Load FAO quantity and value production data
Aquaculture_Quantity <- read.csv("~/finsight/data/Aquaculture_Quantity.csv")
Aquaculture_Value <- read.csv("~/finsight/data/Aquaculture_Value.csv")

# Merge into a single dataframe of FAO data (both quantity and value)
fao <- rbind(Aquaculture_Quantity,Aquaculture_Value)

# For the countries and species data frames,
# get just the columns we want
# and rename to the names used in the production data frame (fao)
countries <- countries[,c("UN_Code","Name_En","Continent_Group_En","ISO2_Code")]
names(countries) <- c("COUNTRY.UN_CODE","Country","Continent","ISO2_Code")
species <- species[,c("X3A_Code","Name_En","Scientific_Name","Major_Group")]
names(species) <- c("SPECIES.ALPHA_3_CODE","Species","Species_binomial","Species_group")

# Some of the countries have long names, and we can shorten these
countries[which(countries$ISO2_Code=="GB"),"Country"] <- "United Kingdom"
countries[which(countries$ISO2_Code=="NL"),"Country"] <- "Netherlands"
countries[which(countries$ISO2_Code=="MD"),"Country"] <- "Moldova"

# In the list of countries that we want to include, add the English name of the country
production_countries <- merge(production_countries,countries,
                              by="ISO2_Code",
                              all.x=T,all.y=F)

# Add the country and species columns to the production data (fao)
# Country columns
fao <- merge(fao,countries,by="COUNTRY.UN_CODE",all.x=T,all.y=F)
# Species columns
fao <- merge(fao,species,by="SPECIES.ALPHA_3_CODE",all.x=T,all.y=F)
# Our custom species definitions
fao <- merge(fao, species_custom, by="Species",all.x=T,all.y=F)
# Set all species that we didn't include in one of our categories to "Other
fao[which(is.na(fao$Species_custom)),"Species_custom"] <- "Other"

# REstrict the FAO data frame to only the countries in the list
# of countries that we specified
fao <- fao[which(fao$ISO2_Code %in% production_countries$ISO2_Code,),]

# Get just the columns we want in the parameters data frame, and rename
parameters_finfish <- parameters_finfish[,c(1,3,5,7)]
names(parameters_finfish) <- c("Species","Harvest_weight_g","Harvest_age_mo","Mortality_rate")

# Convert mortality rate to a proportion (rather than a percentage)
parameters_finfish$Mortality_rate <- 0.01*as.numeric(
  gsub("%","",parameters_finfish$Mortality_rate))

# Convert life durations to years (rather than days or months)
parameters_shrimp$Harvest_age_years <- parameters_shrimp$Harvest_age_days/365.25
parameters_finfish$Harvest_age_years <- parameters_finfish$Harvest_age_mo/12

# Check that all species in the parameters data frame
# are in the production data
# These should all show "TRUE"
parameters_finfish$Species %in% unique(species$Species)
parameters_shrimp$Species_binomial %in% unique(species$Species_binomial)

# Get the English species name for shrimp
parameters_shrimp <- merge(parameters_shrimp,species,by="Species_binomial",
                           all.x=T,all.y=F)

# Keep only the parameter columns we want, and merge into a single
# parameter data frame
parameters_finfish <- parameters_finfish[,c("Species","Harvest_weight_g","Harvest_age_years","Mortality_rate")]
parameters_shrimp <- parameters_shrimp[,c("Species","Harvest_weight_g","Harvest_age_years","Mortality_rate")]
parameters <- rbind(parameters_finfish,parameters_shrimp)

# Add the biological parameters to the production data (FAO)
fao <- merge(fao,parameters,by="Species",all.x=T,all.y=F)

# Get only fish and crustaceans (i.e. remove rows corresponding to bivalves, algae, etc)
fao <- fao[which(fao$Species_group %in% c("PISCES","CRUSTACEA","INVERTEBRATA AQUATICA")),]

# Separate again into quantity and value
fao_quantity <- fao[which(fao$MEASURE=="Q_tlw"),]
fao_value <- fao[which(fao$MEASURE=="V_USD_1000"),]

# Calculate individuals
# Note that we only do this for the rows where the measure is in tonnes
# (i.e. the rows that came from the quantity file rather than the value file)
# Individuals slaughtered = live production weight (converted from tonnes to grams) divided by average harvest weight
fao_quantity$Individuals_slaughtered <- (fao_quantity$VALUE*10^6)/fao_quantity$Harvest_weight_g
# Individuals hatched = individuals slaughtered divided by mortality rate
fao_quantity$Individuals_hatched <- fao_quantity$Individuals_slaughtered/fao_quantity$Mortality_rate
# Individuals alive at any one time = individuals slaughtered multiplied by harvest age
fao_quantity$Individuals_inventory <- fao_quantity$Individuals_slaughtered*fao_quantity$Harvest_age_years

# Get the most recent year in the data
production_recent_year <- max(fao_quantity$PERIOD)

# Restrict the data to only that year
# We'll keep "all years" data as fao_allyears for use later
fao_allyears <- fao_quantity
fao_quantity <- fao_allyears[which(fao_allyears$PERIOD==production_recent_year),]

# Create a sensible order of our custom species categories
# i.e. we'll turn "Species_custom" into a factor, ordered according to production weight
# with "Other" at the end
# This just makes the visualisations a bit nicer and more intuitive and consistent
custom_species_order <- aggregate(Individuals_slaughtered~Species_custom,FUN=sum,na.rm=T,data=fao_quantity)
custom_species_order_vector <- custom_species_order[order(-custom_species_order$Individuals_slaughtered),"Species_custom"]
custom_species_order_vector <- custom_species_order_vector[-which(custom_species_order_vector=="Other")]
custom_species_order_vector <- c(custom_species_order_vector,"Other")
fao_quantity$Species_custom <- factor(fao_quantity$Species_custom,
                                      levels=custom_species_order_vector)
fao_allyears$Species_custom <- factor(fao_allyears$Species_custom,
                                      levels=custom_species_order_vector)


####################################
### Visualise data for main page ###
####################################
# Aggregate individuals slaughtered by country
production_individuals <- aggregate(Individuals_slaughtered~ISO2_Code,FUN=sum,data=fao_quantity,na.rm=T)
production_individuals <- merge(production_individuals,production_countries,
                                by="ISO2_Code",
                                all=T)

# Make a map!
# Obtain the spatial vector data of all international borders
# The scale = 50 ensures we also obtain the very small countries
# This is essential for Malta!
spatial_countries <- ne_countries(scale=50)

# Restrict that set of international borders to the countries that
# we have specified
spatial_countries <- spatial_countries[which(spatial_countries$iso_a2_eh %in% production_countries$ISO2_Code),]

# Add the aggregated production data to the spatial vector data
spatial_countries <- merge(spatial_countries,production_individuals,
                           by.x="iso_a2_eh",by.y="ISO2_Code",
                           all=T)
# Produce a pretty map
map_production <- ggplot() +
  geom_spatvector(aes(fill=Individuals_slaughtered),data=spatial_countries) +
  xlim(-10,45) + 
  ylim(35,70) +
  scale_fill_viridis_c(trans = "log",
                       option="rocket", 
                       direction=-1, 
                       labels = unit_format(unit = "M", scale = 1e-6),
                       breaks = breaks_log(n=6)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        plot.background = element_rect(fill="white",colour="white")) +
  guides(fill = guide_colorbar(barwidth = 25))+
  labs(title="Individuals slaughtered",
       subtitle=production_recent_year,
       fill=NULL)

# Save to file
ggsave("~/finsight/assets/images/map_production.png",
       map_production,
       width=6,height=6)

# Make a table for the website main page
# Perform a similar aggregation,
# except that it's individuals by country and by species category
production_individuals_species <- aggregate(Individuals_slaughtered~ISO2_Code+Species_custom,FUN=sum,data=fao_quantity,na.rm=T)

# Get the country names rather than the ISO2 codes, to make the table
# easier for humans to read
production_individuals_species <- merge(production_individuals_species,
                                        spatial_countries[,c("iso_a2_eh","name")],
                                        by.x="ISO2_Code",by.y="iso_a2_eh",
                                        all.x=T,all.y=F)

# Create a data frame with just the variables that we want
production_individuals_species_long <- data.frame("Species"=production_individuals_species$Species_custom,
                                                  "Country"=production_individuals_species$name,
                                                  "Individuals_slaughtered"=production_individuals_species$Individuals_slaughtered)

# Round the individuals slaughtered to the nearest 100,000
production_individuals_species_long$Individuals_slaughtered <- round(production_individuals_species_long$Individuals_slaughtered/
                                                                      (10^6),1)
# Reshape to wide format for a swanky-looking table
prodtable <- reshape(production_individuals_species_long,
                     idvar="Country",
                     timevar="Species",
                     direction="wide")

# Set all cells that don't have a production value to 0 rather than NA
# (these are actually 0 production for that country and species, the NAs
# appeared by default when converting from long to wide)
prodtable[is.na(prodtable)] <- 0

# Change the column names to just the species category
# with units "millions" (since we divided by 1 million when rounding)
names(prodtable) <- gsub("Individuals_slaughtered.","",names(prodtable))
names(prodtable)[-1] <- paste0(names(prodtable)[-1]," (M)")

# Calculate total individuals slaughtered for country
prodtable$`Total (M)` <- rowSums(prodtable[,-1])

# Order the table by the total individuals slaughtered
prodtable <- prodtable[order(-prodtable$Total),]

# Save to file
write.csv(prodtable,"~/finsight/_data/prodtable.csv",row.names=F)

##############
### Retail ###
##############

# Import all csv files with "retail" in the name in our data
# and assign them to the global environment
filenames <- list.files("~/finsight/data/")
filenames_retail <- filenames[which(grepl("retail",filenames))]
for (i in filenames_retail){
  csv_tmp <- read.csv(paste0("~/finsight/data/",i),sep=";")
  assign(substr(i,1,nchar(i)-4), csv_tmp)
}

# Bind into a single data frame
eu_retail <- rbindlist(mget(ls(pattern = "^data_eu_retail.*")))

# Convert price per kg to numeric
# this produces "NAs" when there is no data, which is correct
eu_retail$PRICE.PER.KG..EUR. <- as.numeric(eu_retail$PRICE.PER.KG..EUR.)

# Import the list of retail products that we want to visualise
retail_products <- read.csv("~/finsight/parameters/retail_products.csv")

# Restrict to the products that we intend to visualise
# (matches the product, category, and size range exact combination
# that we specify in retail_products.csv)
eu_retail <- eu_retail[which(eu_retail$PRODUCT %in% retail_products$PRODUCT),]

# Format the date properly as a single column called Date that R interprets
# as a date rather than a number
eu_retail$DateF <- paste0(eu_retail$YEAR,
                          "-",
                          sprintf("%02d", eu_retail$MONTH),
                          "-",
                          sprintf("%02d", eu_retail$DAY))
eu_retail$Date <- as.Date(eu_retail$DateF,
                                  format="%Y-%m-%d")

# Get just the most recent day of data
eu_retail_recent <- eu_retail[which(eu_retail$Date==max(eu_retail$Date)),]

# Aggregate to get the average price by product (including category and size)
# and country
eu_retail_aggregate <- aggregate(PRICE.PER.KG..EUR.~PRODUCT+COUNTRY+CATEGORY+SIZE.WEIGHT.RANGE+Date,
                                 FUN=mean,na.rm=T,data=eu_retail_recent)

# Order alphabetically by product and then by country
eu_retail_aggregate <- eu_retail_aggregate[order(eu_retail_aggregate$PRODUCT,
                                                 eu_retail_aggregate$COUNTRY),]

# Round the average price in euros to the nearest cent
eu_retail_aggregate$PRICE.PER.KG..EUR. <- round(eu_retail_aggregate$PRICE.PER.KG..EUR.,2)

# Rename the columns to more intuitive column headings for the table
names(eu_retail_aggregate) <- c("Product","Country","Category","Product size","Date","Price (EUR/kg)")

# Save to file
write.csv(eu_retail_aggregate,"~/finsight/_data/eu_retail_aggregate.csv",row.names = F)

#############
### Trade ###
#############

# Import the CSV that specifies how we want to aggregate
# species in the EU trade data
trade_species <- read.csv("~/finsight/parameters/trade_species.csv")

# Import all trade csv files
# and assign to the global environment
filenames_retail <- filenames[which(grepl("trade",filenames))]
for (i in filenames_retail){
  csv_tmp <- read.csv(paste0("~/finsight/data/",i),sep=";")
  assign(substr(i,1,nchar(i)-4), csv_tmp)
}

# Bind into a single data frame
eu_trade <- rbindlist(mget(ls(pattern = "^data_eu_trade.*")))

# Restrict the EU trade dataset to just the species that we have specified
eu_trade <- eu_trade[which(eu_trade$main_commercial_species %in% trade_species$main_commercial_species),]

# Merge with our species grouping definitions
eu_trade <- merge(eu_trade,trade_species,
                  by="main_commercial_species",
                  all.x=T,all.y=F)

# Get the most recent full year of data
# i.e. the most recent year of data, but minus one year!
# This guarantees that we'll have a full year of data
trade_year <- max(eu_trade$year)-1

# Restrict the dataset to just that year
eu_trade <- eu_trade[which(eu_trade$year==trade_year),]

# Now, for all partner countries that we don't care about, we want to
# set that to some other value (Other)
# So we can still see extra-Europe trade on our map
eu_trade[-which(eu_trade$partner_contry %in% production_countries$Country),"partner_contry"] <- "Other"

# Aggregate the trade volume by country, flow type (export vs import)
# and partner country
eu_trade_agg <- aggregate(volume.kg.~country+flow_type+partner_contry,
                          FUN=sum,
                          data=eu_trade)

# Ensure that the countries that we care about are named correctly
eu_trade_agg[which(eu_trade_agg$partner_contry=="Moldova, Republic of"),]<-"Moldova"
eu_trade_agg[which(eu_trade_agg$country=="Moldova, Republic of"),"country"]<-"Moldova"

# Specify the non-EU countries that we care about
#non_eu_flows <- c("Turkey","Norway","Switzerland","Iceland","United Kingdom")
#flows <- c(unique(eu_trade_agg$country),non_eu_flows)

#eu_trade_agg <- eu_trade_agg[which(eu_trade_agg$flow_type=="Export" | eu_trade_agg$partner_contry %in% non_eu_flows),]
#eu_trade_agg[which(eu_trade_agg$flow_type=="Export" | eu_trade_agg$partner_contry %in% non_eu_flows),]
#eu_trade_agg <- eu_trade_agg[which(eu_trade_agg$partner_contry %in% flows),]

# Define origins and destination, based on whether the trade is export or import
# The origin for exports is the reporting country
# The origin for exports is the partner country
eu_trade_agg$o <- ifelse(eu_trade_agg$flow_type=="Export",
                            eu_trade_agg$country,
                            eu_trade_agg$partner_contry)
# The destination for exports is the partner country
# The destination for imports is the reporting country
eu_trade_agg$d <- ifelse(eu_trade_agg$flow_type=="Export",
                            eu_trade_agg$partner_contry,
                            eu_trade_agg$country)

# Set value as the volume of trade
# and ensure it's numeric
eu_trade_agg$value <- as.numeric(eu_trade_agg$volume.kg.)

# Get the spatial vector of the international trade boundaries
# We're also going to convert this to a terra SpatVector
spatial_countries_2 <- vect(ne_countries(scale=50))

# Calculate the centroids of each country, which gives us the point
# where the trade flow will point to and from on the map
trade_centroids <- terra::centroids(spatial_countries_2,inside=T)

# Get just the coordinates of those centroids
trade_centroids_coordinates <- as.data.frame(geom(trade_centroids)[,c("x","y")])

# Add the country names of each set of coordinates
trade_centroids_coordinates$name <- spatial_countries_2$name

# Now we need to add somewhere for the "other" trade to point on the graph
# We'll specify some arbitrary, out-of-the-way coordinate
# To the northeast of Portugal will do nicely
other_coordinate <- trade_centroids_coordinates[which(trade_centroids_coordinates$name=="Portugal"),]
other_coordinate$x <- other_coordinate$x-5
other_coordinate$y <- other_coordinate$y+5
other_coordinate$name <- "Other"
trade_centroids_coordinates <- rbind(trade_centroids_coordinates,other_coordinate)

# Create a list of nodes that is all the countries in our aggregated trade dataset
# where "Other" is "all countries in our dataset that we don't explicitly care about"
nodes <- data.frame("name"=unique(c(eu_trade_agg$o,eu_trade_agg$d)))

# Rename Bosnia and Herzegovina so it matches correctly
trade_centroids_coordinates[which(trade_centroids_coordinates$name=="Bosnia and Herz."),"name"] <- "Bosnia and Herzegovina"

# For each of those nodes, get the coordinate of the centroid
nodes <- merge(nodes,
               trade_centroids_coordinates,
               by="name",
               all.x=T,
               all.y=F)

# Make the base map
map_trade <- ggplot() +
  geom_spatvector(color="white",data=spatial_countries) +
  xlim(-15.5,45) + 
  ylim(35,70) +
  theme_void() +
  scale_fill_viridis_c(
    direction=-1,
    option="plasma",
    labels = unit_format(unit = "k", scale = 1e-6)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        plot.background = element_rect(fill="white",colour="white")) +
  guides(fill = guide_colorbar(barwidth = 25))+
  annotate("text", 
           x = other_coordinate$x-1, 
           y = other_coordinate$y-1, 
           label = "(All\nnon-Europe)",
           size=2,
           colour="grey") +
  labs(title="Trade within Europe",
       subtitle=paste0("tonnes (net weight); ",production_recent_year),
       fill=NULL,
       caption=paste0("Data accounts for ",str_to_lower(paste(unique(eu_trade$Species_custom),collapse=", "))))

# Overlay the trade flows
# Caution: This is computationally intensive
map_trade_flow <- add_flowmap(p=map_trade,
                              od=eu_trade_agg,
                              nodes=nodes,
                              node_radius_factor=0.3,
                              outline_linewidth=0,
                              alpha=1,
                              arrow_point_angle=25
                              )
# Save to file
# Caution: This is computationally intensive
ggsave("~/finsight/assets/images/map_trade_flow.png",
       map_trade_flow,
       width=8,height=7)

#####################
### Country pages ###
#####################
# graph time series of individuals slaughtered, colour by species

# Make a palette for colouring each species, based on our pre-defined colours
# in the species CSV file
# This is so the colours will be consistent across countries
fish_palette <- c(species_custom$Colour)
names(fish_palette) <- c(species_custom$Species_custom)
fish_palette <- fish_palette[!duplicated(fish_palette)]
fish_palette <- c(fish_palette,"#B2A198")
names(fish_palette)[length(fish_palette)] <- "Other"

# get today's date, formatted as necessary for the country page file names
today_date <- as.character(Sys.Date())

# Sort alphabetically (for display on website home page)
production_countries <- production_countries[order(production_countries$Country),]

# Specify the column (corresponds to the column on the website homepage)
production_countries$column <- rep(1:3, 
                                        length.out = nrow(production_countries), 
                                        each = ceiling(nrow(production_countries)/3))

# Remove all old files in the directories
do.call(file.remove, list(list.files("~/finsight/col1/_posts", full.names = TRUE)))
do.call(file.remove, list(list.files("~/finsight/col2/_posts", full.names = TRUE)))
do.call(file.remove, list(list.files("~/finsight/col3/_posts", full.names = TRUE)))

# Loop over all countries that we have specified in the countries CSV
for (i in c(1:nrow(production_countries))){
  # Get just this country
  fao_allyears_tmp <- fao_allyears[which(fao_allyears$ISO2_Code==production_countries[i,"ISO2_Code"]),]

  # If there is no data, skip this iteration of the loop
  if(nrow(fao_allyears_tmp)==0){next}

  # Get the country name
  title_tmp <- production_countries[i,"Country"]
  
  # Get the column (corresponds to the column on the website homepage)
  col_tmp <- production_countries[i,"column"]
  
  # Create a lower-case filename without any spaces
  filename_tmp <- str_to_lower(gsub(" ","",title_tmp))
  
  # Remove the ü in Turkiye (as it causes issues with building the website)
  filename_tmp <- gsub("ü","u",filename_tmp)
  
  # Use this file name to define up-front the file names and paths that we will use for
  # the time series graph
  filepath_tmp_timeseries <- paste0("~/finsight/assets/images/",
                                    filename_tmp,
                                    "_timeseries.png")
  # the table of production for the most recent year
  filepath_tmp_productiontable <- paste0("~/finsight/_data/",
                                         filename_tmp,
                                         "_production.csv")
  # export and import tables
  filepath_tmp_exporttable <- paste0("~/finsight/_data/",
                                         filename_tmp,
                                         "_export.csv")
  filepath_tmp_importtable <- paste0("~/finsight/_data/",
                                         filename_tmp,
                                         "_import.csv")
  # and the post (country page) itself
  filepath_tmp_post <- paste0("~/finsight/col",
                              col_tmp,
                              "/_posts/",
                              today_date,
                              "-",
                              filename_tmp,
                              ".md")
  
  # Create the caption (proportion of production that we have mean weights for)
  caption_tmp <- paste0("This graph accounts for ",
                        100*round(sum(fao_allyears_tmp[!is.na(fao_allyears_tmp$Individuals_slaughtered),"VALUE"])/sum(fao_allyears_tmp$VALUE),3),
                        " % of production weight.")
  
  # Aggregate into our species category
  agg_fao_allyears_tmp <- aggregate(Individuals_slaughtered~PERIOD+Species_custom,
                                    FUN=sum,
                                    data=fao_allyears_tmp)
  g_timeseries_production_tmp <- ggplot(aes(x=PERIOD,y=Individuals_slaughtered,colour=Species_custom),
                                        data=agg_fao_allyears_tmp) +
    scale_x_continuous(breaks=seq(min(agg_fao_allyears_tmp$PERIOD),
                                  max(agg_fao_allyears_tmp$PERIOD),
                                  by=1)) +
    scale_colour_manual(values=fish_palette) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    labs(title=title_tmp,
         subtitle="Individuals slaughtered",
         colour=NULL,
         x=NULL,
         y=NULL,
         caption=caption_tmp) +
    geom_line() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position="bottom",
          plot.background = element_rect(fill="white",colour="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=6))
  
  # Save to file
  ggsave(filepath_tmp_timeseries,g_timeseries_production_tmp,
         width=6,height=4)
  
  # table of most recent production, sorted by individuals descending, showing all biological params
  current_production_tmp <- fao_quantity[which(fao_quantity$ISO2_Code==production_countries[i,"ISO2_Code"]),]
  
  # Order by individuals then (for ones we didn't calculate individuals for) weight
  current_production_tmp <- current_production_tmp[order(-current_production_tmp$Individuals_slaughtered,
                                                         -current_production_tmp$VALUE),]
  
  # Get just the columns we want to visualise
  current_production_tmp <- current_production_tmp[,c("Species","VALUE",
                                                      "Harvest_weight_g",
                                                      "Harvest_age_years",
                                                      "Mortality_rate",
                                                      "Individuals_slaughtered",
                                                      "Individuals_hatched",
                                                      "Individuals_inventory")]
  
  # Format the columns nicely for tabulation
  current_production_tmp$Harvest_age_years <- round(current_production_tmp$Harvest_age_years,3)
  current_production_tmp$Individuals_slaughtered <- comma(current_production_tmp$Individuals_slaughtered)
  current_production_tmp$Individuals_hatched <- comma(current_production_tmp$Individuals_hatched)
  current_production_tmp$Individuals_inventory <- comma(current_production_tmp$Individuals_inventory)
  current_production_tmp[is.na(current_production_tmp)] <- ""
  
  names(current_production_tmp)[2] <- "Production (t)"
  names(current_production_tmp)[3] <- "Harvest weight (g)"
  names(current_production_tmp)[4] <- "Harvest age (years)"
  names(current_production_tmp) <- gsub("_"," ",names(current_production_tmp))
  current_production_tmp <- current_production_tmp[,c(1,3,4,5,2,6,7,8)]
  
  # save to file
  write.csv(current_production_tmp,
            filepath_tmp_productiontable,
            row.names = F)
  
  # table of exports for the most recent year, sorted by volume descending
  # note that eu_trade was already restricted to the most recent year in the
  # "Trade" section of this R script
  eu_trade_tmp <- eu_trade[which(eu_trade$country=="Italy"),]
  eu_trade_tmp_agg <- aggregate(volume.kg.~flow_type+partner_contry+Species_custom,
                            FUN=sum,
                            data=eu_trade_tmp)
  
  # Order by volume
  eu_trade_tmp_agg <- eu_trade_tmp_agg[order(-eu_trade_tmp_agg$volume.kg.),]
  
  # Convert volume from kg to tonnes
  eu_trade_tmp_agg$t <- round(eu_trade_tmp_agg$volume.kg./1000)
  
  # Format numbers with nice commas
  eu_trade_tmp_agg$t <- comma(eu_trade_tmp_agg$t)
  
  # Rename to more attractive names
  names(eu_trade_tmp_agg) <- c("flow_type","Partner Country","Species","kg","Quantity (t)")
  
  # Split into exports and imports (separate tables)
  eu_trade_tmp_agg_exports <- eu_trade_tmp_agg[which(eu_trade_tmp_agg$flow_type=="Export"),]
  eu_trade_tmp_agg_imports <- eu_trade_tmp_agg[which(eu_trade_tmp_agg$flow_type=="Import"),]
  
  # Remove the unnecessary columns
  eu_trade_tmp_agg_exports <- eu_trade_tmp_agg_exports[,-c(1,4)]
  eu_trade_tmp_agg_imports <- eu_trade_tmp_agg_imports[,-c(1,4)]
  
  # save to file
  write.csv(eu_trade_tmp_agg_exports,
            filepath_tmp_exporttable,
            row.names = F)
  write.csv(eu_trade_tmp_agg_imports,
            filepath_tmp_importtable,
            row.names = F)
  
  # time series of all applicable prices
  # to do!

# and finally, construct the text of the country page
write(paste0(
  '---
layout: post
title: "',
title_tmp,
'"
permalink: "/',
filename_tmp,
'/"
---
',
  "# Production (",
  production_recent_year,
  ")  ",
  "\n",
  "![time series of individuals slaughtered over time](",
  gsub("~/finsight/","../",filepath_tmp_timeseries),
  ")",
  "\n\n",
  "
  <table class='prodtable'>
  {% for row in site.data.",filename_tmp,"_production %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>
<div class='prodtablenotes'>
Table notes: harvest weight, harvest age, and mortality rate are set by us as biological parameters (see bottom of page for details). Production is then used, with these parameters, to calculate individuals slaughtered, individuals hatched, and individuals inventory. 'Inventory' refers to the number of fish alive on animals at any one time.
</div>
\n\n",
  "# Trade (",
  trade_year,
  ")  ",
  "\n",
  "## Exports  ",
  "\n\n",
  "
  <table>\n
  {% for row in site.data.",filename_tmp,"_export %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>",
  "\n",
  "## Imports  ",
  "\n",
  "
  <table>
  {% for row in site.data.",filename_tmp,"_import %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}
    
    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>\n"),
  filepath_tmp_post)
}

