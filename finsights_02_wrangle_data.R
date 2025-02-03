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
parameters_finfish <- read.csv("~/finsight/parameters/finfish_parameters.csv")

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
eu_retail$PRICE.PER.UNIT..EUR. <- as.numeric(eu_retail$PRICE.PER.UNIT..EUR.)

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
# eu trade:
filenames_trade <- filenames[which(grepl("eu_trade",filenames))]
for (i in filenames_trade){
  csv_tmp <- read.csv(paste0("~/finsight/data/",i),sep=";")
  assign(substr(i,1,nchar(i)-4), csv_tmp)
}

# non eu trade:
filenames_trade_noneu <- filenames[which(grepl("noneu_trade",filenames))]
for (i in filenames_trade_noneu){
  csv_tmp <- read.csv(paste0("~/finsight/data/",i),sep=";")
  
  # commensurate the column names so we can easily merge with the
  # EU trade data below
  # firstly add a column denoting intra or extra EU trade
  # (obviously all extra EU by definition as this is the non-EU trade data)
  csv_tmp$intra_extra_EU <- "Extra EU"
  # secondly rename columns to match the EU counterparts
  names(csv_tmp)[3] <- "country"
  names(csv_tmp)[4] <- "partner_contry" #hahaha
  names(csv_tmp)[4:9] <- gsub("\\.","_",names(csv_tmp)[4:9])
  names(csv_tmp) <- gsub("Eur","EUR",names(csv_tmp))
  names(csv_tmp) <- gsub("Kg","kg",names(csv_tmp))
  
  # assign to global environment
  assign(substr(i,1,nchar(i)-4), csv_tmp)
}
# Bind each of only EU and non-EU into a single data frame
onlyeu_trade <- rbindlist(mget(ls(pattern = "^data_eu_trade.*")))
noneu_trade <- rbindlist(mget(ls(pattern = "^data_noneu_trade.*")))

# Check to make sure that *only* the United Kingdom is in both data sets
# as a *reporting* country
# (this is due to Brexit; all other *reporting* countries should be in either
# the EU or the non-EU dataset, not both)
# i.e. this command gives a single TRUE and the rest FALSE
unique(onlyeu_trade$country) %in% unique(noneu_trade$country)


# Combine both into a single data frame
# (Sorry for the name; this is because I originally only had the EU dataset
# and extended the code to include the non-EU dataset subsequently)
eu_trade <- rbind(onlyeu_trade,noneu_trade)

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

# Ensure that the countries that we care about are named in a way that
# is consistent with the subsequent map data that we're about to combine with
eu_trade[which(eu_trade$partner_contry=="Moldova, Republic of"),"partner_contry"]<-"Moldova"
eu_trade[which(eu_trade$country=="Moldova, Republic of"),"country"]<-"Moldova"
eu_trade[which(eu_trade$partner_contry=="Turkey"),"partner_contry"]<-"Türkiye"
eu_trade[which(eu_trade$country=="Turkey"),"country"]<-"Türkiye"

# Check Egypt specifically
# eu_trade_egypt <- eu_trade[which(eu_trade$partner_contry=="Egypt"),]

# Now, for all countries that we don't care about, we want to
# set that to some other value (Other)
# So we can still see extra-Europe trade on our map
# This applies to both the reporting country and the partner country
eu_trade[-which(eu_trade$country %in% production_countries$Country),"country"] <- "Other"
eu_trade[-which(eu_trade$partner_contry %in% production_countries$Country),"partner_contry"] <- "Other"

# Remove trade records that do not correspond to the countries we care about
# in either the reporting country or producing country
eu_trade <- eu_trade[-which(eu_trade$partner_contry=="Other" & eu_trade$country=="Other"),]

# Define origins and destination, based on whether the trade is export or import
# The origin for exports is the reporting country
# The origin for exports is the partner country
eu_trade$o <- ifelse(eu_trade$flow_type=="Export",
                            eu_trade$country,
                            eu_trade$partner_contry)
# The destination for exports is the partner country
# The destination for imports is the reporting country
eu_trade$d <- ifelse(eu_trade$flow_type=="Export",
                            eu_trade$partner_contry,
                            eu_trade$country)


# Aggregate the trade volume by country, flow type (export vs import)
# and partner country
eu_trade_agg <- aggregate(volume.kg.~o+d,
                          FUN=sum,
                          data=eu_trade)

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
other_coordinate$y <- other_coordinate$y+6.5
other_coordinate$name <- "Other"
trade_centroids_coordinates <- rbind(trade_centroids_coordinates,other_coordinate)

# Create a list of nodes that is all the countries in our aggregated trade dataset
# where "Other" is "all countries in our dataset that we don't explicitly care about"
nodes <- data.frame("name"=unique(c(eu_trade_agg$o,eu_trade_agg$d)))

# Rename Bosnia and Herzegovina and Türkiye so it matches correctly
trade_centroids_coordinates[which(trade_centroids_coordinates$name=="Bosnia and Herz."),"name"] <- "Bosnia and Herzegovina"
trade_centroids_coordinates[which(trade_centroids_coordinates$name=="Turkey"),"name"] <- "Türkiye"


# For each of those nodes, get the coordinate of the centroid
nodes <- merge(nodes,
               trade_centroids_coordinates,
               by="name",
               all.x=T,
               all.y=F)

# Set Norway's node specifically (otherwise it gives us a node in
# Svalbard which makes visualisation hard)
# and likewise nudge Sweden's a little to the southeast
nodes[which(nodes$name=="Norway"),"x"] <- 7
nodes[which(nodes$name=="Norway"),"y"] <- 62
nodes[which(nodes$name=="Sweden"),"x"] <- nodes[which(nodes$name=="Sweden"),"x"]
nodes[which(nodes$name=="Sweden"),"y"] <- nodes[which(nodes$name=="Sweden"),"y"]-2

# Merge node and trade data
eu_trade_agg_coord1 <- merge(eu_trade_agg,
      nodes,
      by.x="o",
      by.y="name",
      all.x=T,
      all.y=F)
eu_trade_agg_coord2 <- merge(eu_trade_agg_coord1,
                             nodes,
                             by.x="d",
                             by.y="name",
                             all.x=T,
                             all.y=F)

# Rename for creating the curves on the map
names(eu_trade_agg_coord2)[c((ncol(eu_trade_agg_coord2)-3):ncol(eu_trade_agg_coord2))] <- c("x","y","xend","yend")

# Manipulate the spatial data a little bit as necessarsy
eu_trade_agg_coord3 <- st_zm(st_as_sf(eu_trade_agg_coord2,
                                coords=c("x","y"),
                             remove=F))
eu_trade_agg_coord3 <- st_set_crs(eu_trade_agg_coord3,crs(spatial_countries))

# Remove any records where the origin equals the destination
# (this is only true for 1 row, UK, which is presumably due to the devolved
# countries within the UK)
eu_trade_agg_coord3 <- eu_trade_agg_coord3[-which(eu_trade_agg_coord3$d==eu_trade_agg_coord3$o),]

# Make the base map
map_trade <- ggplot() +
  geom_spatvector(color="white",fill="grey40",data=spatial_countries) +
  #xlim(-17,36) +
  #ylim(35.5,65) +
  xlim(-25,38) +
  ylim(35,66) +
  theme_void() +
  geom_curve(aes(x=x,y=y,xend=xend,yend=yend,
                 colour=volume.kg.,
                 alpha = volume.kg.,
                 linewidth=volume.kg.
                 ),
            data=eu_trade_agg_coord3,
             curvature = 0.3,
             arrow = arrow(angle=15)) +
  scale_colour_viridis_c(
    limits = c(-100000000, NA), 
    breaks = c(0,100000000,200000000,300000000,400000000,500000000),
    oob = scales::squish,
    direction=-1,
    option="mako",
    labels = unit_format(unit = "k", scale = 1e-6)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        plot.background = element_rect(fill="white",colour="white")) +
  guides(colour = guide_colorbar(barwidth = 25))+
  scale_alpha_continuous(guide="none") +
  scale_linewidth_continuous(guide="none") +
  annotate("text", 
           x = other_coordinate$x-1, 
           y = other_coordinate$y-2, 
           label = "(All\nnon-Europe)",
           size=4,
           colour="grey20") +
  labs(title="Trade of seafood in Europe",
       subtitle=paste0("tonnes (net weight); ",production_recent_year),
       colour=NULL,
       caption=paste0("Data accounts for ",str_to_lower(paste(unique(eu_trade$Species_custom),collapse=", ")),
                      "\nData *includes fisheries products*, not only aquaculture"))
map_trade

# Save to file
ggsave("~/finsight/assets/images/map_trade_flow.png",
       map_trade,
       width=8,height=8)

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
  # the production time series graph
  filepath_tmp_timeseries <- paste0("~/finsight/assets/images/",
                                    filename_tmp,
                                    "_timeseries.png")
  # the price time series graph
  filepath_tmp_pricetimeseries <- paste0("~/finsight/assets/images/",
                                    filename_tmp,
                                    "_pricetimeseries.png")
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
  g_timeseries_production_tmp <- ggplot(aes(x=PERIOD,y=Individuals_slaughtered,colour=Species_custom,linetype =Species_custom),
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
         linetype=NULL,
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
  current_production_tmp$VALUE <- round(current_production_tmp$VALUE)
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
  eu_trade_tmp <- eu_trade[which(eu_trade$country==production_countries[i,"Country"]),]
  if (nrow(eu_trade_tmp)>0){
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
  }
  # time series of all applicable prices
  # subset retail data set so we only have rows from this country
  eu_retail_country <- base::subset(eu_retail,COUNTRY==production_countries[i,"Country"])
  
  if (nrow(eu_retail_country)>0){
    # Get just the most recent day of data
    # Aggregate to get the average price by product (including category and size)
    # and date
    
    # Specify the price measure (per kg or per unit)
    eu_retail_country$measure <- ifelse(!is.na(eu_retail_country$PRICE.PER.KG..EUR.),
                                        "EUR per kg",
                                        "EUR per unit")
    
    eu_retail_country$ProductCategorySize <- paste0(eu_retail_country$PRODUCT,
                                                          ", ",
                                                          eu_retail_country$CATEGORY,
                                                              ", ",
                                                              eu_retail_country$SIZE.WEIGHT.RANGE,
                                                              ", ",
                                                              eu_retail_country$measure)
    if ("EUR per kg" %in% unique(eu_retail_country$measure)){
    eu_retail_country_aggregate <- aggregate(PRICE.PER.KG..EUR.~ProductCategorySize+Date,
                                     FUN=mean,na.rm=T,data=eu_retail_country)
    names(eu_retail_country_aggregate)[3] <- "Price"
    }
    if ("EUR per unit" %in% unique(eu_retail_country$measure)){
    eu_retail_country_aggregate <- aggregate(PRICE.PER.UNIT..EUR.~ProductCategorySize+Date,
                                     FUN=mean,na.rm=T,data=eu_retail_country)
    names(eu_retail_country_aggregate)[3] <- "Price"
    }
    
    eu_retail_country_aggregate$Date2 <- as.POSIXct(eu_retail_country_aggregate$Date)

    g_eu_retail_country_aggregate_tmp <- ggplot(aes(x=Date2,
                                                    y=Price,
                                                    group=ProductCategorySize,
                                                    colour=ProductCategorySize,
                                                    linetype=ProductCategorySize),
                                        data=eu_retail_country_aggregate) +
    scale_x_datetime(date_breaks = "1 month",
                   name=NULL) +
    labs(title=title_tmp,
         subtitle="Product price",
         colour=NULL,
         linetype=NULL,
         x=NULL,
         y=NULL) +
    geom_line() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position="bottom",
          plot.background = element_rect(fill="white",colour="white"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=6)) +
      guides(colour=guide_legend(nrow=length(unique(eu_retail_country_aggregate$ProductCategorySize))))
  
  # Save to file
  ggsave(filepath_tmp_pricetimeseries,g_eu_retail_country_aggregate_tmp,
         width=6,height=4)

  }
  
  
  
  
  
  

# and finally, construct the text of the country page
# Check if we're adding a country-specific retail price section
  webpage_section_retail <- ""
  if (nrow(eu_retail_country)>0){
   webpage_section_retail <- paste0(
     "# Retail prices",
     "\n",
     "![time series of individuals slaughtered over time](",
  gsub("~/finsight/","../",filepath_tmp_pricetimeseries),
  ")",
  "\n\n"
   )
  }
  
# Check if we're adding a country-specific trade section
  webpage_section_trade <- ""
  if (nrow(eu_trade_tmp)>0){
    webpage_section_retail <- paste0(
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
</table>\n"
    )
  }
  
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
Table notes: harvest weight, harvest age, and mortality rate are set by us as biological parameters (see bottom of page for details). Production is then used, with these parameters, to calculate individuals slaughtered, individuals hatched, and individuals inventory. 'Inventory' refers to the number of fish alive on animals at any one time. Production weight is rounded to the nearest tonne.
Biological parameters explained and cited <a href=\"{% link parameters.md %}\">here</a>. Note that some biological parameters for less frequently farmed species are placeholder values.
</div>
\n\n",
  "![time series of individuals slaughtered over time](",
  gsub("~/finsight/","../",filepath_tmp_timeseries),
  ")",
  "\n\n",
webpage_section_retail,
webpage_section_trade
),
  filepath_tmp_post)
}

