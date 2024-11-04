###################################
### Curl data from the internet ###
###################################
### Define folder path for storing data ###
folder_data <- ("~/finsight/data/")

### Get the current year ###
this_year <- as.numeric(substr(Sys.Date(),1,4))

### Define URLs and filenames for downloading data ###
# Set up an empty data frame that we will store URLs and filenames in
df_url_filename <- data.frame("url"="test",
                              "filename"="test")

# Production
# This only has one CSV file that never changes, so we can just specify a single
# url and a single filename
df_url_filename <- rbind(df_url_filename,
                         c("https://www.fao.org/fishery/static/Data/Aquaculture_2024.1.0.zip",
                           "data_production.zip"))

# Consumption
# This only has one CSV file that never changes, so we can just specify a single
# url and a single filename
df_url_filename <- rbind(df_url_filename,
                         c("https://dev-eumofa-bulk.s3.eu-west-1.amazonaws.com/Monthly_Consumption.csv",
                           "data_eu_consumption.csv"))

# Retail
# This has multiple CSV files, one for each year, beginning in 2021
# So we need to build the url and filenames automatically
# The sequence does this for us
sequence_eu_retail <- seq(2021,this_year,1) # Define the list of years

for (i in sequence_eu_retail){ # For each year...
  # Produce that year's URL
  tmp_url <- paste0("https://dev-eumofa-bulk.s3.eu-west-1.amazonaws.com/",
                    i,
                    "_Daily-online%20retail%20prices.csv")
  
  # Produce that year's filename
  tmp_filename <- paste0("data_eu_retail_",i,".csv")
  
  # Save to our data frame
  df_url_filename <- rbind(df_url_filename,
                           c(tmp_url,tmp_filename))
}

# Trade
# This has multiple CSV files, one for each year, beginning in 2009
# So we need to build the url and filenames automatically
# The for-loops do this for us
sequence_eu_trade <- seq(2009,this_year,1) # Define the list of years

for (i in sequence_eu_trade){ # For each year...
  # Produce that year's URL
  tmp_url <- paste0("https://dev-eumofa-bulk.s3.eu-west-1.amazonaws.com/",
                    i,
                    "_Trade_data_reported_by_EU_countries.csv")
  
  # Produce that year's filename
  tmp_filename <- paste0("data_eu_trade_",i,".csv")
  
  # Save to our data frame
  df_url_filename <- rbind(df_url_filename,
                           c(tmp_url,tmp_filename))
}

# Remove our first row (test row)
if(length(which(df_url_filename$url=="test"))>0){
  df_url_filename <- df_url_filename[-which(df_url_filename$url=="test"),]
}

### Download the data and save to file ###
# First, we use our data frame to produce system (bash) commands
# that call the 'curl' command to download the data from
# the URLs that we produced and save them to the file locations that we specified
# inside the folder that we specified
system_commands <- paste0("curl -X GET '",
                          df_url_filename$url,
                          "' > ",
                          folder_data,
                          df_url_filename$filename)

# Now, we simply call that system command using the R command "system"
# We use a for loop to do one at a time

# Note that this will take some time if the files are quite large
# as this is the stage where the files are actually downloaded from the internet
for (i in c(1:length(system_commands))){
  system(system_commands[i])
}

### Unzip any compressed files ###
# Identify zip files by file extensions
file_types <- substr(df_url_filename$filename,
                     nchar(df_url_filename$filename)-2,
                     nchar(df_url_filename$filename))

# Get a vector of all .zip files
zip_rows <- which(file_types=="zip")

# For each zip file, unzip
for (i in zip_rows){
  unzip(paste0(folder_data,df_url_filename[i,2]),
        exdir=folder_data)
}

