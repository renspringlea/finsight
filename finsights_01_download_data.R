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
# There is a CSV for each year for EU countries and a CSV for each year
# for non-EU countries (but both are provided by EUMOFA, who in turn
# receive the data from the company Trade Data Monitor)
# see meta data here https://eumofa.eu/documents/20124/35680/Metadata+1+-+DATA+COLLECTION.pdf/ce349b1c-f73a-413a-b6f0-7dfee54fa042?t=1680597414536

# So we need to build the url and filenames automatically
# The for-loops do this for us
sequence_eu_trade <- seq(2009,this_year,1) # Define the list of years

for (i in sequence_eu_trade){ # For each year...
  # Produce that year's URL
  tmp_url <- paste0("https://dev-eumofa-bulk.s3.eu-west-1.amazonaws.com/",
                    i,
                    "_Trade_data_reported_by_EU_countries.csv")
  tmp_url_noneu <- paste0("https://dev-eumofa-bulk.s3.eu-west-1.amazonaws.com/",
                    i,
                    "_Trade_data_reported_by_non-EU_countries.csv")
  
  # Produce that year's filename
  tmp_filename <- paste0("data_eu_trade_",i,".csv")
  tmp_filename_noneu <- paste0("data_noneu_trade_",i,".csv")
  
  # Save to our data frame
  df_url_filename <- rbind(df_url_filename,
                           c(tmp_url,tmp_filename),
                           c(tmp_url_noneu,tmp_filename_noneu))
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

# Delete any files that did not exist
# My motivation here is that shortly after the turn of a new year
# (e.g. Jan 2025), "this year" will be 2025 but 2025 data may not yet
# have been produced by the websites we are using as data sources
# We begin the loop at row 3 because the first two rows correspond to
# data sources with a single URL, not a different URL for each year
for (i in c(3:nrow(df_url_filename))){
  file_tmp <- paste0(folder_data,df_url_filename[i,"filename"])
  check_tmp <- is.na(read.csv(file_tmp,sep=";")[2,1])
    if(check_tmp){
    print(paste0("Removing file: ",file_tmp))
    file.remove(file_tmp)
  }
  if(!check_tmp){
    print(paste0("Keeping file: ",file_tmp))
  }
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

