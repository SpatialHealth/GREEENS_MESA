### 01 Data Setup 

# Author: Cameron Reimer
# Created: 10/14/2023
# Last Edited: 10/19/2023

library(dplyr)
library(tidyverse)
library ("sas7bdat")

################################################################################
############################ PROCESS GSV DATA ##################################
################################################################################

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/MESA_LimGSV_20230822")

### 1. Read in GSV files #######################################################

# find all file names in this directory that match this pattern 
filenames <- list.files(path = ".", pattern = "c\\d+\\_\\d+\\c.csv")

# read files into a list 
df_list <- lapply(filenames, read.csv, header = TRUE)

# rename the list items with the file names 
names(df_list) <- gsub(pattern = 'c.csv', replacement = '', x = filenames)


### 2. Loop through files for processing #######################################

for(i in 1:length(df_list)){
  # assign tmp df for readability 
  df <- df_list[[i]]
  
  # remove row numbers 
  df <- df |> dplyr::select(!X)
  
  # re-assign -9 to NA
  df[df == -9] <- NA
  
  # get class info from file name, e.g., "c5", "c10"
  gsv_class <- (names(df_list)[i] |> strsplit(split = '_', fixed = TRUE) |> unlist())
  
  ### 2a. Convert to %
  df <- df %>%
    dplyr::mutate_at(
      grep("c", names(df)),
      ~ ifelse(!is.na(.), ./100, .)
    ) 
  
  ### 2b. Add IQR
  all_cols <- grep(pattern = "20", names(df), fixed = TRUE)
  
  # if IQR is not 0, write IQR as the value / IQR. else, write 0
  for(k in all_cols){
    iqr_colname <- paste0("IQR_", names(df)[k])
    
    if(IQR(df[ ,k], na.rm = T) != 0){
      df[, iqr_colname] <- (df[, k] / IQR(df[ ,k], na.rm = T))
    }else{
      df[, iqr_colname] <- 0
    }
    
  }
  
  ### 2c. Rename columns to the pattern: "gsv_class"_"buffer"."year" --> (e.g., c10_500.2015)
  dat_cols <- grepl(pattern = '20', names(df), fixed = TRUE)
  colnames_out <- NA
  
  split_cols <- names(df)[dat_cols] |> strsplit(split = '.', fixed = TRUE)
  for(y in 1:length(split_cols)){
    colnames_out[y] <- paste0(split_cols[[y]], collapse = paste0('_', gsv_class[2], '.'))
  }
  colnames_out <- c(names(df)[!dat_cols], colnames_out)
  
  names(df) <- colnames_out
  
  # assign df back to list 
  df_list[[i]] <- df
}


### 3. Convert data set to long ################################################

# merge all processed data sets by id 
combined <- df_list %>%
  reduce(full_join, by = "idno")

# get all time-varying columns 
tv_var <- names(combined)[grepl(pattern = '20', names(combined), fixed = TRUE)]

# convert to long
long_df <- reshape(data = combined,
                   idvar = "idno",
                   varying = tv_var,
                   timevar = "year",
                   times = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
                             2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
                   direction = "long")

# order by ID and year 
long_df <- long_df[order(long_df$idno, long_df$year),]


### convert columns to class names (Optional)
gsv_dict <- data.frame(class = c('c5', 'c10', 'c12', 'c13', 'c14', 'c17', 'c18', 'c21', 'c26', 'c28', 'c30', 'c47', 'c61', 'c67', 'c73', 'c84', 'c94', 'c95', 'c103', 'c128', 'c129', 'c137', 'c150'), 
                       definition = c('trees', 'grass', 'pavement', 'person', 'ground', 'mountain', 'plant', 'car', 'house', 'sea', 'field', 'sand', 'river', 'flowers', 'palm', 'truck', 'pole', 'soil', 'van', 'bicycle', 'lake', 'traffic_signal', 'flag'))
for(i in 1:nrow(gsv_dict)){
  colnames(long_df) <- sub(paste0(gsv_dict[i,1], '_'), paste0(gsv_dict[i,2], '_'), colnames(long_df), fixed = T)
}


### 4. Export Data #############################################################
out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"

readr::write_csv(x = long_df, 
                 file = paste0(out_dir, "gsv_long_noNAs.csv"), 
                 num_threads = 3, na=".") # adding option param to change NA to.



