### 01 Data Setup 

# Author: Cameron Reimer, and Marcia P. Jimenez
# modified by Tara Jenson
# Created: 07/24/2024
# Last Edited: 08/03/2024

library(dplyr)
library(tidyverse)
library ("sas7bdat")
library(VIM) # for hotdeck imputation

rm(list = ls())


############################ PROCESS GSV DATA updated from 2000-2006 ##################################


setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/Data_GSV_071524")

### 1. Read in GSV files -------------------------------------------------------

# find all file names in this directory that match this pattern 
filenames <- list.files(path = ".", pattern = "c\\d+\\_\\d+\\c.csv")

# read files into a list 
df_list <- lapply(filenames, read.csv, header = TRUE)

# rename the list items with the file names 
names(df_list) <- gsub(pattern = 'c.csv', replacement = '', x = filenames)


### 2. Loop through files for processing ---------------------------------------

for(i in 1:length(df_list)){
  # assign tmp df for readability 
  df <- df_list[[i]]
  
  # remove row numbers 
  df <- df |> dplyr::select(!X)
  
  # re-assign -9 to NA
  df[df == -9] <- NA
  
  # get all columns > 2007 
  #col_indices <- grep(pattern = "c\\d{1,2}.20(0[8-9]|1[0-9]|2[0])", names(df))
  
  # get class info from file name, e.g., "c5", "c10"
  gsv_class <- (names(df_list)[i] |> strsplit(split = '_', fixed = TRUE) |> unlist())
  # name new columns with class and year e.g., "c10.2000" "c10.2001" "c10.2002" "c
  new_cols <- paste0(gsv_class[i], ".", 2000:2006)
  
  # re-order columns
  dat_cols <- grepl(pattern = '20', names(df), fixed = TRUE)
  df <- df %>%
    dplyr::select(!names(df)[dat_cols], sort(names(df)[dat_cols]))
  
  ### 2c. Convert to %
  df <- df %>%
    dplyr::mutate_at(
      grep("c", names(df)),
      ~ ifelse(!is.na(.), ./100, .)
    ) 
  
  ### 2d. Add IQR
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
  
  ### 2e. Rename columns to the pattern: "gsv_class"_"buffer"."year" --> (e.g., c10_500.2015)
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

### 3. Convert data set to long ------------------------------------------------

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
                   times = c(2000, 2001, 2002, 2003, 2004, 2005, 2006),
                   direction = "long")

# order by ID and year 
long_df <- long_df[order(long_df$idno, long_df$year),]


### convert columns to class names (optional)
gsv_dict <- data.frame(class = c('c5', 'c10', 'c12', 'c13', 'c14', 'c17', 'c18', 'c21', 'c26', 'c28', 'c30', 'c47', 'c61', 'c67', 'c73', 'c84', 'c94', 'c95', 'c103', 'c128', 'c129', 'c137', 'c150'), 
                       definition = c('trees', 'grass', 'pavement', 'person', 'ground', 'mountain', 'plant', 'car', 'house', 'sea', 'field', 'sand', 'river', 'flowers', 'palm', 'truck', 'pole', 'soil', 'van', 'bicycle', 'lake', 'traffic_signal', 'flag'))

for(i in 1:nrow(gsv_dict)){
  colnames(long_df) <- sub(paste0(gsv_dict[i,1], '_'), paste0(gsv_dict[i,2], '_'), colnames(long_df), fixed = T)
}


### 4. Export Data -------------------------------------------------------------
# 
# out_dir <- "/Users/jimenezm/Library/CloudStorage/OneDrive-BostonUniversity/MESA_GSV/Data_GSV_071524/"
#  
# readr::write_csv(x = long_df,
#                  file = paste0(out_dir, "gsv_2000_2006_long_072424.csv"),
#                  num_threads = 3)
# 
# gsv <- readr::read_csv(paste0(out_dir, "gsv_2000_2006_long_072424.csv"))



