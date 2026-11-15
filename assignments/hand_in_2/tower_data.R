# preparing data

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
          "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
          "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")

zip_file <- list.files("data/raw", pattern = "\\.zip$", full.names = TRUE)[2]
hh_name  <- grep("FLUXMET_HH", unzip(zip_file, list = TRUE)$Name, value = TRUE)
unzip(zip_file, files = hh_name, exdir = "data/raw/unzipped")

hh <- read_csv(file.path("data/raw/unzipped", hh_name),
               na = "-9999",
               col_select = any_of(keep),
               col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c", .default = "d")) |>
  mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"))

nrow(hh)
range(hh$time)
mean(hh$NEE_VUT_REF_QC == 0, na.rm = TRUE)   # share of measured NEE

#counting rows per year
df <- hh |> 
  mutate(Year = year(time)) |> 
  group_by(Year) |> 
  mutate(rows = n())
#17520 rows per year

#duplicates
length(unique(df$TIMESTAMP_START)) == nrow(df)
# TRUE so no duplicates in timestemps

#Temperature values
summary(hh$TA_F)
#looks good



############ Function ############

# all of the above as a function

check_data <- function(file_position) {
  keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
          "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
          "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")
  zip_file <- list.files("data/raw", pattern = "\\.zip$", full.names = TRUE)[file_position]
  hh_name  <- grep("FLUXMET_HH", unzip(zip_file, list = TRUE)$Name, value = TRUE)
  unzip(zip_file, files = hh_name, exdir = "data/raw/unzipped")
  
  hh <- read_csv(file.path("data/raw/unzipped", hh_name),
               na = "-9999",
               col_select = any_of(keep),
               col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c", .default = "d")) |>
  mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"))

  cat("Range of time:", format(range(hh$time)), "\n")

  df <- hh |> 
    mutate(Year = year(time)) |> 
    group_by(Year) |> 
    mutate(rows = n())
  cat("number of rows per year:", unique(df$rows), "\n")

  cat("No duplicates?", length(unique(df$TIMESTAMP_START)) == nrow(df), "\n")

  cat("Temperature stats (Min, 1st Q, Median, Mean, 3rd Q, Max):", summary(hh$TA_F))
}

#same file for testing
check_data(2)

check_data(1) #looks good
check_data(3) #looks good
check_data(4) #looks good

## read_hh function = create_data_table

create_data_table <- function() {
  keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
          "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
          "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")
  zip_files <- list.files("data/raw", pattern = "\\.zip$", full.names = TRUE)
  all_data <- list()   # empty list for the results

  for (zip_file in zip_files) {
    hh_name <- grep("FLUXMET_HH", unzip(zip_file, list = TRUE)$Name, value = TRUE)
    unzip(zip_file, files = hh_name, exdir = "data/raw/unzipped")

    all_data[[zip_file]] <- read_csv(file.path("data/raw/unzipped", hh_name),
                                     na = "-9999",
                                     col_select = any_of(keep),
                                     col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c",
                                                      .default = "d")) |>
      mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"),
             site = str_extract(basename(zip_file), "[A-Z]{2}-[A-Za-z0-9]{3}"))
  }

  bind_rows(all_data)   # stack everything into one table and return it
}

complete_df <- create_data_table()
write.csv(complete_df, "data/complete_data.csv", row.names = FALSE)


