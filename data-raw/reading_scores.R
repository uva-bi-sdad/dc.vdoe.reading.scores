# packages
library(readxl)
library(tidyverse)
library(dplyr)
library(RPostgreSQL)
library(fuzzyjoin)
library(reshape2)

########################
# GEOCODE ADDRESSES
########################

# school_test <- read_excel("2020-2021-school-test-by-test.xlsx")
# # keep Subject = English: Reading and Grade = Gr 3
# reading_score <- subset(school_test, Subject == "English: Reading" & Grade == "Gr 3")
#
# # add aka address
# reading_score$address <- paste(reading_score$`Sch Name`, reading_score$`Div Name`)
#
# # geocode addresses
# # installed google api key
# readRenviron("~/.Renviron")
# Sys.getenv("GOOGLEGEOCODE_API_KEY") # <<< use your Google API key
#
# reading_lonlat <- reading_score %>%
#   geocode(address,
#           method = 'google',
#           lat = latitude ,
#           long = longitude,
#           full_results = FALSE)
#
# # save
# # write_csv(reading_lonlat, "reading_score_lonlat.csv")

#############################################

# load in data
read_score <- read_csv("reading_score_lonlat.csv")
read_score$`2018-2019 Pass Rate` <- as.numeric(read_score$`2018-2019 Pass Rate`)
read_score$`2019-2020 Pass Rate` <- as.numeric(read_score$`2019-2020 Pass Rate`)
read_score$`2020-2021 Pass Rate` <- as.numeric(read_score$`2020-2021 Pass Rate`)

# load county and health distrct files
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1", # may need a full DB address if not working from Rivanna
                 port = 5432,
                 user = "YOUR_USER_NAME", # use your DB user name here
                 password = "YOUR_PASSWORD") # enter your DB password here

counties <- dbGetQuery(con, "SELECT * FROM dc_common.va_ct_sdad_2021_virginia_county_geoids")
health_dist <- dbGetQuery(con, "SELECT * FROM dc_common.va_hdct_sdad_2021_health_district_counties")

dbDisconnect(con)

# match counties to reading score
read_score_ct <- counties %>%
  stringdist_inner_join(read_score, by = c('region_name' = 'Div Name'), max_dist = 10)
# mean rate
df1 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2019 =
            mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df2 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df12 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df22 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_ct <- read_score_ct %>%
  select(geoid, region_name)

read_score_ct <- left_join(read_score_ct, df1, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df2, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df12, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df22, by=("geoid"))

read_score_ct <- read_score_ct %>% distinct()
read_score_ct["region_type"] <- "county"


read_score_ct_long <- melt(read_score_ct,
                  id.vars=c("geoid", "region_type", 'region_name'),
                  variable.name="measure",
                  value.name="value"
)
read_score_ct_long['year'] =  str_sub(read_score_ct_long$measure,-4,-1)
read_score_ct_long$measure = str_sub(read_score_ct_long$measure,1,-6)
read_score_ct_long['measure_type'] = ""
indx1 <- grepl('mean', read_score_ct_long$measure)
indx2 <- grepl('median', read_score_ct_long$measure)
read_score_ct_long$measure_type[indx1] <- 'mean'
read_score_ct_long$measure_type[indx2] <- 'median'
read_score_ct_long['measure_units'] <- 'percent'

#re-oder columns
read_score_ct_long <- read_score_ct_long[, c(1, 2, 3, 6, 4, 5, 7, 8)]

#write_csv(read_score_ct, "~/R/access_to_edu/va_ct_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

##################### HEALTH DISCTRICT ##############
read_score_hd <- left_join(read_score_ct, health_dist, by= c("geoid" = "geoid_county"))

df3 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(mean_read_pass_rate_2019 =
              mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df4 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df32 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df42 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_hd <- read_score_hd %>% select(geoid.y, region_name.y)
names(read_score_hd)[1] <- 'geoid'
names(read_score_hd)[2] <- 'region_name'

read_score_hd <- left_join(read_score_hd, df3, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df4, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df32, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df42, by=c("geoid"="geoid.y"))

read_score_hd <- read_score_hd %>% distinct(geoid, .keep_all=TRUE)

read_score_hd["region_type"] <- "health district"

read_score_hd_long <- melt(read_score_hd,
                           id.vars=c("geoid", "region_type", 'region_name'),
                           variable.name="measure",
                           value.name="value"
)

read_score_hd_long['year'] =  str_sub(read_score_hd_long$measure,-4,-1)

read_score_hd_long$measure = str_sub(read_score_hd_long$measure,1,-6)

read_score_hd_long['measure_type'] = ""
indx1 <- grepl('mean', read_score_hd_long$measure)
indx2 <- grepl('median', read_score_hd_long$measure)
read_score_hd_long$measure_type[indx1] <- 'mean'
read_score_hd_long$measure_type[indx2] <- 'median'
read_score_hd_long['measure_units'] <- 'percent'

#write_csv(read_score_hd, "~/R/access_to_edu/va_hd_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

################################ SCHOOL DISTRICT
# upload school geoids
read_score_sd <- read_csv("read_score_sd.csv")
read_score_sd$`2018-2019 Pass Rate` <- as.numeric(read_score_sd$`2018-2019 Pass Rate`)
read_score_sd$`2019-2020 Pass Rate` <- as.numeric(read_score_sd$`2019-2020 Pass Rate`)
read_score_sd$`2020-2021 Pass Rate` <- as.numeric(read_score_sd$`2020-2021 Pass Rate`)


df5 <- read_score_sd %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2019 =
              mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df6 <- read_score_sd %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df52 <- read_score_sd %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df62 <- read_score_sd %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_sd <- read_score_sd %>% select(geoid)
read_score_sd <- left_join(read_score_sd, df5, by=c("geoid"))
read_score_sd <- left_join(read_score_sd, df6, by=c("geoid"))
read_score_sd <- left_join(read_score_sd, df52, by=c("geoid"))
read_score_sd <- left_join(read_score_sd, df62, by=c("geoid"))

read_score_sd <- read_score_sd %>% distinct(geoid, .keep_all=TRUE)

read_score_sd["region_type"] <- "school district"

read_score_sd_long <- melt(read_score_sd,
                           id.vars=c("geoid", "region_type"),
                           variable.name="measure",
                           value.name="value"
)

read_score_sd_long['year'] =  str_sub(read_score_sd_long$measure,-4,-1)

read_score_sd_long$measure = str_sub(read_score_sd_long$measure,1,-6)

read_score_sd_long['measure_type'] = ""
indx1 <- grepl('mean', read_score_sd_long$measure)
indx2 <- grepl('median', read_score_sd_long$measure)
read_score_sd_long$measure_type[indx1] <- 'mean'
read_score_sd_long$measure_type[indx2] <- 'median'
read_score_sd_long['measure_units'] <- 'percent'

#write_csv(read_score_hd, "~/R/access_to_edu/va_sd_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

##################### ADD TO DB
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1", # may need a full DB address if not working from Rivanna
                 port = 5432,
                 user = "YOUR_USER_NAME", # use your DB user name here
                 password = "YOUR_PASSWORD") # enter your DB password here

dbWriteTable(con, c("dc_education_training", "va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_ct_long,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_hd_long, row.names = F)
dbWriteTable(con, c("dc_education_training", "va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_sd_long, row.names = F)

# change ownership to SDAD
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score
                OWNER TO data_commons")

# to remove tables from DB
# dbRemoveTable(con, c("dc_education_training", "va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
#              read_score_ct_long)
# dbRemoveTable(con, c("dc_education_training", "va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
#              read_score_hd_long)
# dbRemoveTable(con, c("dc_education_training", "va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
#              read_score_sd_long)

dbDisconnect(con)

