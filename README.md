# MP

First Phase
TADS_Vegetation.Rmd: this will contain the TADS analysis of the following
- trends in outage cause codes
- vegetation rates

01_saidi_saifi_caidi_duke.Rmd
- calculating SAIDI/SAIFI/CAIDI annula metrics on the Duke Energy data to compare with state level data
02_saidi_saifi_caidi_duke.R
- Allocates outages to every 5-min bin they overlap (count interruption in each spanned bin).
- Computes SAIDI_5min, SAIFI_5min, CAIDI_5min per county x 5-min bin using tidycensus population.
- Joins county -> nearest WBANNO (from reliability_with_matched_uscrn.gpkg).
- Joins 5-min weather from uscrn_combined_reformatted.rds (WBANNO + datetime).
- Builds model-ready dataset and fits lmer: CAIDI_5min ~ weather + (1 | state).

01_NOAA_Weather_Stations.R
- Pulls all the txt files from NOAA (raw)
02_NOAA_Weather_Stations.R
- Processes raw txt files from NOAA
03_NOAA_Weather_Stations.Rmd
- Exploring weather data

01_duke_weather_analysis.Rmd
- Combining duke outage and weather data and outputting county_to_uscrn_lookup.csv
- it matches each county (from your reliability spatial data) to the nearest USCRN weather station
- computes the distance, tags whether the station is within 50 km
- saves a GeoPackage with the joined spatial data plus a CSV lookup table
02_duke_weather_analysis.Rmd
- Contains multilevel model