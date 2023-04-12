# Data cleaning and vis
# 2023-04-12
# J. Holmquist

# Libraries
library(tidyverse)
library(sf)
library(sp)
library(gstat)

cores <- read_csv("data/CCRCN_data/CCRCN_cores.csv")
depths <- read_csv("data/CCRCN_data/CCRCN_depthseries.csv")  
depths <- read_csv("data/CCRCN_data/CCRCN_depthseries.csv", guess_max = nrow(depths))  

cores_filtered <- cores %>% 
  filter(!is.na(stocks_qual_code),
         habitat != "seagrass",
         max_depth >= 30) %>% 
  select(study_id, site_id, core_id, latitude, longitude, admin_division)

depths_filtered <- depths %>% 
  filter(study_id %in% cores_filtered$study_id &
           site_id %in% cores_filtered$site_id &
           core_id %in% cores_filtered$core_id) %>% 
  filter(depth_max <= 30)

depths_summarised <- depths_filtered %>% 
  mutate(organic_matter_density = fraction_organic_matter) %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(fraction_organic_matter = mean(fraction_organic_matter, na.rm =T)) %>% 
  filter(! is.na(fraction_organic_matter)) %>% 
  mutate(percent_organic_matter = fraction_organic_matter * 100)

master_file <- depths_summarised %>% 
  left_join(cores_filtered) %>% 
  filter(complete.cases(latitude, longitude))

write_csv(master_file, "data/Carbon_Fraction_Global_Marshes.csv")

  ###### Variograms for the three different model residuals ######## 
# Define the 1st order polynomial equation
coordinates(master_file) = ~longitude+latitude
master_file@proj4string <- CRS("+proj=longlat +datum=WGS84")

# Define the 1st order polynomial equation
f.1 <- as.formula(percent_organic_matter ~ 1) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl.1 <- variogram(f.1, master_file)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit.1  <- fit.variogram(var.smpl.1, vgm("Sph"))

# The following plot allows us to assess the fit
plot(var.smpl.1, dat.fit.1)

f.2 <- as.formula(model2residuals ~ 1) 
