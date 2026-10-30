library(terra)
library(sf)
library(geodata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)



# 1. Download monthly precipitation data for South Africa (ISO code: ZAF)
# This returns a SpatRaster with 12 layers (January = 1, ..., December = 12)
global_clim <- geodata::worldclim_global(
  var = "prec", 
  res = 10, 
  path = "data/"
)

southern_africa_extent <- terra::ext(10, 41, -35, -10)
sa_precip <- terra::crop(global_clim, southern_africa_extent)

# 2. Sum the rainfall for the distinct seasonal periods
# Winter: April (4) to September (9)
winter_months <- sa_precip[[4:9]]
winter_total  <- sum(winter_months)

# Summer: October (10) to March (3)
# Note: WorldClim layers are stacked Jan-Dec, so we grab Oct-Dec and Jan-Mar
summer_months <- sa_precip[[c(10, 11, 12, 1, 2, 3)]]
summer_total  <- sum(summer_months)

# Total Annual Precipitation (MAP)
annual_total <- sum(sa_precip)

# 3. Calculate the percentage of annual rainfall that occurs in Winter
winter_pct <- (winter_total / annual_total) * 100


# 4. Classify into Winter, Summer, and Year-Round Regimes
# Standard climatological rule-of-thumb:
# - Winter Zone: > 60% of rain falls in winter (Apr-Sep)
# - Summer Zone: < 35% of rain falls in winter (meaning > 65% falls in summer)
# - Year-Round Zone: Balanced distribution (between 35% and 60% winter rain)
classify_matrix <- matrix(c(
  0,  35, 1,   # Zone 1: Summer Rainfall Zone (SRZ)
  35,  60, 2,   # Zone 2: Year-Round Rainfall Zone (YRZ)
  60, 100, 3    # Zone 3: Winter Rainfall Zone (WRZ)
), ncol = 3, byrow = TRUE)

sa_zones_raster <- classify(winter_pct, classify_matrix, right = FALSE)
names(sa_zones_raster) <- "Seasonality_Zone"

# 5. Convert classified raster cells into clean Vector Polygon Boundaries
sa_zones_poly <- as.polygons(sa_zones_raster, aggregate = TRUE)

# 6. Convert to an sf object for mapping and attribute labeling
sa_zones_sf <- st_as_sf(sa_zones_poly)


# Apply descriptive factors
sa_zones_sf$Zone_Name <- factor(sa_zones_sf$Seasonality_Zone,
                                ordered = TRUE,
                                levels = c(1, 2, 3),
                                labels = c("S", 
                                           "Y", 
                                           "W"))

# 7. Map the finalized zone boundaries
ggplot(data = sa_zones_sf) +
  geom_sf(aes(fill = Zone_Name), color = "white", size = 0.1) +
  scale_fill_manual(values = c("#f39c12", "#2ecc71", "#3498db"), 
                    name = "Rainfall Seasonality") +
  theme_minimal() +
  labs(title = "Calculated Rainfall Seasonality Zones of South Africa",
       subtitle = "Generated dynamically via WorldClim monthly baselines",
       caption = "Summer: Oct-Mar | Winter: Apr-Sep")

# 8. Export the boundaries to your local GIS working folder
st_write(sa_zones_sf, "SA_Seasonality_Zones.shp", delete_layer = TRUE)


