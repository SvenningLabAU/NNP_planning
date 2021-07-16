# Load data only

# Setup -------------------------------------------------------------------

# Load libraries
library(raster)
library(tidyverse)
library(sf)
library(rgdal)

# Load DHM -------------------------------------------------------------
if(file.exists("builds/dhm.rds")) {
  dhm <- read_rds("builds/dhm.rds")
} else {
  # install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  library(arcgisbinding)
  arc.check_product()
  # ogrListLayers("O:/AUIT_Geodata/Denmark/Digital_elevation_models/Lidar/DHM_2014.gdb")
  dhm.mosaic <- arc.open("O:/AUIT_Geodata/Denmark/Digital_elevation_models/Lidar/DHM_2014.gdb/Terrain_2014_resample_10m")
  dhm.mosaic <- arc.raster(dhm.mosaic)
  dhm <- as.raster(dhm.mosaic)
  all.equal(crs(dhm), crs(areas))
  write_rds(dhm, "builds/dhm.rds")
}

# Load areas --------------------------------------------------------------
# De 5:
areas5 <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/de fem nye/ForelobigeNNPGraenser.shp")
areas5 <- st_transform(areas5, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas5 <- areas5 %>% 
  transmute(Name = paste0("De5_", Bemark),
            Shape_Area_ha = as.numeric(st_area(areas5))/10000)

# Marianne:
areas.marianne <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/NNP_arealer/NNP_arealer.shp") %>% 
  filter(gennemgang %in% 1:2)
areas.marianne <- st_transform(areas.marianne, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas.marianne <- areas.marianne %>% 
  transmute(Name = skovnavn,
            Shape_Area_ha = as.numeric(st_area(areas.marianne))/10000)

# Engelbreath:
areas.rune <-  st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/NNP_Engelbreth/NNP_Engelbreth.shp")
areas.rune <- st_transform(areas.rune, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas.rune <- areas.rune %>% 
  transmute(Name = paste0("Engelbreth_", Skovnavn),
            Shape_Area_ha = as.numeric(st_area(areas.rune))/10000)

# Combine them all:
areas <- bind_rows(areas5, areas.marianne, areas.rune)
areas <- st_make_valid(areas)

# Make a results table
df <- st_drop_geometry(areas)

# Load Jordart data -------------------------------------------------------
jordart <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/ny_jord/kort overførelse.shp")
jordart <- st_transform(jordart, st_crs(areas))

# Beskyttet natur ---------------------------------------------------------
# Load natura2000 lys
n2000lys <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/N2K_lys/np3b2020_lysaaben_natur2016_2019.shp")
n2000lys <- st_transform(n2000lys, st_crs(areas))
n2000lys <- n2000lys %>% 
  transmute(n2000lys = Habitatnat, ArealAndel) %>% 
  st_make_valid()
# Load natura2000 skov
n2000skov <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/N2K_skov/np3b2020_skov_2016_2019.shp")
n2000skov <- st_transform(n2000skov, st_crs(areas))
n2000skov <- n2000skov %>% transmute(n2000skov = Nattyp, ArealAndel = Arealpct) %>% 
  st_make_valid()
# Load §3 natur
p3_natur <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/bes_natur/bes_natur_dk.shp")
p3_natur <- st_transform(p3_natur, st_crs(areas))
p3_natur <- p3_natur %>% transmute(p3_natur = Natyp_navn) %>% 
  st_make_valid()
# Load §25 skov
p25_skov <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/25_skov/25skov.shp")
p25_skov <- st_transform(p25_skov, st_crs(areas))
p25_skov <- p25_skov %>% transmute(p25_skov = beskrivels) %>% 
  st_make_valid()

# Beskyttede vandløb ------------------------------------------------------
p3_vand <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/bes_natur/bes_vandløb.shp")
p3_vand <- st_transform(p3_vand, st_crs(areas))


# Artsscore og Bioscore  ------------------------------------------------------
artsscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/artsscore")
crs(artsscore) <- crs(areas)
bioscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/bioscore")
crs(bioscore) <- crs(areas)

# Oversvømmelse -----------------------------------------------------------
# Load lavbundskort
lavbundskort <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Tekstur_2014/Tekstur2014.shp")
lavbundskort <- st_transform(lavbundskort, st_crs(areas))

# Naturalness and Openness  ----------------------------------------------------
# Load skovkort
litra <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/SABA_litra/SABA_litra.shp")
litra <- st_transform(litra, st_crs(areas))
litra.tolkning <- readxl::read_xlsx("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/anvendelse_SABA_litra.xlsx")
litra.plus <- litra %>% 
  transmute(Anvendelseskode = hovedanven) %>% 
  left_join(litra.tolkning, by = "Anvendelseskode")

# Save data as *.rds ----------------------------
data <- list(areas = areas,
             df = df,
             dhm = dhm, 
             jordart = jordart, 
             n2000lys = n2000lys, 
             n2000skov = n2000skov, 
             p3_natur = p3_natur, 
             p25_skov = p25_skov, 
             p3_vand = p3_vand, 
             artsscore = artsscore, 
             bioscore = bioscore, 
             lavbundskort = lavbundskort, 
             litra.plus = litra.plus)
write_rds(data, "builds/data.rds")
