# Load data

# library
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(units)

# Load areas
areas <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/de fem nye/ForelobigeNNPGraenser.shp")
areas <- areas %>% 
  transmute(Name = Bemark,
            Shape_outline_m = Shape_Leng,
            Shape_Area_ha = Shape_Area/10000)
areas <- st_transform(areas, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

# Plot
ggplot(areas) +
  geom_sf(fill = NA, col = "red")

# Make a results table
df <- st_drop_geometry(areas)

# Load jordart
jordart <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/ny_jord/kort overførelse.shp")
# ggplot(jordart) +
#   geom_sf(aes(fill = Category_M), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "black")
jordart <- st_transform(jordart, st_crs(areas))

intersect <- st_intersection(areas, jordart)
intersect <- intersect %>% 
  mutate(jord_area_ha = drop_units(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, Jordart) %>% 
  summarise(area = sum(jord_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = Jordart, names_prefix = "jord_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("jord"), ~ round(./Shape_Area_ha * 100, 1)))


# Load natura2000 lys
n2000lys <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/N2K_lys/np3b2020_lysaaben_natur2016_2019.shp")
# ggplot(n2000lys) +
#   geom_sf(aes(fill = NATURTYPE), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "red")
n2000lys <- st_transform(n2000lys, st_crs(areas))

intersect <- st_intersection(areas, n2000lys)
intersect <- intersect %>% 
  mutate(n2000_area_ha = drop_units(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, Habitatnat) %>% 
  summarise(area = sum(n2000_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = Habitatnat, names_prefix = "habLys_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("habLys"), ~ round(./Shape_Area_ha * 100, 1)))


# Load natura2000 skov
n2000skov <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/N2K_skov/np3b2020_skov_2016_2019.shp")
# ggplot(n2000skov) +
#   geom_sf(aes(fill = Nattyp), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "red")
n2000skov <- st_transform(n2000skov, st_crs(areas))

intersect <- st_intersection(areas, n2000skov)
intersect <- intersect %>% 
  mutate(n2000_area_ha = drop_units(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, Nattyp) %>% 
  summarise(area = sum(n2000_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = Nattyp, names_prefix = "habSkov_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("habSkov"), ~ round(./Shape_Area_ha * 100, 1)))

# Load skov25
skov25 <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/25_skov/25skov.shp")
# ggplot(skov25) +
#   geom_sf(aes(fill = p25_type), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "red")
skov25 <- st_transform(skov25, st_crs(areas))

intersect <- st_intersection(areas, skov25)
intersect <- intersect %>% 
  mutate(skov25_area_ha = drop_units(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, p25_type) %>% 
  summarise(area = sum(skov25_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = p25_type, names_prefix = "skov25_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("skov25"), ~ round(./Shape_Area_ha * 100, 1)))

# Load §3 natur
p3_natur <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/bes_natur/bes_natur_dk.shp")
# ggplot(skov25) +
#   geom_sf(aes(fill = p25_type), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "red")
p3_natur <- st_transform(p3_natur, st_crs(areas))

intersect <- st_intersection(areas, p3_natur)
intersect <- intersect %>% 
  mutate(p3_natur_area_ha = drop_units(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, Natyp_navn) %>% 
  summarise(area = sum(p3_natur_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = Natyp_navn, names_prefix = "p3_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("p3_"), ~ round(./Shape_Area_ha * 100, 1)))


# Load §3 vand
p3_vand <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/bes_natur/bes_vandløb.shp")
p3_vand <- st_transform(p3_vand, st_crs(areas))

intersect <- st_intersection(areas, p3_vand)
intersect <- intersect %>% 
  mutate(p3_vand_m = drop_units(st_length(intersect)))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name) %>% 
  summarise(p3_vand_m = sum(p3_vand_m))

df <- left_join(df, intersect)


# Load DHM
dhm <- raster("C:/Files/GIS/DHM_terrain/DHM_terrain_dk.tif")
crs(dhm) <- crs(areas)

names <- areas$Name
dhm.res <- data.frame(Names = names)

for(i in seq_along(names)) {
  area_x <- areas[areas$Name == names[i], ]
  dhm_x <- crop(dhm, area_x)
  dhm_y <- mask(dhm_x, area_x)

  dhm.res$dtm.range[i] <- diff(range(dhm_y[], na.rm = T))
  dhm.res$dtm.sd[i] <- sd(dhm_y[], na.rm = T)
  
  tri <- terrain(dhm_y, opt = "TRI")
  dhm.res$TRI.median[i] <- median(tri[], na.rm = T)
  dhm.res$TRI.q975[i] <- quantile(tri[], probs = c(0.975), na.rm = TRUE)
  
  # SD for 5 neigbourhood
  focal_sd <- function(x, w = 5) {
    m <- matrix(1, nc=w, nr=w)
    f <- focal(x, m, fun = sd)
  }
  sd5 <- focal_sd(dhm_y)
  dhm.res$sd5.median[i] <- median(sd5[], na.rm = T)
  dhm.res$sd5.q975[i] <- quantile(sd5[], probs = c(0.975), na.rm = TRUE)
}

df <- bind_cols(df, dhm.res[-1])


# artsscore
artsscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/artsscore")
crs(artsscore) <- crs(areas)
bioscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/bioscore")
crs(bioscore) <- crs(areas)

names <- areas$Name
score.res <- data.frame(Names = names)

for(i in seq_along(names)) {
  area_x <- areas[areas$Name == names[i], ]
  artsscore_x <- crop(artsscore, area_x)
  artsscore_y <- mask(artsscore_x, area_x)
  
  bioscore_x <- crop(bioscore, area_x)
  bioscore_y <- mask(bioscore_x, area_x)
  
  # plot(stack(artsscore_y, bioscore_y))
  
  score.res$artsscore.median[i] <- median(artsscore_y[], na.rm = T)
  score.res$artsscore.max[i] <- max(artsscore_y[], na.rm = T)
  
  score.res$bioscore.median[i] <- median(bioscore_y[], na.rm = T)
  score.res$bioscore.max[i] <-  max(bioscore_y[], na.rm = T)
}
df <- bind_cols(df, score.res[-1])



# Write final output of dataset
write_excel_csv(df, "O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/output/result.csv")
