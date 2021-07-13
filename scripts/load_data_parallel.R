# Load data


# Setup -------------------------------------------------------------------

# Load libraries
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tictoc)

# Load DHM
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

library(doSNOW)
cluster.size <- 2
cl <- parallel::makeCluster(cluster.size)
registerDoSNOW(cl)

# Load areas --------------------------------------------------------------
areas5 <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/de fem nye/ForelobigeNNPGraenser.shp")
areas5 <- st_transform(areas5, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas5 <- areas5 %>% 
  transmute(Name = paste0("De5_", Bemark),
            Shape_Area_ha = as.numeric(st_area(areas5))/10000)

areas.marianne <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/NNP_gennamgang0707/NNP_gennemgang_0707.shp") %>% 
  filter(gennemgang %in% 1:2)
areas.marianne <- st_transform(areas.marianne, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas.marianne <- areas.marianne %>% 
  transmute(Name = skovnavn,
            Shape_Area_ha = as.numeric(st_area(areas.marianne))/10000)

areas <- bind_rows(areas5, areas.marianne)
areas <- st_make_valid(areas)

# areas <- areas[which(areas$Name %in% c("Tipperne", "Vejers Plantage, sydlige del")), ]
# areas <- areas[19, ]
areas <- areas[1:2, ]

# Plot
# ggplot(areas) +
#   geom_sf(fill = NA, col = "red")

# Make a results table
df <- st_drop_geometry(areas)


# Load Jordart data -------------------------------------------------------
jordart <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/ny_jord/kort overførelse.shp")
# ggplot(jordart) +
#   geom_sf(aes(fill = Category_M), col = NA) +
#   geom_sf(data = areas, fill = NA, col = "black")
jordart <- st_transform(jordart, st_crs(areas))

intersect <- st_intersection(areas, jordart)
intersect <- intersect %>% 
  mutate(jord_area_ha = as.numeric(st_area(intersect)/10000))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name, Jordart) %>% 
  summarise(area = sum(jord_area_ha)) %>% 
  pivot_wider(values_from = area, names_from = Jordart, names_prefix = "jord_", values_fill = 0)

df <- left_join(df, intersect)
df <- df %>% mutate(across(matches("jord"), ~ round(./Shape_Area_ha * 100, 1)))

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
skov25 <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/25_skov/25skov.shp")
skov25 <- st_transform(skov25, st_crs(areas))
skov25 <- skov25 %>% transmute(skov25 = beskrivels) %>% 
  st_make_valid()

# Find beskyttede natur arealer indenfor områderne og fjern eventuelle interne
# overlap for §3, for N2000 kan der være mosaikker
n2000lys.i <- st_intersection(areas[1], n2000lys)
n2000skov.i <- st_intersection(areas[1], n2000skov)
p3_natur.i <- st_intersection(areas[1], p3_natur) %>% st_difference
skov25.i <- st_intersection(areas[1], skov25) %>% st_difference

# Intersect a and b and keep all fields and non overlapping geometries
union_arc_style <<- function(a, b) {
  if(nrow(a) == 0 | nrow(b) == 0) return(bind_rows(a, b))
  a <- st_buffer(a, 0)
  b <- st_buffer(b, 0)
  a.union <- st_buffer(st_union(a), 0)
  b.union <- st_buffer(st_union(b), 0)
  
  # a not in b
  a.only <- st_difference(a, b.union)
  
  # b not in a
  b.only <- st_difference(b, a.union)
  
  # a and b overlap
  a.b.overlap <- st_intersection(a, b[-1])
  
  # Combine all areas
  union <- bind_rows(a.only, b.only, a.b.overlap)
  
  return(union)
}

n2000 <- union_arc_style(n2000lys.i, n2000skov.i)
p.natur <- union_arc_style(p3_natur.i, skov25.i)

n2000.union <- st_buffer(st_union(n2000), 0)
p.natur.minus.2000 <- st_difference(p.natur, n2000.union)

beskyttet.natur <- bind_rows(n2000, p.natur.minus.2000)

beskyttet.natur.df <- beskyttet.natur %>%
  bind_cols(area_ha = as.numeric(st_area(beskyttet.natur))/10000) %>%
  mutate(area_ha = ifelse(!is.na(ArealAndel), area_ha * ArealAndel / 100, area_ha),
         ArealAndel = NULL) %>% 
  st_drop_geometry() %>%
  remove_rownames()

beskyttet.natur.df.clean <- beskyttet.natur.df %>% 
  mutate(n2000lys = ifelse(!is.na(n2000lys), paste0("n2000lys_", n2000lys), NA),
         n2000skov = ifelse(!is.na(n2000skov), paste0("n2000skov_", n2000skov), NA),
         p3_natur = ifelse(!is.na(p3_natur), paste0("p3_natur_", p3_natur), NA),
         skov25 = ifelse(!is.na(skov25), paste0("skov25_", skov25), NA)) %>% 
  transmute(Name, 
            beskyttelse = coalesce(n2000lys, n2000skov, p3_natur, skov25),
            area_ha) %>% 
  group_by(Name, beskyttelse) %>% 
  summarise(area_ha = sum(area_ha)) %>% 
  right_join(df[c("Name", "Shape_Area_ha")], by = "Name") %>% 
  mutate(area = area_ha / Shape_Area_ha * 100,
         Shape_Area_ha = NULL,
         area_ha = NULL)

beskyttet.natur.total <- beskyttet.natur.df.clean %>% 
  group_by(Name) %>% summarise(beskyttet.natur.total = sum(area))

beskyttet.natur.df.wide <- beskyttet.natur.df.clean %>%
  arrange(beskyttelse) %>% 
  pivot_wider(names_from = beskyttelse, values_from = area) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  left_join(beskyttet.natur.total, by = "Name")

df <- df %>% left_join(beskyttet.natur.df.wide)


# Beskyttede vandløb ------------------------------------------------------
p3_vand <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/bes_natur/bes_vandløb.shp")
p3_vand <- st_transform(p3_vand, st_crs(areas))

intersect <- st_intersection(areas, p3_vand)
intersect <- intersect %>% 
  mutate(p3_vand_m = as.numeric(st_length(intersect)))
intersect <- intersect %>% 
  st_drop_geometry() %>% 
  group_by(Name) %>% 
  summarise(p3_vand_m = sum(p3_vand_m))

df <- left_join(df, intersect)



# Terrain data ------------------------------------------------------------
names <- areas$Name
n <- length(names)

pb <- txtProgressBar(max = n, style = 3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))


# Load current maps
timestamp()
tic()
dhm.res <- foreach(i=1:n,                         
                   .packages=c('raster', 'tidyverse', 'sf'), 
                   .combine = c,
                   .inorder = TRUE,
                   .options.snow = opts) %dopar% {
                    area_x <- areas[areas$Name == names[i], ]
                    dhm_x <- crop(dhm, area_x)
                    dhm_y <- mask(dhm_x, area_x)
                    dhm.res <- c()

                    dhm.res$dtm.range <- diff(range(dhm_y[], na.rm = T))
                    dhm.res$dtm.sd <- sd(dhm_y[], na.rm = T)

                    tri <- terrain(dhm_y, opt = "TRI")
                    dhm.res$TRI.median <- median(tri[], na.rm = T)
                    dhm.res$TRI.q975 <- quantile(tri[], probs = c(0.975), na.rm = TRUE)

                    # SD for 3 neigbourhood
                    focal_sd <- function(x, w = 3) {
                      m <- matrix(1, nc = w, nr = w)
                      f <- focal(x, m, fun = sd)
                    }
                    sd3 <- focal_sd(dhm_y)
                    dhm.res$sd3.median <- median(sd3[], na.rm = T)
                    dhm.res$sd3.q975 <- quantile(sd3[], probs = c(0.975), na.rm = TRUE)

                    # Slope
                    slope <- terrain(dhm_y, opt = "slope", unit = "degrees")
                    steep.pct <- mean(na.omit(slope[]) > 15) * 100
                    dhm.res$steep.pct <- steep.pct

                    return(dhm.res)
}
toc()

df <- bind_cols(df, dhm.res)

gc()

# artsscore
artsscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/artsscore")
crs(artsscore) <- crs(areas)
bioscore <- raster("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/Biodiversitetskort_arcgis/bioscore")
crs(bioscore) <- crs(areas)

names <- areas$Name
n <- length(names)

timestamp()
tic()
score.res <- foreach(i=1:n,                         
                   .packages=c('raster', 'tidyverse', 'sf'), 
                   .combine = bind_rows,
                   .inorder = TRUE,
                   .options.snow = opts) %dopar% {
                     score.res <- c()
                    area_x <- areas[areas$Name == names[i], ]
                    artsscore_x <- crop(artsscore, area_x)
                    artsscore_y <- mask(artsscore_x, area_x)
                    
                    bioscore_x <- crop(bioscore, area_x)
                    bioscore_y <- mask(bioscore_x, area_x)
                    
                    # plot(stack(artsscore_y, bioscore_y))
                    
                    score.res$artsscore.median <- median(artsscore_y[], na.rm = T)
                    score.res$artsscore.max <- max(artsscore_y[], na.rm = T)
                    score.res$artsscore.sd <- sd(artsscore_y[], na.rm = T)
                    
                    score.res$bioscore.median <- median(bioscore_y[], na.rm = T)
                    score.res$bioscore.max <-  max(bioscore_y[], na.rm = T)
                    score.res$bioscore.sd <-  sd(bioscore_y[], na.rm = T)
                    
                    return(score.res)
}
toc()
df <- bind_cols(df, score.res)

gc()


# 1000 m ------------------------------------------------------------------

timestamp()
tic()
res1000 <- foreach(i=1:n,                         
                     .packages=c('sf', 'tidyverse'), 
                     .combine = bind_rows,
                     .inorder = TRUE,
                     .options.snow = opts) %dopar% {
                       res <- c()
  blob <- st_buffer(areas[i,], 1000)
  buffer <- st_difference(blob, areas[i,])
  
  n2000lys.i <- st_intersection(buffer[1], n2000lys) %>% st_difference
  n2000skov.i <- st_intersection(buffer[1], n2000skov) %>% st_difference
  p3_natur.i <- st_intersection(buffer[1], p3_natur) %>% filter(p3_natur != "Sø") %>% st_difference
  skov25.i <- st_intersection(buffer[1], skov25) %>% st_difference
  
  n2000 <- union_arc_style(n2000lys.i, n2000skov.i)
  p.natur <- union_arc_style(p3_natur.i, skov25.i) %>% st_buffer(., 0)
  
  n2000.union <- st_buffer(st_union(n2000), 0)
  if(length(n2000.union) > 0) {
    p.natur.minus.2000 <- st_difference(p.natur, n2000.union)
  } else {
    p.natur.minus.2000 <- p.natur
  }
  
  beskyttet.natur <- bind_rows(n2000, p.natur.minus.2000)
  
  beskyttet.natur.df <- beskyttet.natur %>% 
    bind_cols(area = as.numeric(st_area(beskyttet.natur))) %>% 
    st_drop_geometry() %>%
    remove_rownames()
  
  res$buffer1000.beskyttet.natur <- beskyttet.natur.df %>% 
    mutate(n2000lys = ifelse(!is.na(n2000lys), paste0("n2000lys_", n2000lys), NA),
           n2000skov = ifelse(!is.na(n2000skov), paste0("n2000skov_", n2000skov), NA),
           p3_natur = ifelse(!is.na(p3_natur), paste0("p3_natur_", p3_natur), NA),
           skov25 = ifelse(!is.na(skov25), paste0("skov25_", skov25), NA)) %>% 
    transmute(Name, 
              beskyttelse = coalesce(n2000lys, n2000skov, p3_natur, skov25),
              area) %>% 
    group_by(Name) %>% 
    summarise(area = sum(area)/as.numeric(st_area(buffer)) * 100) %>% 
    pull(area)
  
  artsscore_x <- crop(artsscore, buffer)
  artsscore_y <- mask(artsscore_x, buffer)
  bioscore_x <- crop(bioscore, buffer)
  bioscore_y <- mask(bioscore_x, buffer)

  res$buffer1000.artsscore.median <- median(artsscore_y[], na.rm = T)
  res$buffer1000.artsscore.max <- max(artsscore_y[], na.rm = T)
  res$buffer1000.artsscore.sd <- sd(artsscore_y[], na.rm = T)
  
  res$buffer1000.bioscore.median <- median(bioscore_y[], na.rm = T)
  res$buffer1000.bioscore.max <-  max(bioscore_y[], na.rm = T)
  res$buffer1000.bioscore.sd <-  sd(bioscore_y[], na.rm = T)
  
  return(res)
}
toc()
df <- bind_cols(df, res1000)

gc()

# 5000 m ------------------------------------------------------------------

timestamp()
tic()
res5000 <- foreach(i=1:n,                         
               .packages=c('sf', 'tidyverse'), 
               .combine = bind_rows,
               .inorder = TRUE,
               .options.snow = opts) %dopar% {
                 res <- c()
  
  blob <- st_buffer(areas[i,], 5000)
  buffer <- st_difference(blob, areas[i,])
  
  n2000lys.i <- st_intersection(buffer[1], n2000lys) %>% st_difference
  n2000skov.i <- st_intersection(buffer[1], n2000skov) %>% st_difference
  p3_natur.i <- st_intersection(buffer[1], p3_natur) %>% filter(p3_natur != "Sø") %>% st_difference
  skov25.i <- st_intersection(buffer[1], skov25) %>% st_difference
  
  n2000 <- union_arc_style(n2000lys.i, n2000skov.i)
  p.natur <- union_arc_style(p3_natur.i, skov25.i) %>% st_buffer(., 0)
  
  n2000.union <- st_buffer(st_union(n2000), 0)
  if(length(n2000.union) > 0) {
    p.natur.minus.2000 <- st_difference(p.natur, n2000.union)
  } else {
    p.natur.minus.2000 <- p.natur
  }
  
  beskyttet.natur <- bind_rows(n2000, p.natur.minus.2000)
  
  beskyttet.natur.df <- beskyttet.natur %>% 
    bind_cols(area = as.numeric(st_area(beskyttet.natur))) %>% 
    st_drop_geometry() %>%
    remove_rownames()
  
  res$buffer5000.beskyttet.natur <- beskyttet.natur.df %>% 
    mutate(n2000lys = ifelse(!is.na(n2000lys), paste0("n2000lys_", n2000lys), NA),
           n2000skov = ifelse(!is.na(n2000skov), paste0("n2000skov_", n2000skov), NA),
           p3_natur = ifelse(!is.na(p3_natur), paste0("p3_natur_", p3_natur), NA),
           skov25 = ifelse(!is.na(skov25), paste0("skov25_", skov25), NA)) %>% 
    transmute(Name, 
              beskyttelse = coalesce(n2000lys, n2000skov, p3_natur, skov25),
              area) %>% 
    group_by(Name) %>% 
    summarise(area = sum(area)/as.numeric(st_area(buffer)) * 100) %>% 
    pull(area)
  
  artsscore_x <- crop(artsscore, buffer)
  artsscore_y <- mask(artsscore_x, buffer)
  bioscore_x <- crop(bioscore, buffer)
  bioscore_y <- mask(bioscore_x, buffer)
  
  res$buffer5000.artsscore.median <- median(artsscore_y[], na.rm = T)
  res$buffer5000.artsscore.max <- max(artsscore_y[], na.rm = T)
  res$buffer5000.artsscore.sd <- sd(artsscore_y[], na.rm = T)
  
  res$buffer5000.bioscore.median <- median(bioscore_y[], na.rm = T)
  res$buffer5000.bioscore.max <-  max(bioscore_y[], na.rm = T)
  res$buffer5000.bioscore.sd <-  sd(bioscore_y[], na.rm = T)
  
  return(res)
}
toc()
df <- bind_cols(df, res5000)

stopCluster(cl)
gc()

# Write final output of dataset -------------------------------------------
df[-1] <- round(df[-1], 2)
df$Name <- str_replace_all(df$Name, ",", " -")

write_excel_csv(df, "O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/output/result12072021.csv")
