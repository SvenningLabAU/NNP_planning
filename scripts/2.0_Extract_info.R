# Extract information from layers

# Setup -------------------------------------------------------------------

# Load libraries
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tictoc)
library(doSNOW)

# Load data
data <- read_rds("builds/data.rds")
list2env(data, .GlobalEnv)
rm(data)

# Subset for testing
# areas <- areas[which(areas$Name %in% c("Tipperne", "Vejers Plantage, sydlige del")), ]
# areas <- areas[19, ]
# areas <- areas[1:2, ]

# Plot
# ggplot(areas) +
#   geom_sf(fill = NA, col = "red")

# Prep cluster
cluster.size <- 8
cl <- parallel::makeCluster(cluster.size)
registerDoSNOW(cl)

names <- areas$Name
n <- length(names)

pb <- txtProgressBar(max = n, style = 3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

# Create a ArcGIS like intersect function:
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
  # st_buffer(., 0) removes lines and points and keeps only polygons
  a.b.overlap <- st_intersection(a, b[-1]) %>% st_buffer(0)
  
  # Combine all areas
  union <- bind_rows(a.only, b.only, a.b.overlap)

  return(union)
}

# Start parallel ------------------------------------------------
res <- foreach(i=1:n,
               .packages=c('raster', 'tidyverse', 'sf'), 
               .combine = bind_rows,
               .inorder = FALSE,
               .options.snow = opts) %dopar% {
                 # Make a results list
                 res <- c()
                 res$Name <- names[i]
                 
                 # Select area
                 area_x <- areas[areas$Name == names[i], ]
                 
                 # Area of the park
                 park.area <- st_area(area_x) %>% as.numeric

                 # Beskyttet natur ---------------------------------------------------------
                 
                 # Find beskyttede natur arealer indenfor områderne
                 # - fjern eventuelle interne overlap for §3, 
                 #   for N2000 kan der være mosaikker så det kan ikke gøres her
                 # st_buffer(., 0) removes lines and points and keeps only polygons
                 n2000lys.i <- st_intersection(area_x[1], n2000lys) %>% st_buffer(0)
                 n2000skov.i <- st_intersection(area_x[1], n2000skov) %>% st_buffer(0)
                 p3_natur.i <- st_intersection(area_x[1], p3_natur) %>% st_difference %>% st_buffer(0)
                 p25_skov.i <- st_intersection(area_x[1], p25_skov) %>% st_difference %>% st_buffer(0)

                 # Union (ArcStyle) the nature types
                 n2000 <- union_arc_style(n2000lys.i, n2000skov.i)
                 p.natur <- union_arc_style(p3_natur.i, p25_skov.i)

                 if(nrow(n2000) > 0) {
                   # Remove overlaps of p in n2000
                   n2000.union <- st_union(n2000)
                   p.natur.minus.2000 <- st_difference(p.natur, n2000.union)
                 } else {
                   p.natur.minus.2000 <- p.natur
                 }
                 
                 # Combine them all
                 beskyttet.natur <- bind_rows(n2000, p.natur.minus.2000)

                 # Create a dataset of the types
                 beskyttet.natur.df <- beskyttet.natur %>%
                   bind_cols(area = as.numeric(st_area(.))) %>%
                   mutate(
                     area = ifelse(!is.na(ArealAndel), area * ArealAndel / 100, area),
                     ArealAndel = NULL
                   ) %>%
                   st_drop_geometry() %>%
                   remove_rownames() %>%
                   mutate(
                     n2000lys = ifelse(!is.na(n2000lys), paste0("n2000lys_", n2000lys), NA),
                     n2000skov = ifelse(!is.na(n2000skov), paste0("n2000skov_", n2000skov), NA),
                     p3_natur = ifelse(!is.na(p3_natur), paste0("p3natur_", p3_natur), NA),
                     skov25 = ifelse(!is.na(p25_skov), paste0("p25skov_", p25_skov), NA)
                   ) %>%
                   transmute(beskyttelse = coalesce(n2000lys, n2000skov, p3_natur, skov25),
                             area) %>%
                   group_by(beskyttelse) %>%
                   summarise(area.pct = sum(area) / park.area * 100)
                 
                 # Calculate total
                 beskyttet.natur.total <- beskyttet.natur.df %>%
                   summarise(beskyttet.natur.total = sum(area.pct))
                 
                 # Transmute to a wide and combine with total
                 beskyttet.natur.df.wide <- beskyttet.natur.df %>%
                   arrange(beskyttelse) %>%
                   pivot_wider(names_from = beskyttelse, values_from = area.pct) %>%
                   bind_cols(beskyttet.natur.total)
                 
                 # Add beskyttet natur res
                 res <- bind_cols(res, beskyttet.natur.df.wide)


                 # Beskyttede vandløb ------------------------------------------------------
                 p3_vand.i <- st_intersection(area_x, p3_vand) %>%
                   st_union %>% 
                   st_length %>% 
                   as.numeric
                 
                 # Add vandløb res
                 res <- bind_cols(res, p3_vand_m = p3_vand.i)

                 
                 # Extract Jordart data -------------------------------------------------------
                 jordart.i <- st_intersection(area_x, jordart)
                 res.jord <- jordart.i %>%
                   mutate(jord_area = st_area(.) %>% as.numeric) %>%
                   st_drop_geometry() %>%
                   group_by(Jordart) %>%
                   summarise(area.pct = sum(jord_area) / park.area * 100) %>%
                   pivot_wider(
                     values_from = area.pct,
                     names_from = Jordart,
                     names_prefix = "jord_",
                     values_fill = 0
                   )
                 
                 # Add jordart res
                 res <- bind_cols(res, res.jord)

                 
                 # Terrain data ------------------------------------------------------------
                 # Subset DHM to park
                 dhm_crop <- crop(dhm, area_x)
                 dhm_mask <- mask(dhm_crop, area_x)

                 # Calculate height range and sd
                 res$dtm.range <- diff(range(dhm_mask[], na.rm = T))
                 res$dtm.sd <- sd(dhm_mask[], na.rm = T)
                 
                 # Calculate TRI
                 tri <- terrain(dhm_mask, opt = "TRI")
                 res$TRI.median <- median(tri[], na.rm = T)
                 res$TRI.q975 <- quantile(tri[], probs = c(0.975), na.rm = TRUE)
                 
                 # SD for 3 neigbourhood
                 focal_sd <- function(x, w = 3) {
                   m <- matrix(1, nc = w, nr = w)
                   f <- focal(x, m, fun = sd)
                 }
                 sd3 <- focal_sd(dhm_mask)
                 res$sd3.median <- median(sd3[], na.rm = T)
                 res$sd3.q975 <- quantile(sd3[], probs = c(0.975), na.rm = TRUE)
                 
                 # Slope
                 slope <- terrain(dhm_mask, opt = "slope", unit = "degrees")
                 steep.pct <- mean(na.omit(slope[]) > 15) * 100
                 res$steep.pct <- steep.pct
                     
                 
                 # Artsscore og Bioscore ---------------------------------------------------
                 # Crop
                 artsscore_crop <- crop(artsscore, area_x)
                 artsscore_mask <- mask(artsscore_crop, area_x)
                 
                 bioscore_crop <- crop(bioscore, area_x)
                 bioscore_mask <- mask(bioscore_crop, area_x)
                 
                 # Calculate score indexes
                 res$artsscore.median <- median(artsscore_mask[], na.rm = T)
                 res$artsscore.mean <- mean(artsscore_mask[], na.rm = T)
                 res$artsscore.max <- max(artsscore_mask[], na.rm = T)
                 res$artsscore.sd <- sd(artsscore_mask[], na.rm = T)
                 
                 res$bioscore.median <- median(bioscore_mask[], na.rm = T)
                 res$bioscore.mean <- mean(bioscore_mask[], na.rm = T)
                 res$bioscore.max <-  max(bioscore_mask[], na.rm = T)
                 res$bioscore.sd <-  sd(bioscore_mask[], na.rm = T)
                       

                 # Oversvømmelse -----------------------------------------------------------
                 lavbundskort.i <- lavbundskort %>% 
                   st_intersection(area_x, lavbundskort) %>% 
                   st_union
                 
                 lavbund.area <- lavbundskort.i %>% 
                   st_area %>% 
                   as.numeric
                 
                 res$lavbund.pct = ifelse(length(lavbund.area) > 0,
                                          lavbund.area / park.area * 100,
                                          0)


                 # Areal anvendelse and Naturalness and Openness  -----------
                 litra.plus.i <- st_intersection(area_x, litra.plus)
                 
                 anvendelse <- litra.plus.i %>% 
                   mutate(area = as.numeric(st_area(.))) %>% 
                   st_drop_geometry() %>%
                   group_by(Anvendelseskode)  %>%
                   summarise(area.pct = sum(area) / park.area * 100) %>%
                   pivot_wider(
                     values_from = area.pct,
                     names_from = Anvendelseskode,
                     names_prefix = "anvendelse_",
                     values_fill = 0
                   )
                 
                 # Add anvendelse res
                 res <- bind_cols(res, anvendelse)

                 natualness <- litra.plus.i %>% 
                   mutate(area = as.numeric(st_area(.)),
                          natural_area = Naturligt * area,
                          not_natural_area = Ikke_hjemmehørende * area,
                          Skov_closed_area = Skov_closed * area,
                          Open_area = Open * area) %>% 
                   st_drop_geometry() %>% 
                   summarise(natural_area_pct = sum(natural_area) / park.area * 100,
                             not_natural_area_pct = sum(not_natural_area) / park.area * 100,
                             Skov_closed_area_pct = sum(Skov_closed_area) / park.area * 100,
                             Open_area_pct = sum(Open_area) / park.area * 100)
                 
                 # Add Naturalness and Openness to res
                 res <- bind_cols(res, natualness)
                 

                 # 1000 m ------------------------------------------------------------------
                 blob <- st_buffer(area_x, 1000)
                 buffer <- st_difference(blob, area_x)
                 
                 n2000lys.i <- st_intersection(buffer, n2000lys) %>% st_union
                 n2000skov.i <- st_intersection(buffer, n2000skov) %>% st_union
                 p3_natur.i <- st_intersection(buffer, p3_natur) %>% filter(p3_natur != "Sø") %>% st_union
                 p25_skov.i <- st_intersection(buffer, p25_skov) %>% st_union
                 
                 protected.area <- c(n2000lys.i, n2000skov.i, p3_natur.i, p25_skov.i) %>% 
                   st_union %>% 
                   st_area
                 
                 res$buffer1000.beskyttet.natur <- as.numeric(protected.area / st_area(buffer) * 100)
                 
                 artsscore_crop <- crop(artsscore, buffer)
                 artsscore_mask <- mask(artsscore_crop, buffer)
                 bioscore_crop <- crop(bioscore, buffer)
                 bioscore_mask <- mask(bioscore_crop, buffer)
                 
                 res$buffer1000.artsscore.median <- median(artsscore_mask[], na.rm = T)
                 res$buffer1000.artsscore.mean <- mean(artsscore_mask[], na.rm = T)
                 res$buffer1000.artsscore.max <- max(artsscore_mask[], na.rm = T)
                 res$buffer1000.artsscore.sd <- sd(artsscore_mask[], na.rm = T)
                 
                 res$buffer1000.bioscore.median <- median(bioscore_mask[], na.rm = T)
                 res$buffer1000.bioscore.mean <- mean(bioscore_mask[], na.rm = T)
                 res$buffer1000.bioscore.max <-  max(bioscore_mask[], na.rm = T)
                 res$buffer1000.bioscore.sd <-  sd(bioscore_mask[], na.rm = T)
                     

                 # 5000 m ------------------------------------------------------------------
                 blob <- st_buffer(area_x, 5000)
                 buffer <- st_difference(blob, area_x)
                 
                 n2000lys.i <- st_intersection(buffer, n2000lys) %>% st_union
                 n2000skov.i <- st_intersection(buffer, n2000skov) %>% st_union
                 p3_natur.i <- st_intersection(buffer, p3_natur) %>% filter(p3_natur != "Sø") %>% st_union
                 p25_skov.i <- st_intersection(buffer, p25_skov) %>% st_union
                 
                 protected.area <- c(n2000lys.i, n2000skov.i, p3_natur.i, p25_skov.i) %>% 
                   st_union %>% 
                   st_area
                 
                 res$buffer5000.beskyttet.natur <- as.numeric(protected.area / st_area(buffer) * 100)
                 
                 artsscore_crop <- crop(artsscore, buffer)
                 artsscore_mask <- mask(artsscore_crop, buffer)
                 bioscore_crop <- crop(bioscore, buffer)
                 bioscore_mask <- mask(bioscore_crop, buffer)
                 
                 res$buffer5000.artsscore.median <- median(artsscore_mask[], na.rm = T)
                 res$buffer5000.artsscore.mean <- mean(artsscore_mask[], na.rm = T)
                 res$buffer5000.artsscore.max <- max(artsscore_mask[], na.rm = T)
                 res$buffer5000.artsscore.sd <- sd(artsscore_mask[], na.rm = T)
                 
                 res$buffer5000.bioscore.median <- median(bioscore_mask[], na.rm = T)
                 res$buffer5000.bioscore.mean <- mean(bioscore_mask[], na.rm = T)
                 res$buffer5000.bioscore.max <-  max(bioscore_mask[], na.rm = T)
                 res$buffer5000.bioscore.sd <-  sd(bioscore_mask[], na.rm = T)
                 
                 return(res)
               }

# Clean up
stopCluster(cl)
gc()

# Relocate columns
res <- res %>% 
  relocate(colnames(.) %>% str_subset("jord") %>% sort, .after = "Name") %>% 
  relocate(colnames(.) %>% str_subset("p25") %>% sort, .after = "Name") %>% 
  relocate(colnames(.) %>% str_subset("p3") %>% sort, .after = "Name") %>% 
  relocate(colnames(.) %>% str_subset("n2000") %>% sort, .after = "Name") %>% 
  relocate(colnames(.) %>% str_subset("anvendelse") %>% sort, .after = "lavbund.pct")

# Bind to area list
df <- left_join(df, res, by = "Name")

# Write final output of dataset -------------------------------------------
# Round to two decimal places
df[-1] <- round(df[-1], 2)
# Replace NA with 0
df[-1] <- df[-1] %>% mutate_all(coalesce, 0)
# Change Names with commas to dash
df$Name <- str_replace_all(df$Name, ",", " -")

# Write output
folder <- "O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/output/"
name <- "result19072021_v1.csv"
write_excel_csv(df, paste(folder, name, sep = "/"))
