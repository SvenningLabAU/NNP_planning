# Extra load

areas <- st_read("O:/Nat_Ecoinformatics/C_Write/_Proj/NaturNationalparker_au233076_au135847/data/NNP/NNP_nov/tre_NNP_nov.shp")
areas <- st_transform(areas, "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
areas <- areas %>% 
  transmute(Name = paste0("November_", Bemark),
            Shape_Area_ha = as.numeric(st_area(.))/10000)
areas <- st_make_valid(areas)

# Make a results table
df <- st_drop_geometry(areas)
