library(bibtex)

# Libraries used
libraries <- c('base',
               'tidyverse',
               'doSNOW',
               'arcgisbinding',
               'raster',
               'sf',
               'rgdal')

# Write bib-file

write.bib(libraries, file='C:\\Files\\R-scripts.bib')

# Print text string with packages


df <- data.frame(libraries)
version <- unlist(lapply(apply(df, 1, FUN = packageVersion), FUN = as.character))
df["version"] <- version

lines <- apply(df, 1, FUN = function(x) paste0("'", x[1], "' v. ", x[2], collapse = ""))

composite <- paste0(lines, collapse = ", ")
composite


RStudio.Version()


R.version
