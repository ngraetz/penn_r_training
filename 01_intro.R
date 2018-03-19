## Object-oriented language 
  ## class() - functions are defined via inputs of specific classes, outputs of specific classes
  ## Common classes: vectors, data.frames, lists

## Libraries - define functions beyond those included in base R.
  library(data.table)
  library(ggplot2)

######################################################################
### 1. Most important manipulation concepts ##########################
###   - Merge
###   - Collapse
###   - Reshape
######################################################################
  
## Data manipulation concepts
  mmr_file <- "C:/Users/ngraetz/Documents/repos/penn_r_training/data/mmr.csv"
  edu_file <- "C:/Users/ngraetz/Documents/repos/penn_r_training/data/covariates.csv"
  mmr <- fread(mmr_file)
  edu <- fread(edu_file)
  
  ## Alternatively, notice we are doing the extact same thing to both these vectors. We could instead apply the common function over all filepaths.
  data_list <- lapply(c(mmr_file, edu_file), fread)
  class(data_list)
  length(data_list)
  class(data_list[[1]])
  mmr <- data_list[[1]]
  edu <- data_list[[2]]
  
  ## Notice fread() is a function defined by the data.table library. There are many other functions in base R, or in other libraries. You can also define your own.
  calc_qx <- function(nmx, n, nax) {
    qx <- (n * nmx) / (1 + (n-nax) * nmx)
    return(qx)
  }
  calc_qx(nmx = 0.001, n = 5, nax = 2.5)
  
  ## Apply over a list of nmx's
  some_nmxs <- seq(0.001, 0.01, .001)
  lapply(some_nmxs, calc_qx, n = 5, nax = 2.5)
  
  ## Simpler multiply function over a numeric vector...
  multiply <- function(x) {
    return(x * 1000)
  }
  lapply(c(1,2,3,4,5), multiply)
  
## Merge
  head(mmr)
  head(edu)
  all_data <- merge(mmr, edu, by = c('year_id', 'location_name'))
  head(all_data)
  
## Collapse
  test <- all_data[, list(mean_ldi = mean(ldi)), by = c('year_id','super_region_name')]
  
## Reshape (long to wide, wide to long... notice in data.table, these are dcast and melt, respectively.)
  test2 <- all_data[, c('year_id','location_name','mmr')]
  head(test2) # These data are "long" with respect to year, "long on year"
  ## Reshape to be "wide" on year
  test2_wide <- dcast(test2, location_name ~ year_id, value.var = 'mmr')
  head(test2_wide)
  test2[, year_id := paste0('mmr_', year_id)]
  test2_wide <- dcast(test2, location_name ~ year_id, value.var = c('mmr'))
  head(test2_wide)
  ## Reshape to be "wide" on multiple variables
  test3 <- all_data[, c('year_id','location_name','mmr','ldi')]
  test3_wide <- dcast(test3, location_name ~ year_id, value.var = c('mmr','ldi'))
  
  ## Reshape back to be "long" on year
  test2_long = melt(test2_wide, id.vars = c('location_name'), measure.vars = c('mmr_1990','mmr_1995','mmr_2000','mmr_2005','mmr_2010','mmr_2015'))
  head(test2_long)
  test3_long = melt(test3_wide, id.vars = c('location_name'), measure.vars = c(grep('mmr', names(test3_wide), value = TRUE),
                                                                               grep('ldi', names(test3_wide), value = TRUE)))
  head(test3_long)
  
######################################################################
### 2. Data visualization ############################################
######################################################################

## ggplot2 library - features require data, aesthetics, themes
data_by_country <- all_data[location_name %in% c('Afghanistan','Mexico','United States','Bangladesh','Ghana')]
ggplot() + 
  geom_line(data = data_by_country,
            aes(x = year_id,
                y = mmr,
                color = location_name))

ggplot() + 
  geom_line(data = data_by_country,
            aes(x = year_id,
                y = mmr,
                color = location_name),
            size = 1) +
  theme_minimal() +
  labs(title = 'MMR by country',
       x = 'Year',
       y = 'MMR')

ggplot() + 
  geom_point(data = all_data[year_id==2015, ],
             aes(x = maternal_education,
                 y = mmr,
                 color = super_region_name),
                 size = 3) +
  theme_minimal() +
  labs(title = 'MMR by country',
       x = 'Education',
       y = 'MMR')






















