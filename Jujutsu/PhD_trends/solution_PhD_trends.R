rm(list = ls())
library(tidyverse)
library(readxl)

######################################
# 1
######################################

# function for downloading/massaging data for a single year
read_excel_from_url <- function(url, my_year, skip = 1, read_lines = NA){
  temp <- tempfile()
  download.file(url,temp)  
  if(!is.na(read_lines)) {
    dd <- read_xlsx(temp, skip = skip, n_max = read_lines)
  } else {
    dd <- read_xlsx(temp, skip = skip)
  }
  unlink(temp)
  # change col names for easier typing
  colnames(dd) <- c("field", "total", "male", "female", "perc_female") 
  dd <- dd %>% 
    select(field, total) %>% # take only total
    filter(!is.na(field)) %>% # remove empty lines
    add_column(year = my_year)
  return(dd)
}

# info on table location
tt <- read_csv("urls_and_skip_NSF_SED.csv")
sed <- tibble()
for (i in 1:nrow(tt)){
  sed <- bind_rows(sed, read_excel_from_url(tt$url[i],
                                            tt$year[i],
                                            tt$skip[i],
                                            tt$read[i]))
}

######################################
# 2
######################################

# be careful: some names have changed: For example
# Neurosciences, neurobiology
# Neurosciences and neurobiology
# to select certain fields and normalize the field name, join with lookup table
# sed <- sed %>% inner_join(read_csv("lookup_fields_filter.csv"))

######################################
# 3
######################################

# to plot use something like this
plot_PhD_in_time <- function(my_tibble){
  pl <- my_tibble %>% 
    ggplot() +
    aes(x = year, y = total, colour = name_to_use) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~name_to_use, scales = "free") + 
    theme(legend.position = "none")
  return(pl)
}

#plot_PhD_in_time(sed)

######################################
# 4
######################################

# max change

######################################
# 5
######################################

# # compute the correlation between any
# # two fields using cor
# 
# cdt <- sed %>%
#   select(name_to_use, total, year) %>%
#   spread(name_to_use, total) %>%
#   select(-year) %>%
#   cor()
# 
# # transform back to tibble for plotting
# 
# cdt <- cdt %>%
#   as_tibble() %>%
#   add_column(field1 = rownames(cdt)) %>%
#   gather(field, correlation, -field1)
# 
# # plot using geom_tile
# 
# ggplot(cdt) +
#   aes(x = field, y = field1, fill = correlation) +
#   geom_tile() +
#   scale_fill_gradient2()

######################################
# 6
######################################

# # as 5, but save the matrix to compute eigenvectors
# cdt <- sed %>% 
#   select(name_to_use, total, year) %>% 
#   spread(name_to_use, total) %>% 
#   select(-year) %>% 
#   cor() 
# 
# # use the leading eigenvector to order
# 
# M <- as.matrix(cdt)
# my_order <- colnames(M)[order(eigen(M)$vectors[,1])]
#
# # build the tibble
# 
# cdt <- cdt %>%
#   as_tibble() %>%
#   add_column(field1 = rownames(cdt)) %>%
#   gather(field, correlation, -field1)
# 
# # use factors to force the order in the plot
# 
# cdt <- cdt %>% mutate(field = factor(field, levels = my_order),
#                       field1 = factor(field1, levels = my_order))
# 
# # plot using geom_tile
# 
# ggplot(cdt) +
#   aes(x = field, y = field1, fill = correlation) +
#   geom_tile() +
#   scale_fill_gradient2()
