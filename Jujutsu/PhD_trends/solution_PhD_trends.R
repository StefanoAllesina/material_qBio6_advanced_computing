rm(list = ls())
library(tidyverse)
library(readxl)

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
    select(field, male, female) %>% # take only male and female (other info is redundant)
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

# be careful: some names have changed: For example
# field                           male female  year
# <chr>                          <dbl>  <dbl> <dbl>
# 1 Neurosciences, neurobiology      469    568  2018
# 2 Neurosciences, neurobiology      482    503  2017
# 3 Neurosciences, neurobiology      485    512  2016
# 4 Neurosciences, neurobiology      519    572  2015
# 5 Neurosciences and neurobiology   520    530  2014
# 6 Neurosciences and neurobiology   447    570  2013

# to make sure, use something like this
plot_PhD_in_time <- function(sed, label){
  pl <- sed %>% filter(grepl(label, field)) %>% 
    gather(sex, count, -field, -year) %>% 
    ggplot() +
    aes(x = year, y = count, fill = sex) + 
    geom_col()
  return(pl)
}

#plot_PhD_in_time(sed, "Neurosciences")

