
library(RODBC)
library(ggplot2)
library(ggvis)
library(rgeos)
library(magrittr)
library(RColorBrewer)
library(dplyr)

# Getting shape data

county_dat <- map_data('county')
wa_counties <- filter(county_dat, region == 'washington')

wa_counties %<>% mutate(county = Hmisc::capitalize(subregion))

# Getting pop data

con <- odbcConnect('poc')

pop_dat <- sqlQuery(con, 
              'SELECT measurement_year
                  , county_desc AS county
                  , pop_cnt
               FROM dbo.ref_lookup_ofm_total_population AS p
               LEFT JOIN dbo.ref_lookup_county AS c
               ON p.fips = c.countyfips
               WHERE pk_gndr = 0
                  AND cd_race = 0
                  AND measurement_year = 2016
                  AND fips != 53'
              )

# joining data together

county_dat <- left_join(wa_counties, pop_dat)

# getting colors

ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

# map_d$fill_col <- 
  
  
  as.character(cut(county_dat$pop_cnt, seq(0,1000000,100000), include.lowest=TRUE, labels=ramp(100000)))


map_d$fill_col <- ifelse(is.na(map_d$fill_col), "#FFFFFF", map_d$fill_col)

# Centering county labels
# 
# county_centers <- wa_counties %>%
#   gCentroid(byid = TRUE, id = 'group') %>%
#   data.frame %>%
#   cbind(name=maine$name %>% gsub(" County, ME", "", .) )

# Getting tooltip data

get_state_dat <- function(x){
  state_dat <- filter(wa_counties, subregion == x$subregion) %>%
    select(subregion) %>%
    distinct() %>%
    mutate(subregion = Hmisc::capitalize(subregion))
  paste(state_dat, "<br />")
} 

get_state_dat(wa_counties)

# creating  map

county_dat %>%
  ggvis(~long, ~lat) %>%
  group_by(group, subregion) %>%
  layer_paths(fill = ~pop_cnt
              , strokeOpacity := 0.5
              , strokeWidth := 0.25
              , stroke:="#7f7f7f"
              ) %>%
  hide_legend('fill') %>%
  hide_axis('x') %>% 
  hide_axis('y') %>%
  set_options(width = 400, height = 600, keep_aspect = TRUE) #%>% # might need to adjust this
  # add_tooltip(vis = get_state_dat , on = 'hover')




get_state_dat <- function(x){
  row <- wa_counties[wa_counties$group==x$group,]  %>%
    select(subregion)  %>% 
    distinct() %>%
    mutate(subregion = Hmisc::capitalize(subregion))
  # paste0("<b>", row[,"subregion"], "</b>")
  paste0(names(x), ": ", format(x), collapse = "<br />")
  }   

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

all_values(mtcars)

base <- mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points()
base %>% add_tooltip(all_values, "hover")
base %>% add_tooltip(all_values, "click")

get_state_dat <- function(x) {
  if(is.null(x)) {
    return(NULL)
  } 
  # unique_data <- 
  paste0('County: ', Hmisc::capitalize(unique(x$subregion)), collapse = "<br />")
}

get_state_dat(wa_counties)











