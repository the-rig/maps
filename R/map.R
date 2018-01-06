library(RODBC)
library(pocr)
library(shiny)
library(RColorBrewer)
library(ggvis)
library(ggplot2)
library(dplyr)

# Get map
washington <- map_data("county") %>%
  filter(region == "washington") %>%
  rename(county = subregion)

# Get pop dat
con <- odbcConnect('poc')

pop_dat <- sqlQuery(con, 
                    'SELECT measurement_year
                    , county_desc AS county
                    , pop_cnt
                    , countyfips
                    FROM dbo.ref_lookup_ofm_population AS p
                    LEFT JOIN dbo.ref_lookup_county AS c
                    ON p.fips = c.countyfips
                    WHERE pk_gndr = 0
                    AND age_grouping_cd = 0
                    AND cd_race = 0
                    AND measurement_year = 2016
                    AND fips != 53'
) %>%
  mutate(county = tolower(county))

# Join data together
county_dat <- left_join(washington, pop_dat) %>%
  left_join(select(ref_lookup_county, county_desc, countyfips)) %>%
  select(-county) %>%
  rename(county = county_desc)

# Tooltip function
county_name <- function(x) {
  
  county_filter <- filter(county_dat, county == x$county) %>%
    distinct(county, pop_cnt) 
  
  paste0("<div style = 'font-family:Open Sans'>", 
         format(county_filter$county), "<br />", 
         "Count: ", format(round(county_filter$pop_cnt), big.mark = ","))
}

# Interactive map
county_dat %>%
  group_by(group, county) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill = ~pop_cnt,
              strokeWidth:=0.5, stroke:="white") %>%
  scale_numeric("fill", range=c("#9db6c7", "#3B6E8F")) %>%
  add_tooltip(county_name, "hover") %>%
  hide_legend('fill') %>%
  hide_axis('x') %>% 
  hide_axis('y') %>%
  set_options(width = 500, height = 600, keep_aspect = TRUE) 
