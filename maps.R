
# Package -----------------------------------------------------------------

library(tidyverse)
library(googlesheets)
library(readxl)
library(janitor)
library(tigris)
library(hrbrthemes)
library(sf)
library(zip)
library(fs)


# COUNTY MAPS -------------------------------------------------------------


# Get Data ----------------------------------------------------------------

# gs_title("PDG data for static tables") %>%
#   gs_download(to = "data/county-data.xlsx",
#               overwrite = TRUE)

sheet_path <- "data/county-data.xlsx"

dk_get_data <- function(sheet_name) {
  read_excel(sheet_path,
             sheet = sheet_name,
             na = c("*", "-", "--", "NA")) %>%
    clean_names() %>%
    mutate(n = as.numeric(n)) %>%
    mutate(percent = as.numeric(percent))
}

sheet_names <- excel_sheets(sheet_path) %>%
  as_tibble() %>%
  set_names("indicator") %>%
  mutate(number = row_number()) %>%
  mutate(file_name = str_replace_all(indicator, "[[:punct:]]", " ")) %>% 
  mutate(file_name = str_to_lower(file_name)) %>% 
  mutate(file_name = str_squish(file_name)) %>% 
  mutate(file_name = str_replace_all(file_name, " ", "-")) 


data <- map_df(sheet_names$indicator,
               dk_get_data,
               .id = "sheet") %>%
  mutate(sheet = as.numeric(sheet)) %>%
  left_join(sheet_names, by = c("sheet" = "number")) %>%
  select(indicator, everything()) %>%
  select(-c(sheet, x1)) %>% 
  drop_na(indicator)



# Get Geospatial Data -----------------------------------------------------

oregon_map <- counties(state = "OR",
                       cb = TRUE) %>% 
  clean_names()

data_geospatial <- right_join(data, oregon_map, by = c("county" = "name")) %>% 
  select(-(statefp:awater)) %>% 
  mutate(level = case_when(
    level == "L" ~ "Low",
    level == "LM" ~ "Low Medium",
    level == "HM" ~ "Medium High",
    level == "H" ~ "High"
    # is.na(level) ~ "Data Not Available"
  )) %>% 
  mutate(level = factor(level, levels = c("Low", 
                                          "Low Medium", 
                                          "Medium High", 
                                          "High", 
                                          "Data Not Available")))

# Make Map Function ----------------------------------------------------------------

theme_set(theme_ipsum(grid = FALSE,
                      plot_margin = margin(0, 0, 0, 0)))

fill_colors <- c("#47666A",
                 "#50928D",
                 "#68B8B1",
                 "#A3CFCC")

dk_make_map <- function(indicator_to_use) {
  
  data_geospatial %>% 
    filter(indicator == indicator_to_use) %>% 
    st_as_sf() %>% 
    ggplot(aes(fill = level)) +
    geom_sf(color = "white",
            size = 0.75) +
    scale_fill_manual(values = rev(fill_colors),
                      na.value = "#D0D6D9") +
    labs(fill = NULL) +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
}


dk_make_map("Children under 6 in poverty")


# Save Map Function ----------------------------------------------------------------

dk_save_map <- function(indicator_to_use) {
  
  file_name <- sheet_names %>% 
    filter(indicator == indicator_to_use) %>% 
    pull(file_name)
  
  ggsave(paste0("maps/", file_name, ".pdf"),
         device = cairo_pdf,
         width = 6,
         height = 4,
         units = "in")
}

dk_save_map("High Wage")

# Make and Save Map Function ----------------------------------------------------------------


dk_make_and_save_map <- function(indicator_to_use) {
  
  dk_make_map(indicator_to_use)
  
  dk_save_map(indicator_to_use)
  
}


# dk_make_and_save_map("High Wage")

# Make Maps ---------------------------------------------------------------

indicators <- data_geospatial %>% 
  distinct(indicator) %>% 
  pull(indicator)

walk(indicators, dk_make_and_save_map)



# HUB MAPS ----------------------------------------------------------------


# Get Data ----------------------------------------------------------------

# gs_title("PDG data for static maps - Early Learning Hubs") %>%
#   gs_download(to = "data/hub-data.xlsx",
#               overwrite = TRUE)

hub_data <- read_excel("data/hub-data.xlsx",
                       na = "-") %>% 
  clean_names() 




# Get Geospatial Data -----------------------------------------------------

early_learning_hubs <- tribble(
  ~hub_name, ~address, ~region, ~website,
  "Blue Mountain", "2001 SW Nye Ave Pendleton, OR 97801", "Morrow, Umatilla, Union", "https://bluemountainearlylearninghub.org/",
  "Clackamas", "150 Beavercreek Road, Oregon City, OR 97045", "Clackamas", "https://earlylearninghubofclackamascounty.org/",
  "Central Oregon", "2804 SW 6th St Redmond OR 97756", "Crook, Deschutes, Jefferson", "https://earlylearninghubco.org",
  "Linn-Benton-Lincoln", "6500 Pacific Blvd. SW Albany, OR  97321", "Benton, Lincoln, Linn",  "https://lblearlylearninghub.org",
  "Multnomah", "619 SW 11th Ave. Portland, OR 97205", "Multnomah", "http://www.earlylearningmultnomah.org",
  "Washington", "20665 SW Blanton Street Aloha, OR 97078", "Washington", "https://www.co.washington.or.us/HHS/ChildrenYouthFamilies/",
  "Eastern Oregon", "363 A St W, Vale, OR 97918", "Baker, Malheur, Wallowa", "https://www.malesd.k12.or.us/eastern-oregon-hub",
  "Four Rivers", "400 East Scenic Drive, The Dalles, OR 97058", "Gilliam, Hood River, Sherman, Wasco, Wheeler", "https://4relh.org/",
  "Frontier", "25 Fairview Hts, Burns OR 97720", "Harney, Grant", "https://harneyesd.sharpschool.com/e_c_c/harney_grant_frontier_hub",
  "Lane", "3171 Gateway Loop, Springfield, OR 97477", "Lane", "https://earlylearningalliance.org",
  "Marion-Polk", "2995 Ryan Drive SE, Salem, Oregon 97301", "Marion, Polk", "https://parentinghub.org",
  "Northwest Regional", "5825 NE Ray Circle Hillsboro, Oregon", "Clatsop, Columbia, Tillamook", "http://nwelhub.org/",
  "South Coast", "1855 Thomas Avenue Coos Bay, OR 97420", "Coos, Curry", "https://www.screlhub.com/",
  "South Central", "1409 NE Diamond Lake Blvd, Roseburg, OR 97470", "Douglas, Klamath, Lake", "https://douglasesd.k12.or.us/early-learning-hub/home",
  "Southern Oregon", "101 North Grape Street, Medford OR 97501", "Jackson, Josephine", "https://www3.soesd.k12.or.us/southernoregonlearninghub/",
  "Yamhill", "807 NE Third Street McMinnville, OR 97128", "Yamhill", "https://yamhillcco.org/early-learning-hub/"
)

oregon_counties <- counties(state = "Oregon",
                            cb = TRUE) %>%
  clean_names()

early_learning_hubs_regions <- early_learning_hubs %>% 
  separate_rows(region, sep = ",") %>% 
  mutate(region = str_trim(region)) %>% 
  right_join(oregon_counties, by = c("region" = "name")) %>% 
  group_by(hub_name) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  drop_na(hub_name) %>% 
  full_join(hub_data, by = c("hub_name" = "early_learning_hub")) %>% 
  mutate(level = case_when(
    level == "L" ~ "Low",
    level == "LM" ~ "Low Medium",
    level == "HM" ~ "Medium High",
    level == "H" ~ "High"
    # is.na(level) ~ "Data Not Available"
  )) %>% 
  mutate(level = factor(level, levels = c("Low", 
                                          "Low Medium", 
                                          "Medium High", 
                                          "High", 
                                          "Data Not Available")))

# Make Map Function ----------------------------------------------------------------

theme_set(theme_ipsum(grid = FALSE,
                      plot_margin = margin(0, 0, 0, 0)))

fill_colors <- c("#47666A",
                 "#50928D",
                 "#68B8B1",
                 "#A3CFCC")


early_learning_hubs_regions %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = level)) +
  geom_sf(color = "white",
          size = 0.75) +
  scale_fill_manual(values = rev(fill_colors),
                    na.value = "#D0D6D9") +
  labs(fill = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())





# Save Map ----------------------------------------------------------------

ggsave(paste0("maps/early-learning-hubs.pdf"),
       device = cairo_pdf,
       width = 6,
       height = 4,
       units = "in")




# ZIP ALL MAPS ------------------------------------------------------------

all_maps <- dir_ls("maps")

zipr(zipfile = "maps/maps.zip",
     files = all_maps)

