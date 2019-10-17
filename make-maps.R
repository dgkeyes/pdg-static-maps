
# Package -----------------------------------------------------------------

library(tidyverse)
library(googlesheets)
library(readxl)
library(janitor)
library(tigris)
library(hrbrthemes)
library(sf)


# Get Data ----------------------------------------------------------------

# gs_title("PDG data for static tables") %>%
#   gs_download(to = "data/data.xlsx",
#               overwrite = TRUE)

sheet_path <- "data/data.xlsx"

dk_get_data <- function(sheet_name) {
  read_excel(sheet_path,
             sheet = sheet_name,
             na = c("*", "-", "--")) %>%
    clean_names() %>%
    mutate(n = as.numeric(n)) %>%
    mutate(percent = as.numeric(percent))
}

sheet_names <- excel_sheets(sheet_path) %>%
  as_tibble() %>%
  set_names("indicator") %>%
  mutate(number = row_number()) %>%
  filter(indicator != "Child Abuse and Maltreatment")

data <- map_df(sheet_names$indicator,
               dk_get_data,
               .id = "sheet") %>%
  mutate(sheet = as.numeric(sheet)) %>%
  left_join(sheet_names, by = c("sheet" = "number")) %>%
  select(indicator, everything()) %>%
  select(-sheet)


# Google Sheets 4 Version ---------------------------------------------------

library(googlesheets4)

google_sheet_path <- "https://docs.google.com/spreadsheets/d/1OUG2tNy9cfC5I_kvM6dkM16v5h2E8GfdhiGl1lASog4/edit#gid=1402645496"

google_sheet_names <- google_sheet_path %>%
  sheets_get()

google_sheet_names <- google_sheet_names$sheets %>%
  select(name) %>%
  set_names("indicator") %>%
  mutate(number = row_number()) %>%
  filter(indicator != "Child Abuse and Maltreatment")

dk_get_data_google <- function(sheet_name) {
  read_sheet(google_sheet_path,
             sheet = sheet_name,
             na = c("*", "-")) %>%
    clean_names()
}


# Google Sheets Version ---------------------------------------------------

library(googlesheets)

google_sheet <- gs_title("PDG data for static tables")

google_sheet_names <- google_sheet %>% 
  gs_ws_ls() %>% 
  tibble() %>% 
  set_names("indicator") %>%
  mutate(number = row_number()) %>%
  filter(indicator != "Child Abuse and Maltreatment")

dk_get_data_google <- function(sheet_name) {
  gs_read(google_sheet,
             sheet = sheet_name,
             na = c("*", "-")) %>%
    clean_names()
}


google_data <- map_df(google_sheet_names$indicator,
               dk_get_data_google,
               .id = "sheet") %>%
  mutate(sheet = as.numeric(sheet)) %>%
  left_join(sheet_names, by = c("sheet" = "number")) %>%
  select(indicator, everything()) %>%
  select(-sheet)



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


dk_make_map("Low Wage")


# Save Map Function ----------------------------------------------------------------

dk_save_map <- function(indicator_to_use) {
  
  file_name <- str_to_lower(indicator_to_use) %>% 
    str_replace_all(" ", "-")
  
  ggsave(paste0("maps/", file_name, ".pdf"),
         device = cairo_pdf,
         width = 6,
         height = 4,
         units = "in")
}

# dk_save_map("High Wage")

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



