# Purpose: Explore the data provided for plotting in Tableau
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_03_19
# Audience: USAID Indonesia Program Office


# Setting up --------------------------------------------------------------

# What have you loaded?
(.packages())
sessionInfo()

# Load the data provided
dir(datapath)

geo2 <- st_read(file.path(admin2, "BPS_Admin2Boundary_2013.shp"))
geo1 <- st_read(file.path(admin1, "BPS_Admin1Boundary_2013.shp"))

# What do we all have in the data provided to us? Let's make it into a listed dataframe
excel_sheets(file.path(datapath, "USAID Indonesia Investment Mapping.xlsx"))
investpath <- file.path(datapath, "USAID Indonesia Investment Mapping.xlsx")

ind_invest <-
  investpath %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = investpath)


# Coded investments -------------------------------------------------------

# Focus on the 1 data set to be used, let's review the structure and data types
invest <- ind_invest$`Location Coded`
invest %>% str()
invest %>% distinct(ID) %>% tally() # 815 unique IDs, so that's a good start

# List of columns to be looped over to check frequencies
summary_list <- list("IM", "Office", "Sector", "Type", "Granularity", "Region", "District")

# Unquote the symbol (string) and pass to the function to loop over key vars in which you are interested.
map(summary_list, ~ group_check(invest, !!sym(.)))

# Clean up discrepancies
invest <-
  invest %>%
  mutate(District = ifelse(District != "`",
    District,
    NA_character_
  ))




# Now seems to be consistend across KABKOT_ID and District names
# Count is equivalent to group_by() + tally()

dist <-
  invest %>%
  filter(!is.na(District)) %>%
  count(Province, District, KABKOT_ID)


# Create a crosswalk with the Kabkot codes, name, and province; Remove the geometry
# 502 Unique District
admin2_cw <- strip_geom(geo2, OBJECTID, KABKOT, KABKOT_ID, PROVINSI)
admin1_cw <- strip_geom(geo1, OBJECTID, PROVINSI, Region)


# Compare Province and District Names / Numbers ---------------------------
# Two tasks to do: 1)Compare number and names of Provinces in each dataset
# 2) Compare districts and how many potentially should match (326 per above)

prov_sf <-
  admin2_cw %>%
  # Use count to skip the group_by step
  count(PROVINSI)

prov_df <-
  invest %>%
  filter(Province != "NATIONWIDE") %>%
  count(Province)

# Compare the two dataframes - East and North Province issue resolved with new shapefile
# There are no differences between the KABKOT IDs when joining, so things should be good to go
compare_vars(prov_df$Province, prov_sf$PROVINSI)
compare_vars(dist$KABKOT_ID, admin2_cw$KABKOT_ID)


# How many Districts merge to the Admin2 shapefile data?
dist_join <-
  dist %>%
  left_join(x = ., y = admin2_cw, by = c("KABKOT_ID"))




# Investigate and reshape loaded data  ------------------------------------
# Dates have been resolved by the Mission -- even POSIXct
# Reshape the data based on Fiscal year dates
invest_long <- 
  invest %>% 
  gather(starts_with("FY"), 
         key = Fiscal_year, 
         value = "amount") %>% 
  filter(amount != 0) # filter out all rows that contain no information

  
# Mission asked for 3 data sets, Nation-wide, Provincal and District
unique(invest$Granularity)



# Checking sums -----------------------------------------------------------

# Check that total estimated costs add up ---------------------------------
invest_long <- 
  invest_long %>% 
  group_by(IM) %>% 
  
  # Create a TEC variable to check the math from Excel
  mutate(total_amt = sum(amount, na.rm = "TRUE")) %>% 
  ungroup() %>% 
  
  # Create a tolerance range that marks if the new TEC is different from the old
  mutate(TEC_diff = ifelse(near(TEC, total_amt, tol = 2), 1, 0)) %>% 
  
  # Create dummy variables to filter the level of geography,
  mutate(prov = ifelse(Granularity == "Provincial", 1, 0),
         dist = ifelse(Granularity == "District", 1, 0), 
         national = ifelse(Granularity == "Nationwide", 1, 0)) %>% 
  
  select(IM, amount, TEC, total_amt, TEC_diff, everything()) %>% 
  arrange(IM, Fiscal_year) 


invest_long %>% 
  select(IM, TEC_diff, TEC, total_amt) %>% 
  filter(TEC_diff == 0) %>% 
  group_by(IM) %>% 
  summarise(
    TEC = mean(TEC), 
    FY_amount = mean(total_amt, na.rm = TRUE),
    diff = TEC - FY_amount
  ) %>% 
  arrange(desc(diff)) %>% 
  knitr::kable() 
