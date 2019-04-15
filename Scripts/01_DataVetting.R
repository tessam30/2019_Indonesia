# Purpose: Explore the data provided for plotting in Tableau
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_03_19
# Audience: USAID Indonesia Program Office


# Setting up --------------------------------------------------------------

# What have you loaded?
(.packages())
sessionInfo()
options(stringsAsFactors = FALSE)

# Load the data provided
dir(datapath)

geo2 <- st_read(file.path(admin2, "BPS_Admin2Boundary_2013.shp"))
geo1 <- st_read(file.path(admin1, "BPS_Admin1Boundary_2013.shp"))

# What do we all have in the data provided to us? Let's make it into a listed dataframe
excel_sheets(file.path(datapath, "USAID Indonesia Investment Mapping_2019412.xlsx"))
investpath <- file.path(datapath, "USAID Indonesia Investment Mapping_2019412.xlsx")

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
admin1_cw <- strip_geom(geo1, OBJECTID, PROVINSI, Region, prop_code)


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

tmp <- full_join(dist, admin2_cw, by = c("KABKOT_ID"))

# How many Districts merge to the Admin2 shapefile data?
dist_join <-
  dist %>%
  left_join(x = ., y = admin2_cw, by = c("KABKOT_ID"))

# Use below to get the case_when statements you need to clean up investment data
tmp <- admin2_cw %>% 
  left_join(dist, by = c("KABKOT" = "District")) %>% 
  mutate(id_check = ifelse(KABKOT_ID.x == KABKOT_ID.y, 0, 1)) %>% 
  arrange(desc(id_check)) %>% 
  select(contains("KABKOT"), everything())


# What happens if we join to the District level shapefile?
geo2_invest <- 
  geo2 %>% 
  left_join(x = ., y = invest_long, by = c("KABKOT_ID"))

st_write(geo2_invest, file.path("Data", "IND_admin2_investments.shp"), delete_dsn=TRUE)


# Investigate and reshape loaded data  ------------------------------------
# Dates have been resolved by the Mission -- even POSIXct
# Found a problem w/ the PROV ID and KABKOT IDs not matching, even PROV IDs being wrong
# Need to join the investment data to the province shapefile info and double check PROV IDS
# Then, repeat the process w/ the first two digits from the KABKOT IDs to see
# what districts got messed up?

invest_prov <- 
  invest %>% 
  left_join(., admin1_cw, by = c("Province" = "PROVINSI")) %>% 
  mutate(prov_id_flag = ifelse(prop_code == PROV_ID, 0, 1),) %>% 
  mutate(prov_ID_fixed = ifelse(prov_id_flag == 1, prop_code, PROV_ID)) %>% # Fix the problemmatic Provinces and create a new variable
  select(prov_id_flag, Province, prov_ID_fixed, PROV_ID, prop_code, Region.y, Region.x, everything()) %>% 
  arrange(desc(prov_id_flag)) %>% 
  
  # Next step is to interrogate the district data using the first 
  # two digits from the KABKOT_ID to see if they align to the PROV_ID_FIXED
  mutate(District = case_when(
    District == "KOTA GORONTALO"
  ))


%>% 
  mutate(kabkot_id_check = substr(KABKOT_ID, 1, 2) %>% as.numeric(), 
         prov_dist_flag = ifelse(kabkot_id_check == prov_ID_fixed, 0, 1)) %>% 
  select(District, KABKOT_ID, kabkot_id_check, prov_dist_flag, everything()) %>% 
  arrange(desc(prov_dist_flag))









# Reshape the data based on Fiscal year dates
invest_long <- 
  invest %>% 
  gather(starts_with("FY"), 
         key = Fiscal_year, 
         value = "amount") %>% 
  filter(amount != 0) %>% # filter out all rows that contain no information 
  mutate(prov_first2 = substr(KABKOT_ID, 1, 2) %>% as.numeric,
         prov_flag = ifelse(prov_first2 == PROV_ID, 0, 1))

  
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
  mutate(check_totals = TEC - total_amt) %>% 
  
  # Create a tolerance range that marks if the new TEC is different from the old
  mutate(TEC_diff = ifelse(near(TEC, total_amt, tol = 2), 1, 0)) %>% 
  
  # Create dummy variables to filter the level of geography,
  mutate(prov = ifelse(Granularity == "Provincial", 1, 0),
         dist = ifelse(Granularity == "District", 1, 0), 
         national = ifelse(Granularity == "Nationwide", 1, 0)) %>% 
  
  select(IM, amount, TEC, total_amt, TEC_diff, everything()) %>% 
  arrange(IM, Fiscal_year) %>% 
  select(IM, Office, Sector, TEC, total_amt, check_totals, 
         amount, Province, District, Region, 
         KABKOT_ID, PROV_ID, Granularity, everything()) 


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
  #knitr::kable() %>% 
  mutate(IM = fct_reorder(IM, diff)) %>% 
  ggplot(aes(x = IM, y = diff)) + geom_col() +
  coord_flip()



# Levels of granularity for datasets

IND_investments_dist <- fltr_func(invest_long, Granularity == "District") 
IND_investments_prov <- fltr_func(invest_long, Granularity == "Provincial")
IND_investments_natl <- fltr_func(invest_long, Granularity == "Nationwide")


datalist = list(IND_investments_dist = IND_investments_dist, 
                IND_investments_prov = IND_investments_prov,
                IND_investments_natl = IND_investments_natl, 
                IND_investments_all = invest_long)

# write the files to the data folder using the list names 
datalist %>%  
  names() %>% 
  map(., 
      ~ ex(datalist[[.]], 
                  file.path(datapath, str_c(., ".csv"))))

# Make a markdown table
invest_long %>% group_by(Province, District) %>% summarise(sum = sum(amount, na.rm = TRUE)) %>% arrange(Province, desc(sum), District) %>% print(n=Inf) %>% knitr::kable(format.args = list(big.mark = ","), digits = 0)


