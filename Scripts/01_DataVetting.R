# Purpose: Explore the data provided for plotting in Tableau
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_03_19
# Audience: USAID Indonesia Program Office 

# What have you loaded?
(.packages())
sessionInfo()
# Load the data provided
dir(datapath)

geo1 <- st_read(file.path(admin2, "BPS_Admin2Boundary_2013.shp"))
geo2 <- st_read(file.path(admin1, "BPS_Admin1Boundary_2013.shp"))

# What do we all have in the data provided to us? Let's make it into a listed dataframe
excel_sheets(file.path(datapath, "USAID Indonesia Investment Mapping.xlsx"))
investpath <- file.path(datapath, "USAID Indonesia Investment Mapping.xlsx")

ind_invest <- 
  investpath %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = investpath)

# Focus on the 1 data set to be used, let's review the structure and data types
invest <- ind_invest$`Location Coded`
invest %>% str()
invest %>% distinct(ID) %>% tally() # 815 unique IDs, so that's a good start

# Create a small function to group each var and return groupings

group_check <- function(df, x) {
  xvar <- enquo(x)
  df %>% 
    group_by(!!xvar) %>%
    tally() %>% 
    arrange(desc(n)) %>% 
    print(n = Inf)
}

summary_list <- list("IM", "Office", "Sector", "Type", "Granularity", "Region")

# Unquote the symbol (string) and pass to the function to loop over key vars in which you are interested.
map(summary_list, ~group_check(invest, !!sym(.)))


