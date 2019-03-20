# Selects columns of spatial data frame, removes geometry, converts it to a tbl_df
# inputs required are a dataframe and list of columns

strip_geom <- function(df, ...) {
  
  if(class(df)[1] != "sf") {
    stop(str_c("Please pass a spatial dataframe (class = sf). You passed a ", class(df)[1]))
  }
  
  selecting <- quos(...)
  
  df_nogeom <- df %>% 
    select(!!!selecting) %>% 
    st_set_geometry(NULL)
  
  #Show that operation worked as desired
  print(str(df_nogeom))
  return(df_nogeom)
}

# Example below, removing geometry and subsetting columns
#tmp <- strip_geom(admin2_cw, OBJECTID, PROVINSI, Region)