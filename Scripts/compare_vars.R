# Function to compare two dataframe columns, return a count of differences
compare_vars <- function(x, y) {
  
  # Compare each variable, both ways
  xy <- length(setdiff(x, y))
  yx <- length(setdiff(y, x))
  
  if(xy == 0 & yx == 0){
    return("There are no differences between the columns")
  }
  
  print(str_c(xy, " differences between x and y"))
  print(str_c(yx, " differences between y and x"))
}

# Example of call below, x = df$var1 and y = df$var2
# compare_vars(admin2_cw$KABKOT_ID, dist$KABKOT_ID)
