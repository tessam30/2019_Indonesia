# Basic filter function

# Filter function based on string input
fltr_func <- function(df, fltr_exp) {
  
  filter_exp_enq <- enquo(fltr_exp)
  
  df %>% filter(!!filter_exp_enq)
}

#sample fltr_func(df_long, Granularity == "District") 