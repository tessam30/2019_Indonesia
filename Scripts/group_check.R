# Group check
# Check how many unique groups are in a variable, count occurences

group_check <- function(df, x) {
  xvar <- enquo(x)
  df %>% 
    group_by(!!xvar) %>%
    tally() %>% 
    arrange(desc(n)) %>% 
    print(n = Inf)
}