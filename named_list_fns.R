parse_sheet_names <- function(...){
  l_names <- as.list(substitute(list(...)))[-1L]
  l <- list(...)
  l <- setNames(l, l_names)
  return(l)
}

l <- parse_sheet_names(mtcars, iris)
names(l)

library(TidyDensity)

names(parse_sheet_names(tidy_normal()))
