
# get_mtasub <- function(fips){
#   ifelse(fips %in% constants$nycfips, "nyc", "suburbs")
# }

cut_labels <- function(breaks) {
  n <- length(breaks)
  labels <- character(n - 1)
  for (i in 1:(n - 1)) {
    labels[i] <- paste(breaks[i], breaks[i + 1] - 1, sep = "-")
  }
  return(labels)
}

cutlabs_ge <- function(breaks) {
  n <- length(breaks)
  labels <- character(n - 1)
  for (i in 1:(n - 1)) {
    paste(breaks[i], breaks[i + 1], sep = " to < ")
  }
  return(labels)
}

# cutlabs_ge(breaks)
# 
# breaks <- c(0, 1000, 2000)
# i <- 2
# paste(breaks[i], breaks[i + 1], sep = " to < ")

