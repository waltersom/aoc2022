library(dplyr)

input_data <- readLines("day6/input.txt")

#Find first occurence of 4 unique values

is_unique <- function(x, start, n = 4){
	 sub <- lapply(start, \(start, x, n){unlist(strsplit(substr(x, start, start+n-1), ""))}, x = x, n)
	 lapply(sub, \(x){length(unique(x)) == length(x)})
}

check_all <- is_unique(input_data, seq(nchar(input_data) - 4))	
match(TRUE, check_all) + 3

check_all_longer <- is_unique(input_data, seq(nchar(input_data) - 14), n=14)	
match(TRUE, check_all_longer) + 13
