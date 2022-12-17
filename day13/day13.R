library(dplyr)
library(reticulate)

input <- readLines("day13/input.txt")


parsed_input <- lapply(input, \(x){if(x=="")return(NULL);reticulate::py_eval(x)})
parsed_input <- parsed_input[!sapply(parsed_input, is.null)]

#This takes two values, converts to list if necessary, and compares
compare <- function(l, r){
	compare_lists(l, r)
}

compare_integers <- function(l, r){
	if(l < r) return(TRUE)
	if(l > r) return(FALSE)
	return(as.logical(NA_integer_))
}

compare_lists <- function(l, r){
	ll <- length(l)
	lr <- length(r)
	lm <- min(ll, lr)
	if(ll == 0 & lr > 0) return(TRUE)
	if(lr == 0 & ll > 0) return(FALSE)
	if(ll == 0 & lr == 0) return(as.logical(NA_integer_))
	for(i in seq.int(lm)){
		# browser()
		if(length(l[[i]]) == 1 && length(r[[i]]) == 1 & !is.list(l[[i]]) & !is.list(r[[i]])){ #Compare integers
			res <- compare_integers(l[[i]], r[[i]])
		}else{
			res <- compare_lists(l[[i]], r[[i]])
		}
		
		if(!is.na(res) & !res) return(FALSE) #They're in the wrong order
		if(!is.na(res) & res) return(TRUE) #They're in the wrong order
	}
	if(ll < lr){
		return(TRUE)
	}
	if(ll > lr){
		return(FALSE)
	}
	return(as.logical(NA_integer_))
}


number_lines <- length(parsed_input)
number_pairs <- number_lines / 2

check_pair <- function(i, input){
	compare(input[[2 * i - 1]], input[[2 * i]])
}

which(sapply(seq(number_pairs), check_pair, input = parsed_input)) |> sum()

#Custom sort code
#(adapted from https://stackoverflow.com/a/70138588/5528982)

`[.packets` <- function(x, i, ...) structure(unclass(x)[i], class = "packets")
`==.packets` <- function(a, b) {is.na(compare(a, b))}
`>.packets` <- function(a, b) {
	compare(a,b) == FALSE
}

class(parsed_input) = "packets"
parsed_input <- c(parsed_input, list(2, 6))
sorted <- sort(parsed_input)
which(sorted %in% c(2,6)) |> prod()
