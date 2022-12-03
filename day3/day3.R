library(dplyr)

backpacks <- readLines("day3/input.txt")

get_backpacks <- function(string){
	backpack_strings <- lapply(string, \(x){l <- nchar(x); l1 <- floor(l/2); c(first = substr(x, 1, l1), second = substr(x, l1+1, l))})
	backpack_lists <- lapply(backpack_strings, strsplit, split = "")
	backpack_sets <- lapply(backpack_lists, 
													\(x){
														list(fo = setdiff(x[["first"]], x[["second"]]),
																 so = setdiff(x[["second"]], x[["first"]]),
																 both = intersect(x[["second"]], x[["first"]]))
													}
	)
}

get_priority_from_set <- function(x){
	sum(c(match(x, letters, nomatch=0), match(x, LETTERS, nomatch = -26) + 26))
}

backpack_sets <- get_backpacks(backpacks)
backpack_priority <- lapply(backpack_sets, \(x){get_priority_from_set(x[["both"]])})

sprintf("The priority of all backpacks is %d", sum(unlist(backpack_priority)))


backpacks |> as_tibble() |> 
	mutate(group = (row_number() + 2) %/% 3, list = strsplit(value, "")) |> 
	group_by(group) |> 
	summarize(common = Reduce(intersect, list)) |> 
	ungroup() |> 
	rowwise() |> 
	mutate(priority = get_priority_from_set(common)) |> 
	ungroup() |> 
	summarize(sum(priority))
	
