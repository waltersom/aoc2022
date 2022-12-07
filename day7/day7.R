library(dplyr)
library(stringr)

input_data <- readLines("day7/input.txt")

cd_com_inds <- seq(length(input_data))[str_starts(input_data, "\\$ cd")]

directory_sequence <- str_replace(input_data[cd_com_inds], fixed("$ cd "), "")

go_up <- function(x){
	x[-length(x)]
}

enter <- function(x, d){
	c(x, d)
}
# Get ordered list of directories we are in
# 
dir_seq <- vector(length = length(directory_sequence), mode = "list")
dir_seq[1] <- directory_sequence[1]
for(i in seq(2, length(dir_seq))){
	if(directory_sequence[i] == "/"){
		dir_seq[[i]] = "/"
	}else{
		if(directory_sequence[i] == ".."){
			dir_seq[[i]] <- go_up(dir_seq[[i-1]])
		}else{
			dir_seq[[i]] <- enter(dir_seq[[i-1]], directory_sequence[i])
		}
	}
}

dir_seq #This lets us know where we are
unique_dirs <- unique(dir_seq) #These are all of the directories

#get list of files in each directory, and any sub-directories
#between each two sub-dirs, get each result from ls


for(i in seq(1, length(cd_com_inds))){
	if(i != length(cd_com_inds)){
		end_ind <- cd_com_inds[i+1] - 1
	}else{
		end_ind <- length(input_data)
	}
	current_dir <- dir_seq[[i]]
	current_dir_ind <- match(list(current_dir), unique_dirs)
	if(is.null(attr(unique_dirs[[current_dir_ind]], "visited"))){
		inter_comms <- input_data[seq(cd_com_inds[i] + 1, end_ind)]
		#Remove ls command, and any "dir" entries
		sizes <- str_extract(inter_comms, "^([0-9]+)")
		sizes <- sum(as.integer(sizes[!is.na(sizes)]))
		subdirs <- str_match(inter_comms, "^dir\ ([a-zA_Z]*)")[,2]
		subdirs <- subdirs[!is.na(subdirs)]
		subdir_list <- lapply(subdirs, enter, x = current_dir)
		attr(unique_dirs[[current_dir_ind]], "file_sizes") <- sizes
		attr(unique_dirs[[current_dir_ind]], "subdirs") <- subdir_list
		attr(unique_dirs[[current_dir_ind]], "visited") <- TRUE
	}
}

#Now get sum of all subdirs... start with the long ones, and walk up

unique_dirs_orderd <- unique_dirs[order(unlist(lapply(unique_dirs, length)), decreasing = TRUE)]

for(i in seq(length(unique_dirs_orderd))){
	if(length(attr(unique_dirs_orderd[[i]], "subdirs")) > 0){
		sub_sizes <- sum(unlist(
			lapply(attr(unique_dirs_orderd[[i]], "subdirs"),
						 \(x){attr(unique_dirs_orderd[match(list(x), unique_dirs_orderd)][[1]], "file_sizes")})
			))
		attr(unique_dirs_orderd[[i]], "file_sizes") <- attr(unique_dirs_orderd[[i]], "file_sizes") + sub_sizes
	}
}

sum(unlist(lapply(unique_dirs_orderd, \(x){if(attr(x, "file_sizes") <= 100000){return(attr(x, "file_sizes"))}else{return(0)}})))


total_used <- attr(tail(unique_dirs_orderd, n=1)[[1]], "file_sizes")

total_capacity <- 70000000
needed_capacity <- 30000000

needed_to_free <- total_used - (total_capacity - needed_capacity)

#Find the smalled directory with a size greater than that
sort(unlist(lapply(unique_dirs_orderd, \(x){if(attr(x, "file_sizes") > needed_to_free){return(attr(x, "file_sizes"))}else{return(NA)}})))[1]
