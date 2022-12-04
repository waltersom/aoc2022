library(dplyr)

input_data <- read.table("day4/input.txt", sep=",")

input_data |> rowwise() |> 
	mutate(across(.cols = c("V1", "V2"),.fns = \(x){v <- strsplit(x, "-")
	if(length(v[[1]]) == 1){
		return(data.frame(x=as.integer(v[[1]][1]), y = as.integer(v[[1]][1])))
		}
	data.frame(x = as.integer(v[[1]][1]), y = as.integer(v[[1]][2])) #get cols for start, end of each range
	}
	)) |> 
	ungroup() |> 
	filter((V1$x <= V2$x & V1$y >= V2$y) | (V1$x >= V2$x & V1$y <= V2$y)) |> #not-very-pretty check of full overlap
	summarize(n())

input_data |> rowwise() |> 
	mutate(across(.cols = c("V1", "V2"),.fns = \(x){v <- strsplit(x, "-")
	if(length(v[[1]]) == 1){
		return(data.frame(x=as.integer(v[[1]][1]), y = as.integer(v[[1]][1])))
	}
	data.frame(x = as.integer(v[[1]][1]), y = as.integer(v[[1]][2]))
	}
	)) |> 
	ungroup() |> 
	filter((V1$x >= V2$x & V1$x<= V2$y) | (V1$y >= V2$x & V1$y <= V2$y) | (V2$x >= V1$x & V2$x<= V1$y) | (V2$y >= V1$x & V2$y <= V1$y)) |> #Not very pretty check of partial overlap
	summarize(n())

		

input_data |> rowwise() |> 
	mutate(across(.cols = c("V1", "V2"),.fns = \(x){v <- strsplit(x, "-")
	if(length(v[[1]]) == 1){
		return(data.frame(x=as.integer(v[[1]][1]), y = as.integer(v[[1]][1])))
	}
	data.frame(x = as.integer(v[[1]][1]), y = as.integer(v[[1]][2])) #get cols for start, end of each range
	}
	)) |> 
	filter(all(seq(V1$x, V1$y) %in% seq(V2$x, V2$y)) | all(seq(V2$x, V2$y) %in% seq(V1$x, V1$y))) |> #slightly cleaner check of full overlap
	ungroup() |> 
	summarize(n())		 

input_data |> rowwise() |> 
	mutate(across(.cols = c("V1", "V2"),.fns = \(x){v <- strsplit(x, "-")
	if(length(v[[1]]) == 1){
		return(data.frame(x=as.integer(v[[1]][1]), y = as.integer(v[[1]][1])))
	}
	data.frame(x = as.integer(v[[1]][1]), y = as.integer(v[[1]][2])) #get cols for start, end of each range
	}
	)) |> 
	filter(any(seq(V1$x, V1$y) %in% seq(V2$x, V2$y)) | any(seq(V2$x, V2$y) %in% seq(V1$x, V1$y))) |> #cleaner check of full overlap
	ungroup() |> 
	summarize(n())	
