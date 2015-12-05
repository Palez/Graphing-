
run <- function(benchmark, output="log.txt", version="rjit", repeats=1){
	source(benchmark)
	result = paste(version, ",", benchmark)
	for(i in 1:repeats){
		result  <- paste(result, "," , round(system.time(execute())[3]*1000))
	}

	write(result, file=output, append=TRUE)
}