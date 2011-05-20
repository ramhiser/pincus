library('foreach')
library('itertools')

grid_size <- 10
chunk_size <- 5
beta1 <- beta2 <- beta3 <- beta4 <- seq(0, 1, length = grid_size)

# TODO: Change pincus_integral() to actual formula.
pincus_integral <- function(x) {
	x
}

pincus <- function(..., chunk_size = 10) {
	iter_chunk <- ichunk(product(...), chunkSize = chunk_size)
	out <- foreach(chunk = iter_chunk, .combine=rbind) %do% {
		x <- do.call(rbind, lapply(chunk, function(x) x))	
		pincus_integral(x)
	}
	out
}

(pincus_out <- pincus(beta1, beta2, beta3, chunk_size = chunk_size))
#(pincus_out <- pincus(beta1, beta2, beta3, beta4, chunk_size = 10000))
