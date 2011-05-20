library('foreach')
library('itertools')
library('Brobdingnag')

grid_size <- 10
chunk_size <- 5
beta1 <- beta2 <- beta3 <- beta4 <- seq(0, 1, length = grid_size)

pincus_formula <- function(x, fn, lambda = 10) {
	colSums(exp(lambda * x * fn(x)) / exp(lambda * fn(x)))
}

pincus <- function(..., fn, lambda, chunk_size = 10) {
	iter_chunk <- ichunk(product(...), chunkSize = chunk_size)
	pincus_sums <- foreach(chunk = iter_chunk, .combine=rbind) %do% {
		x <- do.call(rbind, lapply(chunk, function(x) unlist(x)))
		pincus_formula(x, fn = fn, lambda = lambda)
	}
	# TODO: Divide by the total number of combinations
	colSums(pincus_sums) / 10000
}

#(pincus_out <- pincus(beta1, beta2, beta3, chunk_size = chunk_size))
#(pincus_out <- pincus(beta1, beta2, beta3, beta4, chunk_size = 10000))


f <- function(x) {
	sin(sqrt(x[,1]^2 + x[,2]^2)) / sqrt(x[,1]^2 + x[,2]^2) - sqrt(x[,1]^2 + x[,2]^2)
}
x <- y <- seq(-5, 5, length = 100)
pincus.out <- pincus(x, y, fn = f, lambda = 10, chunk_size = 1000)

print(pincus.out)
