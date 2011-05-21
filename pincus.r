library('foreach')
library('itertools')
library('Brobdingnag')


# The actual formula to compute the function, fn's, optimum
pincus_formula <- function(x, fn, lambda = 10) {
	colSums(exp(lambda * x * fn(x)) / exp(lambda * fn(x)))
}

# ...: parameter grids
# fn: function to optimize
# lambda: constant that should be large to use the Pincus theorem
# chunk_size: Size of subset of Cartesian product of grid.
pincus <- function(..., fn, lambda, chunk_size = 10) {
	iter_chunk <- ichunk(product(...), chunkSize = chunk_size)
	out <- foreach(chunk = iter_chunk)  %dopar% {
		grid <- do.call(rbind, lapply(chunk, function(x) unlist(x)))
		list(
			pincus = pincus_formula(grid, fn = fn, lambda = lambda),
			count = nrow(grid)
		)
	}
	col_sums <- do.call(rbind, lapply(out, function(x) x$pincus))
	count <- do.call(rbind, lapply(out, function(x) x$count))
	colSums(col_sums) / sum(count)
}

f <- function(x) {
	sin(sqrt(x[,1]^2 + x[,2]^2)) / sqrt(x[,1]^2 + x[,2]^2) - sqrt(x[,1]^2 + x[,2]^2)
}
x <- y <- seq(-5, 5, length = 100)
pincus_out <- pincus(x, y, fn = f, lambda = 10, chunk_size = 1000)

print(pincus_out)
