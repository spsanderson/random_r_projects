val <- function(x, x_tilde, m) {
  sum_val <- 0.0
  for (i in 1:length(x)) {
    diff <- x[[i]] - x_tilde[[i]]
    sum_val <- sum_val + 0.5 * m[i] * sum(diff * diff)
  }
  return(sum_val)
}

grad <- function(x, x_tilde, m) {
  g <- matrix(0.0, nrow=length(x), ncol=2)
  for (i in 1:length(x)) {
    g[i, ] <- m[i] * (x[[i]] - x_tilde[[i]])
  }
  return(g)
}

hess <- function(x, m) {
  IJV <- list(i = integer(length(x) * 2), 
              j = integer(length(x) * 2), 
              v = numeric(length(x) * 2))
  for (i in 1:length(x)) {
    for (d in 0:1) {
      IJV$i[i * 2 - 1 + d] <- i * 2 - 1 + d
      IJV$j[i * 2 - 1 + d] <- i * 2 - 1 + d
      IJV$v[i * 2 - 1 + d] <- m[i]
    }
  }
  return(IJV)
}

n <- 100L

x <- RandomWalker::random_normal_walk(.num_walks = 1, .n = n, .samp = FALSE)$y
y <- RandomWalker::random_normal_walk(.num_walks = 1, .n = n, .samp = FALSE, .sd = 0.05)$y
z <- RandomWalker::random_normal_walk(.num_walks = 1, .n = n, .samp = FALSE, .sd = .1)$y

val(x, y, z)

max_val <- max(x, y, z)
min_val <- min(x, y, z)
max_hist_val <- max(hist(x)$counts, hist(y)$counts, hist(z)$counts)
cs_max_val <- max(cumsum(x), cumsum(y), cumsum(z))
cs_min_val <- min(cumsum(x), cumsum(y), cumsum(z))

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END
red_col <- my_col <- t_col('red', 50, 'my_red')
blue_col <- t_col('blue', 50, 'my_blue')
green_col <- t_col('green', 50, 'my_green')

layout(matrix(1:4, ncol=2))
plot(x, type = "l", ylim = c(min_val, max_val), col=green_col)
lines(y, type = "l", col=red_col)
lines(z, type = "l", col=blue_col)

plot(cumsum(x), type = "l", ylim = c(cs_min_val, cs_max_val), col=green_col)
lines(cumsum(y), type = "l", col=red_col)
lines(cumsum(z), type = "l", col=blue_col)

grad(x, y, z) |> plot()

hist(x, col=green_col, xlim=c(min_val, max_val), ylim=c(0, max_hist_val))
hist(y, col=red_col, add=TRUE)
hist(z, col=blue_col, add=TRUE)

layout(matrix(1, nrow=1))
