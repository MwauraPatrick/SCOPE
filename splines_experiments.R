
# 
# # Litrature Review
# 
# -   A critical review of what has been done so far in research
# -   To give the reader a critical overview of how the research fits in.
# -   To find out what is missing to the current knowledge.




# 
# 
# 1.  Hook you reader
# 
# -   Short sentences
# -   Clear
# -   Concise
# -   Catchy
# 
# 
# 2.  Background information
# 
# -   Historical or social context
# -   Defining key terms
# -   Introduce relevant theories or research
# 
# 3.  Thesis statement
# 
# -   Sums up the argument in 1-2 sentences
# -   Most important part of the introduction
# -   Defines the focus of the thesis statement
# 
# 4.  Map the thesis structure
# 

x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 5, 4, 6)

rearrange_data <- function(x, y, method) {
  if (method == "periodic") {
    # Ensure y[1] == y[n] for "periodic"
    y[length(y)] <- y[1]
  } else if (method == "hyman") {
    # Ensure y is strictly increasing for "hyman"
    y <- sort(y)  # Sort the y values in increasing order
  }
  return(y)
}

methods <- c("fmm", "periodic", "natural", "monoH.FC", "hyman")
colors <- c("red", "green", "purple", "orange", "brown")


xout <- seq(min(x), max(x), length.out = 2000)

plot(x, y, main = "Spline Interpolation using Different Methods",
     xlab = "X", ylab = "Y", pch = 19, col = "blue",
     xlim = range(x), ylim = range(y))

for (i in seq_along(methods)) {
  y_rearranged <- rearrange_data(x, y, methods[i])

  spline_fn <- splinefun(x, y_rearranged, method = methods[i])

  lines(xout, spline_fn(xout), col = colors[i], lwd = 2)
}

legend("topleft", legend = methods, col = colors, lwd = 2, bty = "n")


# Save 
png("spline_interpolation_methods.png", width = 800, height = 600)

plot(x, y, main = "Spline Interpolation using Different Methods",
     xlab = "X", ylab = "Y", pch = 19, col = "blue",
     xlim = range(x), ylim = range(y))

for (i in seq_along(methods)) {
  y_rearranged <- rearrange_data(x, y, methods[i])
  spline_fn <- splinefun(x, y_rearranged, method = methods[i])
  lines(xout, spline_fn(xout), col = colors[i], lwd = 2)
}

legend("topleft", legend = methods, col = colors, lwd = 2, bty = "n")

dev.off()  



# toy data
x   <- c(1, 2, 3, 4, 5)
y   <- c(2, 3, 5, 4, 6)
xout <- seq(min(x), max(x), length.out = 500)

# rearranger (as you have it)
rearrange_data <- function(x, y, method) {
  if (method == "periodic") {
    y[length(y)] <- y[1]
  } else if (method == "hyman") {
    y <- sort(y)
  }
  y
}

# set up plot
plot(x, y, pch=19, col="black", xlab="X", ylab="Y",
     main="Comparison of Smoothing & Interpolation Methods",
     xlim=range(xout), ylim=range(y)*c(0.9,1.1))

# 1) splinefun methods
methods <- c("fmm","periodic","natural","monoH.FC","hyman")
cols_m  <- c("red","green","purple","orange","brown")
for (i in seq_along(methods)) {
  yr <- rearrange_data(x, y, methods[i])
  fn <- splinefun(x, yr, method = methods[i])
  lines(xout, fn(xout), col=cols_m[i], lwd=2)
}

# 2) smoothing spline
ss <- smooth.spline(x, y, spar=0.5)
lines(xout, predict(ss, xout)$y, lwd=2, lty=2, col="blue")

# 3) loess
lo <- loess(y ~ x, span=0.7)
lines(xout, predict(lo, xout), lwd=2, lty=3, col="darkgreen")

# 4) kernel smoothing
ks <- ksmooth(x, y, kernel="normal", bandwidth=0.5)
lines(ks$x, ks$y,       lwd=2, lty=4, col="magenta")

# 5) linear & step
lin  <- approxfun(x, y, method="linear", rule=2)
step <- approxfun(x, y, method="constant", f=0, rule=2)
lines(xout, lin(xout),  lwd=2, lty=5, col="grey40")
lines(xout, step(xout), lwd=2, lty=6, col="grey70")

# legend
legend("topleft", 
       legend = c(methods,
                  "smoothing spline",
                  "loess(span=0.7)",
                  "ksmooth(h=0.5)",
                  "linear",
                  "step"),
       col    = c(cols_m, "blue","darkgreen","magenta","grey40","grey70"),
       lwd    = 2,
       lty    = c(rep(1, length(methods)), 2,3,4,5,6),
       bty="n")

