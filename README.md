# Attractors
The code below produces Aizawa attractors in 3D using plotly.

```
library(deSolve)
library(plotly)

parms <- c(a = 0.95, b = 0.7, c = 0.6, d = 3.5, e = 0.25, f = 0.1)
init <- c(x = 0, y = 1, z = 1)
Aizawa <- function(t = 1, init, parms) {
  with(as.list(c(parms,init)), {dx <- (z - b) * x - d * y;
  dy <- d * x + (z - b) * y;
  dz <- c + a * z - ((z ^ 3) / 3) - (x ^ 2 + y ^ 2) * (1 + e * z) + f *  z * x ^ 3
  list(c(dx, dy, dz))
  })
}

iter <- seq(0, 5000, 0.01)
out <- deSolve::ode(y = c(x = 0, y = 1, z = 1), times = iter, func = Aizawa, parms = parms)
out <- as.data.frame(out)

plot_ly(out, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
        opacity = 0.02) 
```
