# load data
yc <- read.csv("~/Documents/Learning/R/scripts/data/yellowcroaker.csv")
yc <- yc[c(-1,-5,-6,-7)]
names(yc) <- c("area", "long", "lat")

# legend indicating density using color
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  fields::image.plot(xm,ym,z, col = colramp(256), legend.only = T, add =F, zlim = c(0, 160))
}

# adjust axises
long_vals <- seq(120,128, by=.5)
long_labels <- paste(long_vals, c("°E"), sep = "")
lat_vals <- seq(27,33, by=.5)
lat_labels <- paste(lat_vals, c("°N"), sep = "")

opar <- par(no.readonly = T) 
par(mar = c(5,4,4,5) + .1)
par(family='STKaiti')
with(yc, 
      {
        smoothScatter(long, lat, nrpoints = 0, 
                      postPlotHook = fudgeit, 
                      main="模拟小黄鱼分布图",
                      # to set the axises
                      xlim = c(120, 128), ylim = c(27, 33),
                      xaxt = "n", yaxt = "n",
                      xaxs = "i", yaxs = "i", # intersection at 0
                      tck = .5, ann = F
                      )
        grid(nx = length(long_vals) - 1, ny = length(lat_vals) - 1, lwd = 1, lty =1)
        axis(1, at = long_vals, labels = long_labels, las = 0)
        axis(2, at = lat_vals, labels = lat_labels, las = 2)
        text(long, lat, area, cex = 0.8, col = "gray")
        title("小黄鱼模拟分布图")
      })
par <- opar