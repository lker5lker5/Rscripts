library(ggplot2) 
library(reshape2)

sprdata <- read.csv("~/Personal/MarineFishery/papers/chart/data/sprEnvFactor.csv", header = T, 
                    sep = ",", row.names = "position")

sprdatascaled <- as.data.frame(lapply(sprdata, ggplot2:::rescale01))
sprdatascaled$position <- rownames(sprdata)
sprdatamelted <- reshape2::melt(sprdatascaled)

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(
  mtcarsmelted, 
  aes(x = variable, 
      y = value)) + geom_polygon(aes(group = model, color = model), fill = NA, size = 2, show.legend = FALSE) + geom_line(aes(group = model, color = model), size = 2) + theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()
