library(lattice)
load("fig2.rdata")

n <- prod(dim(pmat.ssr))

tmp <- expand.grid(x = directs, y = indirects)
graph.frame <- rbind(tmp, tmp)

graph.frame$z <- c(as.vector(pmat.ssr),
                   as.vector(pmat.ks))

graph.frame$t <- factor(c(rep("SSR", n),
                          rep("KS", n)))

col.l <- colorRampPalette(c('white', 'black'))(1000)

depth.breaks <- do.breaks(c(0,1), 20)
fig2 <- levelplot(z ~ x*y | t,
                  graph.frame,
                  cuts = 20,
                  col.regions = col.l,
                  colorkey=FALSE,
                  xlab = "Direct Effect",
                  ylab = "Indirect Effect")
  
pdf("CoppockJEPS_figure2.pdf", width = 8, height = 5)
print(fig2)
dev.off()

