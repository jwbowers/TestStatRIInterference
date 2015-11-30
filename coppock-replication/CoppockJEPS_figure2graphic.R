library(lattice)
load("fig2.rdata")

n <- prod(dim(pmat.ssr))

tmp <- expand.grid(x=directs, y=indirects)
graph.frame <- rbind(tmp, tmp, tmp, tmp, tmp)

graph.frame$z <- c(as.vector(pmat.ssr),
                   as.vector(pmat.ks),
                   as.vector(pmat.ssr.comp),
                   as.vector(pmat.ks.comp),
                   as.vector(pmat.ssr.comp.y0))

graph.frame$t <- factor(c(rep("SSR (Coppock)", n),
                          rep("KS (Coppock)", n),
                          rep("SSR (Computed)", n),
                          rep("KS (Computed)", n),
                          rep("SSR (Y0, Comp)", n)))

col.l <- colorRampPalette(c('white', 'black'))(1000)

depth.breaks <- do.breaks(c(0,1), 20)
fig2 <- levelplot(z ~ x*y | t, graph.frame, cuts=20, col.regions=col.l,
                  colorkey=FALSE,
                  at=depth.breaks,
                        ylab = "Hypothesized indirect effect",
                        xlab = "Hypothesized direct effect",
                        scales=list(x=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1)),
                                    y=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(y0=-.5, y1= -.5, x0=directs[direct_breaks[1]], x1=directs[direct_breaks[2]], angle=90,code=3)
                          larrows(x0=-.6, x1= -.6, y0=indirects[indirect_breaks[1]], y1=indirects[indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)
  
pdf("CoppockJEPS_figure2.pdf", width = 8, height = 8)
print(fig2)
dev.off()

