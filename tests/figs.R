library(grid)
postscript("baby.eps", horiz=F, width=4, height=3)
grid.start()
grid.rect(x=0.5, y=0.5, h=1, w=1, 
          just="centre", gp=gpar(border=NULL, fill="#FFBF00"))
grid.rect(x=0.33, y=0, h=1, w=0.2, 
          just=c("left", "bottom"), gp=gpar(border=NULL, fill="#FF8000"))
grid.text(x=0.5, y=0.5, "A Label")
dev.off()
postscript("strip.eps", horiz=F, width=4, height=3)
grid.start()
vp <- viewport(x=0.5, y=1, w=1, h=unit(1, "lines"),
                    just=c("centre", "top"))
push.viewport(vp)
grid.rect(gp=gpar(border=NULL, fill="#FFBF00"))
grid.rect(x=0.33, y=0, h=1, w=0.2, 
          just=c("left", "bottom"), 
          gp=gpar(border=NULL, fill="#FF8000"))
grid.text(x=0.5, y=0.5, "A Label")
pop.viewport(vp)
dev.off()
postscript("simple.eps", horiz=F, width=4, height=3)
grid.start()
vp <- viewport(x=0.5, y=0.5, w=0.6, h=0.6,
                    xscale=c(0, 11), yscale=c(0, 11))
push.viewport(vp)
grid.rect()
grid.points(runif(10, 1, 10), runif(10, 1, 10))
grid.xaxis()
grid.yaxis()
pop.viewport(vp)
dev.off()
postscript("layout.eps", horiz=F, width=4, height=3)
grid.start()
vp1 <- viewport(x=0.5, y=0.5, w=0.6, h=0.6,
                     layout=grid.layout(2, 1, 
                                        heights=unit(c(1, 1), 
                                                     c("lines", "null"))))
push.viewport(vp1)
vp2 <- viewport(layout.pos.row=1)
push.viewport(vp2)
grid.rect(x=0.5, y=0.5, h=1, w=1, 
          just="centre", 
          gp=gpar(border=NULL, fill="#FFBF00"))
grid.rect(x=0.33, y=0, h=1, w=0.2, 
          just=c("left", "bottom"), 
          gp=gpar(border=NULL, fill="#FF8000"))
grid.text(x=0.5, y=0.5, "A Label")
pop.viewport(vp2)
vp3 <- viewport(layout.pos.row=2, xscale=c(0, 11), yscale=c(0, 11))
push.viewport(vp3)
grid.rect()
grid.points(runif(10, 1, 10), runif(10, 1, 10))
grid.xaxis()
grid.yaxis()
pop.viewport(vp3)
pop.viewport(vp1)
dev.off()
