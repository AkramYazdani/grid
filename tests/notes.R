library(grid)
postscript("notes.ps")
grid.rect(x=unit(1, "inches"), y=unit(.1, "npc"),
          width=unit(.5, "native"), height=unit(1, "lines"),
          just=c("left", "bottom"))

grid.newpage()
push.viewport(viewport(layout=grid.layout(2, 2)))
push.viewport(viewport(layout.pos.col=2, layout.pos.row=2))
grid.rect()
pop.viewport(2)

grid.newpage()
gt <- grid.text("howdy")
grid.rect(width=unit(1, "grobwidth", gt), height=unit(1, "lines"))

grid.newpage()
gf <- grid.frame(draw=TRUE)
grid.pack(gf, grid.rect(x=unit(1, "npc"), 
                        w=unit(1, "char"), h=unit(1, "char"),
                        just=c("right", "centre"), draw=F))
grid.pack(gf, grid.text("A label", draw=F), side="right")

grid.newpage()
gf <- grid.frame(draw=TRUE)
grid.pack(gf, grid.rect(width=unit(1, "inches"),
                        height=unit(.5, "npc"),
                        draw=F))

dev.off()
