
library(grid)

postscript("bugs.ps")
gf1 <- grid.frame(gp=gpar(fontsize=20), draw=FALSE)
grid.pack(gf1, grid.text("howdy", draw=FALSE), draw=FALSE)
grid.pack(gf1, grid.rect(draw=FALSE), col=1, row=1, draw=FALSE)
gf2 <- grid.frame(draw=FALSE)
grid.pack(gf2, gf1, draw=FALSE)
grid.pack(gf2, grid.rect(gp=gpar(col="red"), draw=FALSE), col=1, row=1, draw=FALSE)
grid.draw(gf2)
grid.text("In the bug, you see a black and a red rectangle\nWhen the bug is fixed, the red rectangle overwrites the black one",
          y=unit(1, "npc") - unit(2, "lines"))

# tb stands for testbetween
grid.newpage()
push.viewport(viewport(w=.8, h=.8))
grid.rect(height=unit(1, "mylines"))
grid.rect(height=unit(1, "mylines"), 
          gp=gpar(col="red", lineheight=2))
grid.rect(height=unit(1, "mylines"),
          gp=gpar(col="green"),
          vp=viewport(gp=gpar(lineheight=2)))
grid.text("In the bug, the red rect overwrites the black rect\nWhen the bug is fixed, the green rect overwrites the red rect",
          y=unit(1, "npc"), just=c("center", "top"))
pop.viewport()

# tb stands for testbetween
grid.newpage()
push.viewport(viewport(width=.8, height=.8))
for (i in 1:5) {
  grid.rect(height=unit(1, "mychar"), y=i/6, gp=gpar(fontsize=i*5))
  grid.text("(just testing)", y=i/6, gp=gpar(fontsize=i*5))
}
grid.text("What should the relationship be\nbetween the heights of the rectangles and the heights of the text?",
          y=unit(1, "npc") - unit(1, "lines"), 
          gp=gpar(col="red"))


dev.off()
