library(grid)
postscript()
grid.start()
x <- runif(10)
y <- runif(10)

plot.layout <- grid.layout(ncol=3, nrow=3, 
                           widths=unit(c(5, 1, 2), 
                                       c("lines", "null", "lines")),
                           heights=unit(c(3, 1, 5), 
                                        c("lines", "null", "lines")))
plot.vp <- viewport(layout=plot.layout)
data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                         xscale=range(x) + c(-.05, .05)*diff(range(x)),
                         yscale=range(y) + c(-.05, .05)*diff(range(y)))
title.vp <- viewport(layout.pos.row=1)

push.viewport(plot.vp)
grid.points(x, y, vp=data.vp)
grid.rect(vp=data.vp)
grid.xaxis(vp=data.vp)
grid.yaxis(vp=data.vp)
grid.text("x axis", y=unit(-4, "lines"), 
          gp=gpar(fontsize=14), vp=data.vp)
grid.text("y axis", x=unit(-4, "lines"), 
          gp=gpar(fontsize=14), rot=90, vp=data.vp)
grid.text("A Simple Plot", gp=gpar(fontsize=16), vp=title.vp)
pop.viewport(plot.vp)

splot.layout <- function(margins) {
  grid.layout(ncol=3, nrow=3,
              widths=unit.c(margins[2], unit(1, "null"), margins[4]),
              heights=unit.c(margins[3], unit(1, "null"), margins[1]))
}

splot.draw.data <- function(x, y, xlabel, ylabel, vp) {
  push.viewport(vp)
  grid.points(x, y)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.text(xlabel, y=unit(-4, "lines"), gp=gpar(fontsize=14))
  grid.text(ylabel, x=unit(-4, "lines"), gp=gpar(fontsize=14), rot=90)
  pop.viewport(vp)
}

splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis",
                  title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines")) {
  grid.newpage()
  plot.layout <- splot.layout(margins)
  plot.vp <- viewport(layout=plot.layout)
  push.viewport(plot.vp)
  data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                           xscale=range(x) + c(-.05, .05)*diff(range(x)),
                           yscale=range(y) + c(-.05, .05)*diff(range(y)))
  splot.draw.data(x, y, xlabel, ylabel, data.vp)
  title.vp <- viewport(layout.pos.row=1)
  grid.text(title, gp=gpar(fontsize=16), vp=title.vp)
  pop.viewport(plot.vp)
}
                  
splot()

splot(1:10, 1:10, title="The Standard 1:10 Plot", 
      xlabel="1:10", ylabel="1:10")

splot.layout <- function(margins) {
  grid.layout(ncol=3, nrow=3,
              widths=unit.c(margins[2], unit(1, "null"), margins[4]),
              heights=unit.c(margins[3], unit(1, "null"), margins[1]))
}

splot.draw.data <- function(x, y, xlabel, ylabel, vp) {
  push.viewport(vp)
  grid.points(x, y)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.text(xlabel, y=unit(-4, "lines"), gp=gpar(fontsize=14))
  grid.text(ylabel, x=unit(-4, "lines"), gp=gpar(fontsize=14), rot=90)
  pop.viewport(vp)
}

splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis",
                  title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines")) {
  grid.newpage()
  plot.layout <- splot.layout(margins)
  plot.vp <- viewport(layout=plot.layout)
  push.viewport(plot.vp)
  data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                           xscale=range(x) + c(-.05, .05)*diff(range(x)),
                           yscale=range(y) + c(-.05, .05)*diff(range(y)))
  splot.draw.data(x, y, xlabel, ylabel, data.vp)
  title.vp <- viewport(layout.pos.row=1)
  grid.text(title, gp=gpar(fontsize=16), vp=title.vp)
  pop.viewport(plot.vp)
}
                  
splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis",
                  title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines"),
                  vp=NULL, add=FALSE) {
  if (!add)
    grid.newpage()
  plot.layout <- splot.layout(margins)
  plot.vp <- viewport(layout=plot.layout)
  push.viewport(vp, plot.vp)
  data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                           xscale=range(x) + c(-.05, .05)*diff(range(x)),
                           yscale=range(y) + c(-.05, .05)*diff(range(y)))
  splot.draw.data(x, y, xlabel, ylabel, data.vp)
  title.vp <- viewport(layout.pos.row=1)
  grid.text(title, gp=gpar(fontsize=16), vp=title.vp)
  pop.viewport(plot.vp, vp)
}

w <- runif(50)
x <- rnorm(50)
y <- rexp(50)
z <- rbinom(50, 10, .5)
data <- data.frame(w, x, y, z)
grid.newpage()
top.vp <- viewport(layout=grid.layout(4, 4))
push.viewport(top.vp)
for (i in 1:4) 
  for (j in 1:4) 
    if (i != j)
      splot(data[,j], data[,i], 
            title="", xlabel="", ylabel="",
            margins=unit(c(3, 3, 0, 0), "lines"),
            vp=viewport(layout.pos.row=i, layout.pos.col=j),
            add=TRUE)
pop.viewport(top.vp)

draw.str.or.obj <- function(text, ...) {
  if (is.character(text))
    grid.text(text, ...)
  else
    grid.draw(text)
}

splot.data <- function(x, y) {
  grid.points(x, y)
  grid.rect()
}

splot.viewports <- function(x, y, margins) {
  plot.layout <- 
    grid.layout(ncol=3, nrow=3, 
                widths=unit.c(margins[2], unit(1, "null"), margins[4]),
                heights=unit.c(margins[3], unit(1, "null"), margins[1]))
  plot.vp <- viewport(layout=plot.layout)
  data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                           xscale=range(x) + c(-.05, .05)*diff(range(x)),
                           yscale=range(y) + c(-.05, .05)*diff(range(y)))
  title.vp <- viewport(layout.pos.row=1)
  list(plot.vp=plot.vp, data.vp=data.vp, title.vp=title.vp)
}

splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis", title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines"),
                  xaxis=grid.xaxis(draw=FALSE), yaxis=grid.yaxis(draw=FALSE),
                  data=splot.data, 
                  vp=NULL, add=FALSE) {
  if (!add)
    grid.newpage()
  vps <- splot.viewports(x, y, margins)
  push.viewport(vp, vps$plot.vp, vps$title.vp)
  draw.str.or.obj(title, gp=gpar(fontsize=16))
  pop.viewport(vps$title.vp)
  push.viewport(vps$data.vp)
  data(x, y)
  grid.draw(xaxis)
  grid.draw(yaxis)
  draw.str.or.obj(xlabel, y=unit(-4, "lines"), gp=gpar(fontsize=14))
  draw.str.or.obj(ylabel, x=unit(-4, "lines"), gp=gpar(fontsize=14), rot=90)
  pop.viewport(vps$data.vp, vps$plot.vp, vp)
  invisible(vps)
}

splot()

library(modreg)
my.data <- function(x, y) {
  splot.data(x, y)
  lo <- (loess(y ~ x))
  grid.lines(lo$x, lo$fitted, default.units="native")
}
splot(1:100, 1:100+runif(100, -10, 10), data=my.data)

splot(title=grid.text("Custom Title", just="right", gp=gpar(fontsize=24),
                      x=unit(1, "npc") - unit(1, "inches"), draw=FALSE))

svps <- splot(title="")
push.viewport(svps$plot.vp)
grid.text("Title Centred on Data Region", gp=gpar(fontsize=16),
          vp=viewport(layout.pos.row=1, layout.pos.col=2))

draw.details.splot <- function(sp, grob, recording=TRUE) {
  if (!sp$add)
    grid.newpage(recording=FALSE)
  push.viewport(sp$plot.vp, sp$title.vp, recording=FALSE)
  grid.draw(sp$title, recording=FALSE)
  pop.viewport(sp$title.vp, recording=FALSE)
  push.viewport(sp$data.vp, recording=FALSE)
  grid.draw(sp$data, recording=FALSE)
  grid.draw(sp$xaxis, recording=FALSE)
  grid.draw(sp$yaxis, recording=FALSE)
  grid.draw(sp$xlabel, recording=FALSE)
  grid.draw(sp$ylabel, recording=FALSE)  
  pop.viewport(sp$data.vp, sp$plot.vp, recording=FALSE)
}

make.str.or.obj <- function(text, ...) {
  if (is.character(text))
    grid.text(text, ..., draw=FALSE)
  else
    text
}

splot.data <- function(x, y) {
  grid.collection(points=grid.points(x, y, draw=FALSE), 
                  box=grid.rect(draw=FALSE),
                  draw=FALSE) 
}

splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis", title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines"),
                  xaxis=grid.xaxis(draw=FALSE), yaxis=grid.yaxis(draw=FALSE),
                  data=splot.data, draw=TRUE, add=FALSE,
                  vp=NULL) {
  vps <- splot.viewports(x, y, margins)
  title <- make.str.or.obj(title, gp=gpar(fontsize=16))
  xlabel <- make.str.or.obj(xlabel, y=unit(-4, "lines"), gp=gpar(fontsize=14))
  ylabel <- make.str.or.obj(ylabel, x=unit(-4, "lines"), 
                            gp=gpar(fontsize=14), rot=90)
  sp <- list(x=x, y=y, title=title, xlabel=xlabel, ylabel=ylabel,
             data=data(x, y), data.func=data, xaxis=xaxis, yaxis=yaxis,
             plot.vp=vps$plot.vp, data.vp=vps$data.vp, 
             title.vp=vps$title.vp, add=add, vp=vp)
  grid.grob(sp, "splot", draw)
}

splot()

framer <- function(any.old.grob=grid.text("An Unexciting Default", draw=FALSE)) {
  grid.newpage()
  grid.rect(gp=gpar(border=NULL, fill="grey"))
  vp <- viewport(width=0.8, height=0.8)
  push.viewport(vp)
  grid.rect(gp=gpar(fill="white"))
  grid.text("Frame around ...", y=unit(1, "npc") + unit(1, "cm"), 
            gp=gpar(fontsize=20))
  grid.draw(any.old.grob)
  pop.viewport(vp)
}
framer()
framer(splot(add=TRUE, draw=FALSE))

draw.details.simple <- function(simple, grob, recording=TRUE) {
  grid.draw(simple$splot, recording=FALSE)
}
simple <- function() {
  splot <- splot(draw=FALSE)
  grid.grob(list(splot=splot), "simple")
}
simple()
  
draw.details.splot <- function(sp, grob, recording=TRUE) {
  if (!sp$add)
    grid.newpage(recording=FALSE)
  push.viewport(sp$plot.vp, sp$title.vp, recording=FALSE)
  grid.draw(sp$title, recording=FALSE)
  pop.viewport(sp$title.vp, recording=FALSE)
  push.viewport(sp$data.vp, recording=FALSE)
  grid.draw(sp$data, recording=FALSE)
  grid.draw(sp$xaxis, recording=FALSE)
  grid.draw(sp$yaxis, recording=FALSE)
  grid.draw(sp$xlabel, recording=FALSE)
  grid.draw(sp$ylabel, recording=FALSE)  
  pop.viewport(sp$data.vp, sp$plot.vp, recording=FALSE)
}

make.str.or.obj <- function(text, ...) {
  if (is.character(text))
    grid.text(text, ..., draw=FALSE)
  else
    text
}

splot.data <- function(x, y) {
  grid.collection(points=grid.points(x, y, draw=FALSE), 
                  box=grid.rect(draw=FALSE),
                  draw=FALSE) 
}

splot <- function(x=runif(10), y=runif(10),
                  xlabel="x axis", ylabel="y axis", title="A Simple Plot",
                  margins=unit(c(5, 5, 3, 2), "lines"),
                  xaxis=grid.xaxis(draw=FALSE), yaxis=grid.yaxis(draw=FALSE),
                  data=splot.data, draw=TRUE, add=FALSE,
                  vp=NULL) {
  vps <- splot.viewports(x, y, margins)
  title <- make.str.or.obj(title, gp=gpar(fontsize=16))
  xlabel <- make.str.or.obj(xlabel, y=unit(-4, "lines"), gp=gpar(fontsize=14))
  ylabel <- make.str.or.obj(ylabel, x=unit(-4, "lines"), 
                            gp=gpar(fontsize=14), rot=90)
  sp <- list(x=x, y=y, title=title, xlabel=xlabel, ylabel=ylabel,
             data=data(x, y), data.func=data, xaxis=xaxis, yaxis=yaxis,
             plot.vp=vps$plot.vp, data.vp=vps$data.vp, 
             title.vp=vps$title.vp, add=add, vp=vp)
  grid.grob(sp, "splot", draw)
}

splot.plot.vp <- function(margins) {
  plot.layout <- 
    grid.layout(ncol=3, nrow=3, 
                widths=unit.c(margins[2], unit(1, "null"), margins[4]),
                heights=unit.c(margins[3], unit(1, "null"), margins[1]))
  plot.vp <- viewport(layout=plot.layout)
}

splot.data.vp <- function(x, y) {
  data.vp <- viewport(layout.pos.row=2, layout.pos.col=2,
                           xscale=range(x) + c(-.05, .05)*diff(range(x)),
                           yscale=range(y) + c(-.05, .05)*diff(range(y)))
}

splot.title.vp <- function() {
  title.vp <- viewport(layout.pos.row=1)
}

splot.viewports <- function(x, y, margins) {
  list(plot.vp=splot.plot.vp(margins), 
       data.vp=splot.data.vp(x, y), 
       title.vp=splot.title.vp())
}

edit.details.splot <- function(splot, new.values) {
  slot.names <- names(new.values)
  x.index <- match("x", slot.names, nomatch=0)
  y.index <- match("y", slot.names, nomatch=0)
  if (x.index != 0 || y.index != 0) {
    x <- if (x.index) new.values[[x.index]] else splot$x
    y <- if (y.index) new.values[[y.index]] else splot$y
    splot$data.vp <- splot.data.vp(x, y)
    splot$data <- splot$data.func(x, y)
    grid.edit(splot$xaxis, at=NA, redraw=FALSE)
    grid.edit(splot$yaxis, at=NA, redraw=FALSE)
    if (x.index) x.index <- -x.index else x.index <- NA
    if (y.index) y.index <- -y.index else y.index <- NA
    new.values <- new.values[c(x.index, y.index)]
  }
  splot
}

sp <- splot()
grid.edit(sp, grid.prop.list(x=1:10, y=rexp(10)))

x <- grid.xaxis(vp=viewport(w=.5, h=.5))

grid.edit(x, gp=gpar(col="red"))

grid.edit(x, "labels", gp=gpar(col="green"))

grid.edit(x, at=c(0.0, 0.5, 1.0))

grid.edit(x, "labels", grid.prop.list(gp=gpar(col="black"), rot=30))

grid.newpage()
push.viewport(viewport(layout=grid.layout(1, 2, respect=TRUE)))

x <- 1:10
y1 <- rnorm(10)
vp1a <- viewport(layout.pos.col=1)
vp1b <- viewport(width=0.6, height=0.6,
                 xscale=c(0, 11), yscale=c(-4, 4))
push.viewport(vp1a, vp1b)
xa <- grid.xaxis()
ya <- grid.yaxis()
grid.points(x, y1)
pop.viewport(vp1b, vp1a)

y2 <- rnorm(10)
vp2a <- viewport(layout.pos.col=2)
vp2b <- viewport(width=0.6, height=0.6,
                 xscale=c(0, 11), yscale=c(-4, 4))
push.viewport(vp2a, vp2b)
grid.draw(xa)
grid.draw(ya)
grid.points(x, y2)
pop.viewport(vp2b, vp2a)

grid.edit(xa, at=c(1, 5, 9))

grid.legend(1:3, c("one line", "two\nlines", "three\nlines\nof text"))

  top.vp <- viewport(w=0.8, h=0.8)
  push.viewport(top.vp)
  x <- runif(10)
  y1 <- runif(10)
  y2 <- runif(10)
  pch <- 1:3
  labels <- c("Girls", "Boys", "Other")
  gf <- grid.frame()
  plot <- grid.collection(grid.rect(draw=F),
                          grid.points(x, y1, pch=1, draw=F),
                          grid.points(x, y2, pch=2, draw=F),
                          grid.xaxis(draw=F),
                          grid.yaxis(draw=F),
                          draw=F)
  grid.pack(gf, plot)
  grid.pack(gf, grid.legend(pch, labels, draw=F), 
            height=unit(1,"null"), side="right")
  grid.rect(gp=gpar(lty="dotted"))
  pop.viewport(top.vp)
  grid.rect(gp=gpar(lty="dashed"), w=.99, h=.99)

dev.off()
