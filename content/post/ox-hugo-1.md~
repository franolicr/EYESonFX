+++
title = "What's in a name?"
author = ["Robert Franolic"]
publishDate = 2020-02-29T00:00:00+00:00
draft = false
+++

{{< figure src="/ox-hugo/The_Tiger_That_Isn't.jpg" >}}

```R
library(VennDiagram)

# Re-write function to allow labels to be printed for each of the 15 intersections
draw.quad.venn <- function (area1, area2, area3, area4, n12, n13, n14, n23, n24,
    n34, n123, n124, n134, n234, n1234, category = rep("", 4), labels = c(1:15),
    lwd = rep(2, 4), lty = rep("solid", 4), col = rep("black",
      4), fill = NULL, alpha = rep(0.5, 4), label.col = rep("black",
      15), cex = rep(1, 15), fontface = rep("plain", 15), fontfamily = rep("serif",
      15), cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22, 0.22,
      0.11, 0.11), cat.col = rep("black", 4), cat.cex = rep(1,
      4), cat.fontface = rep("plain", 4), cat.fontfamily = rep("serif",
      4), cat.just = rep(list(c(0.5, 0.5)), 4), rotation.degree = 0,
    rotation.centre = c(0.5, 0.5), ind = TRUE, cex.prop = NULL,
    print.mode = "raw", sigdigs = 3, direct.area = FALSE, area.vector = 0,
    ...)
{
    if (length(category) == 1) {
      cat <- rep(category, 4)
    }
    else if (length(category) != 4) {
      flog.error("Unexpected parameter length for 'category'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'category'")
    }
    if (length(lwd) == 1) {
      lwd <- rep(lwd, 4)
    }
    else if (length(lwd) != 4) {
      flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'lwd'")
    }
    if (length(lty) == 1) {
      lty <- rep(lty, 4)
    }
    else if (length(lty) != 4) {
      flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'lty'")
    }
    if (length(col) == 1) {
      col <- rep(col, 4)
    }
    else if (length(col) != 4) {
      flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'col'")
    }
    if (length(label.col) == 1) {
      label.col <- rep(label.col, 15)
    }
    else if (length(label.col) != 15) {
      flog.error("Unexpected parameter length for 'label.col'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'label.col'")
    }
    if (length(cex) == 1) {
      cex <- rep(cex, 15)
    }
    else if (length(cex) != 15) {
      flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cex'")
    }
    if (length(fontface) == 1) {
      fontface <- rep(fontface, 15)
    }
    else if (length(fontface) != 15) {
      flog.error("Unexpected parameter length for 'fontface'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'fontface'")
    }
    if (length(fontfamily) == 1) {
      fontfamily <- rep(fontfamily, 15)
    }
    else if (length(fontfamily) != 15) {
      flog.error("Unexpected parameter length for 'fontfamily'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'fontfamily'")
    }
    if (length(fill) == 1) {
      fill <- rep(fill, 4)
    }
    else if (length(fill) != 4 & length(fill) != 0) {
      flog.error("Unexpected parameter length for 'fill'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'fill'")
    }
    if (length(alpha) == 1) {
      alpha <- rep(alpha, 4)
    }
    else if (length(alpha) != 4 & length(alpha) != 0) {
      flog.error("Unexpected parameter length for 'alpha'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'alpha'")
    }
    if (length(cat.pos) == 1) {
      cat.pos <- rep(cat.pos, 4)
    }
    else if (length(cat.pos) != 4) {
      flog.error("Unexpected parameter length for 'cat.pos'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.pos'")
    }
    if (length(cat.dist) == 1) {
      cat.dist <- rep(cat.dist, 4)
    }
    else if (length(cat.dist) != 4) {
      flog.error("Unexpected parameter length for 'cat.dist'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.dist'")
    }
    if (length(cat.col) == 1) {
      cat.col <- rep(cat.col, 4)
    }
    else if (length(cat.col) != 4) {
      flog.error("Unexpected parameter length for 'cat.col'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.col'")
    }
    if (length(cat.cex) == 1) {
      cat.cex <- rep(cat.cex, 4)
    }
    else if (length(cat.cex) != 4) {
      flog.error("Unexpected parameter length for 'cat.cex'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.cex'")
    }
    if (length(cat.fontface) == 1) {
      cat.fontface <- rep(cat.fontface, 4)
    }
    else if (length(cat.fontface) != 4) {
      flog.error("Unexpected parameter length for 'cat.fontface'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.fontface'")
    }
    if (length(cat.fontfamily) == 1) {
      cat.fontfamily <- rep(cat.fontfamily, 4)
    }
    else if (length(cat.fontfamily) != 4) {
      flog.error("Unexpected parameter length for 'cat.fontfamily'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter length for 'cat.fontfamily'")
    }
    if (!(class(cat.just) == "list" & length(cat.just) == 4 &
      length(cat.just[[G1]]) == 2 & length(cat.just[[2]]) ==
      2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) ==
      2)) {
      flog.error("Unexpected parameter format for 'cat.just'",
	  name = "VennDiagramLogger")
      stop("Unexpected parameter format for 'cat.just'")
    }
    cat.pos <- cat.pos + rotation.degree
    if (direct.area) {
      areas <- area.vector
      for (i in 1:15) {
	  assign(paste("a", i, sep = ""), area.vector[i])
      }
    }
    else {
      a6 <- n1234
      a12 <- n123 - a6
      a11 <- n124 - a6
      a5 <- n134 - a6
      a7 <- n234 - a6
      a15 <- n12 - a6 - a11 - a12
      a4 <- n13 - a6 - a5 - a12
      a10 <- n14 - a6 - a5 - a11
      a13 <- n23 - a6 - a7 - a12
      a8 <- n24 - a6 - a7 - a11
      a2 <- n34 - a6 - a5 - a7
      a9 <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15
      a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15
      a1 <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13
      a3 <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11
      areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
	  a12, a13, a14, a15)
    }
    areas.error <- c("a1  <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13",
      "a2  <- n34 - a6 - a5 - a7", "a3  <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11",
      "a4  <- n13 - a6 - a5 - a12", "a5  <- n134 - a6", "a6  <- n1234",
      "a7  <- n234 - a6", "a8  <- n24 - a6 - a7 - a11", "a9  <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15",
      "a10 <- n14 - a6 - a5 - a11", "a11 <- n124 - a6", "a12 <- n123 - a6",
      "a15 <- n12 - a6 - a11 - a12", "a13 <- n23 - a6 - a7 - a12",
      "a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15")
    for (i in 1:length(areas)) {
      if (areas[i] < 0) {
	  flog.error(paste("Impossible:", areas.error[i], "produces negative area"),
	      name = "VennDiagramLogger")
	  stop(paste("Impossible:", areas.error[i], "produces negative area"))
      }
    }
    if (length(cex.prop) > 0) {
      if (length(cex.prop) != 1) {
	  flog.error("Value passed to cex.prop is not length 1",
	      name = "VennDiagramLogger")
	  stop("Value passed to cex.prop is not length 1")
      }
      func = cex.prop
      if (class(cex.prop) != "function") {
	  if (cex.prop == "lin") {
	      func = function(x) x
	  }
	  else if (cex.prop == "log10") {
	      func = log10
	  }
	  else flog.error(paste0("Unknown value passed to cex.prop: ",
	      cex.prop), name = "VennDiagramLogger")
	  stop(paste0("Unknown value passed to cex.prop: ",
	      cex.prop))
      }
      maxArea = max(areas)
      for (i in 1:length(areas)) {
	  cex[i] = cex[i] * func(areas[i])/func(maxArea)
	  if (cex[i] <= 0)
	      stop(paste0("Error in rescaling of area labels: the label of area ",
		i, " is less than or equal to zero"))
      }
    }
    grob.list <- gList()
    ellipse.positions <- matrix(nrow = 4, ncol = 7)
    colnames(ellipse.positions) <- c("x", "y", "a", "b", "rotation",
      "fill.mapping", "line.mapping")
    ellipse.positions[1, ] <- c(0.65, 0.47, 0.35, 0.2, 45, 2,
      2)
    ellipse.positions[2, ] <- c(0.35, 0.47, 0.35, 0.2, 135, 1,
      1)
    ellipse.positions[3, ] <- c(0.5, 0.57, 0.33, 0.15, 45, 4,
      4)
    ellipse.positions[4, ] <- c(0.5, 0.57, 0.35, 0.15, 135, 3,
      3)
    for (i in 1:4) {
      grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i,
	  "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i,
	  "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i,
	  "rotation"], gp = gpar(lty = 0, fill = fill[ellipse.positions[i,
	  "fill.mapping"]], alpha = alpha[ellipse.positions[i,
	  "fill.mapping"]])))
    }
    for (i in 1:4) {
      grob.list <- gList(grob.list, ellipse(x = ellipse.positions[i,
	  "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i,
	  "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i,
	  "rotation"], gp = gpar(lwd = lwd[ellipse.positions[i,
	  "line.mapping"]], lty = lty[ellipse.positions[i,
	  "line.mapping"]], col = col[ellipse.positions[i,
	  "line.mapping"]], fill = "transparent")))
    }
    label.matrix <- matrix(nrow = 15, ncol = 3)
    colnames(label.matrix) <- c("label", "x", "y")
    label.matrix[1, ] <- c(a1, 0.35, 0.77)
    label.matrix[2, ] <- c(a2, 0.5, 0.69)
    label.matrix[3, ] <- c(a3, 0.65, 0.77)
    label.matrix[4, ] <- c(a4, 0.31, 0.67)
    label.matrix[5, ] <- c(a5, 0.4, 0.58)
    label.matrix[6, ] <- c(a6, 0.5, 0.47)
    label.matrix[7, ] <- c(a7, 0.6, 0.58)
    label.matrix[8, ] <- c(a8, 0.69, 0.67)
    label.matrix[9, ] <- c(a9, 0.18, 0.58)
    label.matrix[10, ] <- c(a10, 0.32, 0.42)
    label.matrix[11, ] <- c(a11, 0.425, 0.38)
    label.matrix[12, ] <- c(a12, 0.575, 0.38)
    label.matrix[13, ] <- c(a13, 0.68, 0.42)
    label.matrix[14, ] <- c(a14, 0.82, 0.58)
    label.matrix[15, ] <- c(a15, 0.5, 0.28)
    processedLabels <- rep("", length(label.matrix[, "label"]))
    if (print.mode[1] == "percent") {
      processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[,
	  "label"]) * 100, digits = sigdigs), "%", sep = "")
      if (isTRUE(print.mode[2] == "raw")) {
	  processedLabels <- paste(processedLabels, "\n(",
	      label.matrix[, "label"], ")", sep = "")
      }
    }
    if (print.mode[1] == "raw") {
      processedLabels <- label.matrix[, "label"]
      if (isTRUE(print.mode[2] == "percent")) {
	  processedLabels <- paste(processedLabels, "\n(",
	      paste(signif(label.matrix[, "label"]/sum(label.matrix[,
		"label"]) * 100, digits = sigdigs), "%)", sep = ""),
	      sep = "")
      }
    }
    for (i in 1:nrow(label.matrix)) {
      grob.list <- gList(grob.list, textGrob(label = labels[i],
	  x = label.matrix[i, "x"], y = label.matrix[i, "y"],
	  gp = gpar(col = label.col[i], cex = cex[i], fontface = fontface[i],
	      fontfamily = fontfamily[i])))
    }
    cat.pos.x <- c(0.18, 0.82, 0.35, 0.65)
    cat.pos.y <- c(0.58, 0.58, 0.77, 0.77)
    for (i in 1:4) {
      this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i],
	  pos = cat.pos[i], dist = cat.dist[i])
      grob.list <- gList(grob.list, textGrob(label = category[i],
	  x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]],
	  gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i],
	      fontfamily = cat.fontfamily[i])))
    }
    grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list,
      rotation.degree, rotation.centre[1], rotation.centre[2]),
      ...)
    if (ind) {
      grid.draw(grob.list)
    }
    return(grob.list)
}


dev.off()
m$p$lbls <- c( "Nerd", "R Code\nTeam", "Hacker", "Stats\nProfessor", "Good\nConsultant",
	    "EYES on FX", "Quant\nTrader", "Front\nOffice Developer", "Hot air", "Computing\nProfessor",
	    "Head\nof IT", "Currency\nPM", "Traditional\nQuant", "FX\nTrader", "FX Sales")
m$p$lbls
m$p$ctgrs <- matrix( c( c( 0, 1.0, 0, 0),
		     rep( c( 1.0, 1.0, 0, 0), 2),
		     rep( c( 1.0, 1.0, 1.0, 0), 4),
		     rep( c( 1.0, 1.0, 1.0, 1.0), 8)),
		  nrow= 15, ncol= 4, byrow= TRUE)

m$p$sqnc <- c( 14, 9, 15, 1, 13, 4, 12, 3, 8, 11, 10, 5, 2, 7, 6)

for( i in 1:( length( m$p$sqnc)+ 1))
    {
    grid.newpage(recording = TRUE)
    tmp <- m$p$lbls
    tmp[ m$p$sqnc[i:15]] <- ""
    venn.plot <- draw.quad.venn(
      alpha= m$p$ctgrs[ i,]* 0.5,
      labels= tmp,
      area1 = 72, area2 = 86, area3 = 50, area4 = 52,
    n12 = 44, n13 = 27, n14 = 32, n23 = 38, n24 = 32, n34 = 20,
    n123 = 18, n124 = 17, n134 = 11, n234 = 13, n1234 = 6,
    category = c("Comms", "FX", "Maths / Stats", "Data / IT"),
    fill = c("black", m$p$clr$grn, m$p$clr$rd, "grey"), lwd= 0,
    lty = c("solid", "solid", "solid", "solid"),
    cex = 1,
    cat.cex = m$p$ctgrs[ i, ]* 1.5,
    cat.col = c("black", m$p$clr$grn, m$p$clr$rd, "dark grey")
)}
```

This is my very first blog post! In it I will explain how I came up with
the name, Eyes on FX, and my intentions for this website.

<!--more-->

According to the Bank for International Settlements (BIS), USD 6.5
trillion of Foregin Exchange (FX) was conducted each day on average in
April 2019. As Blastland and Dilnot advise in their excellent book
'The Tiger that Isn't', we should always consider a comparison, to
answer the simple question: 'is that a big number?'.

According to the World Bank, 2018 global GDP, the value of all the
good and services produced throughout the world that year, was 85.91
trillion, or around 0.33 billion per working day. Very roughly then,
in terms of USD volumes, the FX market is twenty times larger than the
global economy. To repeat, this is a very rough calculation, but
there's no refinement, nor reasonable adjustment, that can change the
conclusion: the FX market, is not just big, it's monstrous!

Apart from it's sheer size, the FX market is critical to the global
economy, trade in goods and services cannot. It's not just it's sheer
size that makes the FX market so important to the global

Despite it's importance the FX market remains relatively opaque
compared to other financial market.


## Why FX? {#why-fx}


### By many measures the Largest market - of any kind {#by-many-measures-the-largest-market-of-any-kind}


### Relevant globally, nationally and individually {#relevant-globally-nationally-and-individually}


### Its what I know {#its-what-i-know}


## Why _Eyes on_ FX? {#why-eyes-on-fx}


### Not transparent compared to other markets {#not-transparent-compared-to-other-markets}

FX: over 6,000, currency: over 10,000 results, stocks: over 50,000 results, bionds: over 30,000
Google scholar: Stock market: About 3,560,000 results, Bond market: About 2,680,000 results,
Currency markets: 2,440,000. Commodity Markets: 1,900,000.

Google stock market data: About 2,260,000,000 results
FX market data: 248,000,000
Currency market data: 931,000,000
Bond market data: 402,000,000
Treasury market data: 153,000,000
Commodity: 140,000,000


### In a literal sense {#in-a-literal-sense}


#### Importance of visualisation {#importance-of-visualisation}

A picture paints a thousand words

-   Exploratory visualization
-   Explanatory visualization


### In a metaphorical sense {#in-a-metaphorical-sense}

Improving understanding


## Intention for the website {#intention-for-the-website}

In three ways: 1) posting blogs 2) providing an on-line reference 3)
providing external references for deeper study and understanding.


### Write about the FX market {#write-about-the-fx-market}


### A brief and accessible online reference to the FX market {#a-brief-and-accessible-online-reference-to-the-fx-market}

Key questions about the FX market:

-   what is FX?
-   who trades FX?
-   where is FX traded?
-   when is FX traded?
-   why is FX traded?
-   how is FX traded?


### Explore challenges {#explore-challenges}


### Provide more details references {#provide-more-details-references}

The online reference is based on my own experience of the markat and also questions rely on three key sources:

-   data
-   books
-   white/academic papers


## Principles {#principles}