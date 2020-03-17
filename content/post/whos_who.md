+++
title = "Who's who in the Zoo"
author = ["Robert Franolic"]
publishDate = 2020-03-02T00:00:00+00:00
tags = ["Who"]
draft = false
+++

o![](/ox-hugo/Whos_who.gif)
Who are the most important players in the FX market.

```R
m$i$ermny <- fread( "/Dnk/Srcs/Ermny/Out/Ermny_Srvy.csv")

# restrict to top 10
m$x$ermny <- m$i$ermny[ rnk <= 10]
m$x$ermny[ lp== "Bank of America Merrill Lynch", lp:= "Bank of America"]

# Order by when LP's dropped out ( earliest first) then by rank in latest year
m$p$lps <- m$x$ermny[ , .( lst_yr= max( yr), yr, shr), .( lp)][ yr== lst_yr][ order( lst_yr, shr)]
m$x$ermny[ , lp:= factor( lp, levels= m$p$lps$lp)]

m$x$ermny[ , hrfndl:= sum( shr^ 2), yr]

m$o$ermny <- m$x$ermny
# Commentary
m$o$cmnts <- list(
    3, "black", 85, -300, "1. Dollar dominates in 1992 \n- 80% of trades against USD")

m$o$cmnts <- as.data.table( matrix( unlist( m$o$cmnts), ncol= 5, byrow= TRUE,
				 dimnames= list( NULL, c( "frm", "clr", "x", "y", "cmnt"))))
m$o$cmnts[ , `:=`( x= as.numeric( x), y= as.numeric( y))]

# Let's plot it!
saveGIF({ ani.options(interval = 2, nmax = 75)

for( j in m$o$ermny[ , .N, lp][ order( lp), lp]){
    print(
      ggplot( data= m$o$ermny) +
      # lines
      geom_line( mapping= aes( x= yr, y= shr, group= lp),
		colour= m$p$clr$drk_gry) +
      # labels
      geom_text( data= function( x)
	  x[ lp!= j, .( lst_yr= max( yr), yr, rnk, shr), .( lp)][ yr== lst_yr],
	  mapping= aes( x= yr + 3/12, y= shr, label= paste( lp), hjust= 0),
	  colour= m$p$clr$drk_gry, size= 5) +
      # last point
      geom_point( data= function( x)
	  x[ lp!= j, .( lst_yr= max( yr), yr, shr), .( lp)][ yr== lst_yr],
	  mapping= aes( x= yr, y= shr),
	  colour= m$p$clr$drk_gry, size= 3)+
      # highlighted line
      geom_line( data= function( x) x[ lp== j],
		mapping= aes( x= yr, y= shr),
		colour= m$p$clr$rd, size= 2) +
      # highlighted last point line
      geom_point(
	  data= function( x) x[ lp== j, .( lst_yr= max( yr), yr, shr), .( lp)][ yr== lst_yr],
	  mapping= aes( x= yr, y= shr),
	  colour= m$p$clr$rd, size= 4)+
      # white background highlighted label
      geom_text( data= function( x)
	  x[ lp== j, .( lst_yr= max( yr), yr, rnk, shr), .( lp)][ yr== lst_yr][
	      rep( 1, 4), .( yr, shr, rnk, x= c( 1/24, 0, -1/24, 0),
			    y= c( 0, 0.0002, 0, -0.0002),
			    lp)],
	  mapping= aes( x= yr + 3/12 + x, y= shr+ y,
		       label= paste( rnk, lp, sprintf( fmt= "%g%%", round( shr* 100),1))),
	  colour= "white", fontface= "bold", hjust= 0, size= 5) +
      # highlighted label
      geom_text( data= function( x)
	  x[ lp== j, .( lst_yr= max( yr), yr, rnk, shr), .( lp)][ yr== lst_yr],
	  mapping= aes( x= yr + 3/12, y= shr,
		       label= paste( rnk, lp, sprintf( fmt= "%g%%", round( shr* 100, 1)))),
		fontface= "bold", hjust= 0, size= 5, colour= m$p$clr$rd) +
      # logo
      annotation_custom( m$p$eyesonfx_lg, xmin= 2022, xmax= 2025, ymin=0.195, ymax= 0.2) +
      # source
      annotate( geom= "text", x= 2020, y= -10, hjust= 1, vjust= 0, size= 5,
	       label= "Source: Euromoney FX surveys", colour= m$p$clr$drk_gry) +
      annotate( geom= "segment", x= 2009.5, xend= 2019, y= 0, yend= 0, colour= m$p$clr$lght_lght_gry) +
      annotate( geom= "text", x= 2010:2019, y= -0.002, label= 2010:2019,
	       size= 5) +
      annotate( geom= "segment", x= 2009.5, xend= 2009.5, y= 0, yend= 0.19, colour= m$p$clr$lght_lght_gry) +
      annotate( geom= "text", x= 2009, y= seq( 0.0, 0.175, 0.025), label= seq( 0.0, 0.175, 0.025),
	       size= 5, hjust= 1) +
      # commentary
      geom_label( data= m$o$cmnts[ frm== j],
	     mapping= aes( x= x, y= y, label= cmnt, fill= I( clr)),
	     alpha= 1.0, colour= "white", size= 6, fontface= "bold") +
      scale_x_continuous( limits= c( 2008, 2025), breaks= 2010:2019, expand= expansion(mult = 0, add = 0)) +
      scale_y_continuous( limits= c( -0.01, 0.2), expand= expansion(mult = 0, add = 0)) +
#      theme_grey())
      theme_void())
#    geom_label( data= function( x) x[ n==0],
#	      mapping= aes( x= crncy_x + x, y= -crncy_y -y- m$p$unt_sz, label= round( vl, 0)),
#	      alpha= 0.8, size= 10, label.size= unit( 0, "mm"), label.padding= unit( 0.1, "lines"),
#	      hjust= 0, vjust= 0.5 ) +
}}
, movie.name = "Whos_who.gif", ani.width = 500, ani.height = 800)
#, movie.name = "Whats_what.mp4", ani.width = 750, ani.height = 1200)
```

<!--more-->

In my last post, I described what the main

-   Herfindahl Index?

Concentration within the FX market

-   Rise of non-Bank liquidity providers