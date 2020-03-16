+++
title = "What's what?"
author = ["Robert Franolic"]
publishDate = 2020-03-01T00:00:00+00:00
tags = ["tag1", "viz"]
categories = ["What"]
draft = false
+++

Five key facts about the currencies traded in the FX market using data from the BIS surveys.

{{< figure src="/ox-hugo/Whats_what.gif" >}}

```R
m$i$bis <- fread( "/Dnk/Srcs/BIS/Out/Crcny_Prs.csv")

m$i$bis[ , vl:= vl/1e3]
# Unit size (in billions)
m$p$unt_sz <- 5

# Identify pre-Euro currencies, as those not present in 2019
m$p$eur_crncys <- m$i$bis[ !crncy1 %in% m$i$bis[ yr== 2019, unique( crncy1)],
			.( yr= max( yr)),
			.( crncy= crncy1)]
m$i$bis[ crncy1 %in% m$p$eur_crncys$crncy, crncy1 := 'EUR']
m$i$bis[ crncy2 %in% m$p$eur_crncys$crncy, crncy2 := 'EUR']

m$i$bis <- m$i$bis[ crncy1!= "USD" & ( crncy1!= "EUR" | ( crncy1== "EUR" & crncy2 %in% c( "other", "USD"))) &
       ( crncy1!= "other" | ( crncy1== "other" & crncy2 %in% c( "other", "USD"))) &
      crncy2 %in% c( "USD", "EUR", "other")]

# Identify small currencies (less that unit size on average) and merge into "other"
m$p$sml_crncy <- m$i$bis[ , .( vl= sum( vl)), .( yr, crncy1)][ , .( vl= mean( vl)), crncy1][ vl <= m$p$unt_sz, crncy1]
m$i$bis[ crncy1 %in% m$p$sml_crncy, crncy1:= "other"]

m$i$bis[ crncy1== "other" & crncy2== "other", vl:= vl/ 2]

# aggregate (for EUR and countercurrency changes) and convert to billions
m$x$bis <- m$i$bis[ , .( vl= sum( vl)), .( yr, crncy1, crncy2)]



# reorder currencies based on value (but always put "other" last)
m$x$bis[ , crncy2 := factor( crncy2, levels= c( "USD", "EUR", "other"))]
m$x$bis[ , crncy1 := factor( crncy1,
			  c( m$x$bis[ yr== 2019 & crncy1 != "other",
				     .( vl= sum( vl)), crncy1][ order( -vl), crncy1],
			    "other"))]

m$x$bis <- m$x$bis[ order( crncy1, crncy2, yr)]

# Work out x position of each counter-currency - using highest max value across the years
m$o$crncy_x <- m$x$bis[ , .( wdth= 2* sqrt( sum( vl))), .( yr, crncy2)][
		   order( crncy2), .( wdth= max( wdth)), .( crncy2)]
m$o$crncy_x[ , crncy_x:= as.numeric( crncy2)* m$p$unt_sz + cumsum( wdth)- wdth]
setkeyv( m$o$crncy_x, "crncy2")

# Work out y position of each currency
m$o$crncy_y <- m$x$bis[ , .( mx_vl= max( vl), vl, crncy2), .( crncy1)][ vl== mx_vl, .( crncy1, crncy2, mx_vl)]
m$o$crncy_y <- m$o$crncy_y[ m$o$crncy_x, .( crncy1, hght= ( mx_vl/ ( wdth/ m$p$unt_sz))), on= "crncy2"]
m$o$crncy_y[ hght < 2.1 * m$p$unt_sz, hght:= 2.1 * m$p$unt_sz]
m$o$crncy_y[ order( crncy1), crncy_y:= cumsum( 1.5 * m$p$unt_sz + hght)- hght]
setkeyv( m$o$crncy_y, "crncy1")


# Create units of 1 billion
m$i$unts <- data.table( n= seq( 0, 1590, by= m$p$unt_sz), dmy= 1)
# dummy needed to allow cross join
m$x$bis[ , dmy:= 1]
# Disaggregate to units
m$o$bis <- merge( m$x$bis, m$i$unts, allow.cartesian= TRUE, by= "dmy")
m$o$bis[ , prsnt:= round( vl, 0) >= n] # + m$p$unt_sz/ 2]
# Remove unnnecessary rows - n is greater than the maxium n present
m$o$bis <- m$o$bis[ , .( yr, mx= max( n* prsnt), vl, n, prsnt), .( crncy1, crncy2)][ n <= mx]

# Identify gains and losses
m$o$bis[ order( crncy1, crncy2, n, yr), prvs_prsnt:= shift( prsnt), .( crncy1, crncy2, n)]
m$o$bis[ prsnt== TRUE & is.na( prvs_prsnt), prvs_prsnt:= TRUE] # treat new pair as if they were present before
m$o$bis <- m$o$bis[ prsnt== TRUE | prvs_prsnt== TRUE,
      sgn:= as.numeric( prsnt)- as.numeric( prvs_prsnt)]


# Repeat each year three times: first time to highlight losses, next to highlight gains and last the final position
m$o$bis <- m$o$bis[ rep( 1:.N, each= 3), cbind( i= 1:3, .SD)][ order( yr, sgn),
							    cbind( frm= i + 3* ( .GRP- 1), .SD), yr]

m$o$bis[ , clr:= fifelse( sgn== -1, m$p$clr$rd, fifelse( sgn== 0, m$p$clr$drk_gry, m$p$clr$grn))]

m$o$bis <- rbindlist( list(
    m$o$bis[ i== 1 & sgn != 1],
    m$o$bis[ i== 2 & sgn != -1],
    m$o$bis[ i== 3 & sgn != -1][, clr:= m$p$clr$drk_gry]))

setkeyv( m$o$bis, "crncy2")
m$o$bis <- m$o$bis[ m$o$crncy_x]
setkeyv( m$o$bis, "crncy1")
m$o$bis <- m$o$bis[ m$o$crncy_y]

# Position units
m$o$bis[ , x:= n %% ( m$p$unt_sz * round( wdth / m$p$unt_sz, 0))]
m$o$bis[ , y:= n %/% ( m$p$unt_sz * round( wdth / m$p$unt_sz, 0)) * m$p$unt_sz ]

# Commentary
m$o$cmnts <- list(
    3, "black", 85, -300, "1. Dollar dominates in 1992 \n- 80% of trades against USD",
    4, "black", 85, -300, "1. Dollar dominates in 1992 \n- 80% of trades against USD",
    5, m$p$clr$grn, 100, -300, "2. Growth through the 1990's \n in many pairs",
    6, m$p$clr$grn, 100, -300, "2. Growth through the 1990's \n in many pairs",
    7, m$p$clr$grn, 100, -300, "2. Growth through the 1990's \n in many pairs",
    8, m$p$clr$grn, 100, -300, "2. Growth through the 1990's \n in many pairs",
    10, m$p$clr$rd, 120, -42, "3. European Monetary Union leads to\nlarge reductions in Euro pairs",
    11, m$p$clr$rd, 120, -42, "3. European Monetary Union leads to\nlarge reductions in Euro pairs",
    12, m$p$clr$rd, 120, -42, "3. European Monetary Union leads to\nlarge reductions in Euro pairs",
    11, m$p$clr$grn, 115, -145, "3. ... but there is\ngrowth in several\nother pairs",
    12, m$p$clr$grn, 115, -145, "3. ... but there is\ngrowth in several\nother pairs",
    14, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    15, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    16, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    17, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    18, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    19, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    20, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    21, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    22, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    23, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    24, m$p$clr$grn, 90, -500, "4. Strong growth in many \n pairs in 2004 thru 2013",
    25, m$p$clr$rd, 210, -35, "5. Many large pairs\ndecline in 2016",
    26, m$p$clr$rd, 210, -35, "5. Many large pairs\ndecline in 2016",
    27, m$p$clr$rd, 210, -35, "5. Many large pairs\ndecline in 2016",
    26, m$p$clr$grn, 90, -470, "5. ... but Chinese Yuan \n(CNY) and many Asian\npairs continue to grow",
    27, m$p$clr$grn, 90, -470, "5. ... but Chinese Yuan \n(CNY) and many Asian\npairs continue to grow",
    28, m$p$clr$grn, 100, -500, "6. Return to growth in\n2019 for most but JPY",
    29, m$p$clr$grn, 100, -500, "6. Return to growth in\n2019 for most but JPY",
    30, m$p$clr$grn, 100, -500, "6. Return to growth in\n2019 for most but JPY",
    30, "black", 92, -600, "7. Dollar still dominates in\n2019 as it did in 1992")


m$o$cmnts <- as.data.table( matrix( unlist( m$o$cmnts), ncol= 5, byrow= TRUE,
				 dimnames= list( NULL, c( "frm", "clr", "x", "y", "cmnt"))))
m$o$cmnts[ , `:=`( x= as.numeric( x), y= as.numeric( y))]

# Years
m$o$yrs <- m$o$bis[ !is.na( yr), .N, yr]

# Let's plot it!
saveGIF({ ani.options(interval = 2, nmax = 75)
#saveVideo({ ani.options(interval = 1, nmax = 75)

#for( j in m$o$bis[ yr== 2016  & yr!= 2022 & !( yr== 1992 & i < 3), .N, frm][ order( frm), frm]){
for( j in m$o$bis[ yr!= 2022 & !( yr== 1992 & i < 3), .N, frm][ order( frm), frm]){
    print(
      ggplot( data= m$o$bis[ frm== j]) +
      # years -  background tiles
      geom_tile( data= function( x)
	  x[ , .N, yr][ m$o$yrs, on= "yr"][
	    , .( yr, n= seq_along( yr),
		fll= fifelse( is.na( N), m$p$clr$lght_gry, "white"),
	      clr= fifelse( is.na( N), m$p$clr$drk_gry, "white"))],
	  mapping= aes( x= -36 + n * 22, y= 29, colour= I( clr), fill= I( fll)),
	  width= 22, height= 20) +
      # years - text
      geom_text( data= function( x)
	  x[ , .N, yr][ m$o$yrs, on= "yr"][
	    , .( yr, n= seq_along( yr),
		fll= fifelse( is.na( N), m$p$clr$lght_gry, "white"),
	      clr= fifelse( is.na( N), m$p$clr$drk_gry, "black"))],
	  mapping= aes( x= -36 + n * 22, y= 29, label= yr, colour= I( clr)),
	  size= 5) +
      # logo
      annotation_custom( m$p$eyesonfx_lg, xmin= 198, xmax=260, ymin=22, ymax=40) +
      # vertical lines
      geom_segment( data= m$o$crncy_x,
		    mapping= aes( x= crncy_x - 4, xend= crncy_x - 4,
				 y= 15, yend= -655),
		   colour= m$p$clr$lght_gry, size= 0.5) +
      # horizontal lines
      geom_segment( data= m$o$crncy_y,
		    mapping= aes( x= -25, xend= +Inf,
				 y= -crncy_y + m$p$unt_sz, yend= -crncy_y + m$p$unt_sz),
		   colour= m$p$clr$lght_gry, size= 0.5) +
      # gain/loss label
      geom_label( data= function( x) x[ , .N, .( yr, i)][
				       , .( lbl= ifelse(  i <= 1, "loss",
						 ifelse( i <= 2, "gain", "final")),
					   fll= ifelse(  i <= 1, m$p$clr$rd,
						ifelse( i <= 2, m$p$clr$grn, m$p$clr$drk_gry)))],
		 mapping= aes( x= -13, y= 9, label= lbl, fill= I( fll)),
		 fontface= "bold", colour= "white", size= 6, label.size= unit( 1, "mm"),
		 label.padding= unit( 0.25, "lines"),
		 hjust= 0.5, vjust= 0.5) +
      # currencies - across top
      geom_text( data= function( x)
	  x[ , .( crncy_x= max( crncy_x)+ min( x), sgn= mean( sgn), i= max( i)), .( crncy2)][
	     ,.( crncy_x, clr= fifelse( sgn== 0 | i>= 3, "black", fifelse( sgn< 0, m$p$clr$rd, m$p$clr$grn)),
		fnt= fifelse( mean(sgn)== 0 | i>= 3, "plain", "bold")),
			   .( crncy2)],
	    mapping= aes( x= crncy_x, y= 1, label= crncy2, colour= I( clr), fontface= I( fnt)),
	    hjust= 0, vjust= 0, size= 6) +
      # currencies - down the side
      geom_text( data= function( x)
	  x[ , .( n, mx= max( n), y= crncy_y + min( y) + m$p$unt_sz,
		 clr= fifelse( sgn== 0 | i>= 3, "black", clr),
		 fnt= fifelse( sgn== 0 | i>= 3, "plain", "bold")),
	    .( crncy1)][ n== mx],
	  mapping= aes( y= -y, label= crncy1, colour= I( clr), fontface= I( fnt)),
	  x= -25, hjust= 0, vjust= 0.5, size= 6) +
      # dots
      geom_point( mapping= aes( x= crncy_x + x, y= -crncy_y -y, colour= I( clr)),
		 alpha= 1, size= 2.5) +
      # key dot
      annotate( geom= "point", x= 5, y= -659, size= 2.5, colour= m$p$clr$drk_gry) +
      # key text
      annotate( geom= "text", x= 7, y= -663, hjust= 0, vjust= 0, size= 5,
	       label= " = 5 billion USD") +
      # source
      annotate( geom= "text", x= 259, y= -663, hjust= 1, vjust= 0, size= 5,
	       label= "Source: BIS FX surveys", colour= m$p$clr$drk_gry) +
      # commentary
      geom_label( data= m$o$cmnts[ frm== j],
	     mapping= aes( x= x, y= y, label= cmnt, fill= I( clr)),
	     alpha= 1.0, colour= "white", size= 6, fontface= "bold") +
      scale_x_continuous( limits= c( -26, 260), expand= expansion(mult = 0, add = 0)) +
      scale_y_continuous( limits= c( -665, 40), expand= expansion(mult = 0, add = 0)) +
#      theme_grey())
      theme_void())
#    geom_label( data= function( x) x[ n==0],
#	      mapping= aes( x= crncy_x + x, y= -crncy_y -y- m$p$unt_sz, label= round( vl, 0)),
#	      alpha= 0.8, size= 10, label.size= unit( 0, "mm"), label.padding= unit( 0.1, "lines"),
#	      hjust= 0, vjust= 0.5 ) +
}}
, movie.name = "Whats_what.gif", ani.width = 500, ani.height = 800)
#, movie.name = "Whats_what.mp4", ani.width = 750, ani.height = 1200)
```

<!--more-->

Having introduced the intention of this blog in my last post, I
will start right a way with a description of what currencies are
traded in the FX market using data from the Bank for International
Settlement's triennial survey.

As the animation highlights, since the survey began in 1989 until now,
the US dollar (USD) is by far the most traded currency.

USD is most commonly traded against the EUR, the second most traded
currency. The importance of the Eurozone currency can be seen in the
impact of its go live in January 1999, between the 1998 and 2001 survey.

After USD, referred to as the G7 currencies
The market has grown every period, sa