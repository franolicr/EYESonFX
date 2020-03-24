grid.newpage()

grid.polyline( x= c(.2, .3, .4, .6, .7, .8), y= c(.7, .5, .7, .3, .5, .3),
              id=rep(1:2, each=3), gp=gpar(lwd=5), name="polyline")

polylineY <- animUnit(unit(c(.7, -.5, .7, .3, .5, .3, .3, .5, .3, .7, .5, .7, .7, .5, .7, .3, .5, .3),
                           unit="npc"), timeid=rep(1:3, each=6), id=rep(rep(1:2, each=3), 3))

polylinegp <- animValue( c( 7, 5, 7, 3, 5, 3, 3, 5, 3, 7, 5, 7, 7, 5, 7, 3, 5, 3),
                        timeid=rep(1:3, each=6), id=rep(rep(1:2, each=3), 3))


grid.circle(.1, .5, r=.1, gp=gpar(fill="white", lwd= 5), name="circle")
grid.animate("circle",  fill= c( "red", "white"), x= c( .1, .9), size= c( 0,5), rep=TRUE)
grid.export("animPolyline.svg")

