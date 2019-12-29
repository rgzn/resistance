
library("gdistance")
set.seed(9999999)
r = raster(ncol=3, nrow=3)
r[] = 1:ncell(r)
r

plot(r, main="r", xlab="Longitude (degrees)", ylab="Latitude (degrees)")
text(r)

r[] = 1
tr1 = transition(r, transitionFunction = mean, directions = 8)

tr1

r[] = runif(9)
ncf = function(x) { max(x) -x[1] + x[2]}
tr2 = transition(r, ncf, 4, symm=FALSE)
tr2

tr3 = sqrt(tr1)
tr3[cbind(1:9, 1:9)] <- tr2[cbind(1:9, 1:9)]
tr3[1:9, 1:9] <- tr2[1:9, 1:9]
tr3[1:5, 1:5]


plot(raster(tr3), main="raster(tr3)", xlab="Longitude (degrees)", ylab="Latitude (degrees)")

tr1C <- geoCorrection(tr1, type="c")
tr2C <- geoCorrection(tr2, type="c")


r3 <- raster(ncol=18, nrow=9)
r3 <- setValues(r3, runif(18*9)+5)
tr3 <- transition(r3, mean, 4)
tr3C <- geoCorrection(tr3, type="c", multpl=FALSE, scl=TRUE)
tr3R <- geoCorrection(tr3, type="r", multpl=FALSE, scl=TRUE)


sP <- cbind(c(-100, -100, 100), c(50, -50, 50))
costDistance(tr3C, sP)
commuteDistance(tr3R, sP)
rSPDistance(tr3R, sP, sP, theta=1e-12, totalNet="total")
