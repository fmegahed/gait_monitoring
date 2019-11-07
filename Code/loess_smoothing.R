load("../Data/FeatureGeneration.RData")
for (j in 1:15) {
  s = get(paste0("subject",j,"_features"))
  time = s$time.from.start
  strideLength = s$scaled.stride.len
  strideHeight = s$scaled.stride.height
  strideDuration = s$stride.duration
  
  sp = 0.2
  LL = loess( strideLength ~ time, span = sp )
  yhatLL = LL$fitted
  LH = loess( strideHeight ~ time , span = sp )
  yhatLH = LH$fitted
  LD = loess( strideDuration ~ time , span = sp )
  yhatLD = LD$fitted
  
  plot( strideLength )
  lines( yhatLL , col="red" , lwd=3)
  plot( strideHeight )
  lines( yhatLH , col="blue" , lwd=3)
  m = length(strideHeight)
  cols = rep( "" , m )  
  for ( i in 1:m ) cols[i] = rgb(1-i/m,1-i/m,1-i/m)
  
  k1 = 1
  k2 = m 
  #windows( 12 , 12 )
  par(6.5,6.5)
  pairs(~ yhatLL[k1:k2] + yhatLH[k1:k2] + yhatLD[k1:k2] , col=cols, main = paste0("subject",j,"_LOESS"), cex.lab=2)
  dev.copy(png,paste0("../Figures/subject",j,"_LOESS.png"))
  dev.off()
  
  
  angles <- vector(mode = "numeric", length = (length(yhatLD)-1) )
  for (k in 2:(length(yhatLD)-1)) {
    vec_kMinus1 <- c(yhatLL[k-1], yhatLH[k-1], yhatLD[k-1])
    vec_k <- c(yhatLL[k], yhatLH[k], yhatLD[k])
    vec_kPlus1 <- c(yhatLL[k+1], yhatLH[k+1], yhatLD[k+1])
    
    w1 <- vec_k - vec_kMinus1
    w2 <- vec_kPlus1 - vec_k
    
    angles[k] <- angle(w2,  w1)
  }
  plot(angles, main= paste("Angles for Subject",j))
  dev.copy(png,paste0("../Figures/subject",j,"_Angles.png"))
  dev.off()
}





###########################
######## Angle
# https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
angle <- function(x,y){
  dot.prod <- x%*%y
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}



############################
####  Scratch 
############################
##k1 = 2349
##k2 = 2400
##plot( yhatLL[k1:k2] , yhatLH[k1:k2] , col=cols , xlim=c(1.10,1.40) , ylim=c(0.08,0.11) )
##plot( yhatLL[k1:k2] , yhatLH[k1:k2])

# windows( 7 , 12 )
# par(mfrow=c(3,1))
# plot( yhatLL[k1:k2] , yhatLH[k1:k2] , col=cols , xlim=c(0.8,1.6) , ylim=c(0.06,0.15) )
# ##  points( yhatLL[2349] , yhatLH[2349] , col="red" , pch=18 , cex=2 )
# plot( yhatLL[k1:k2] , yhatLD[k1:k2] , col=cols , xlim=c(0.8,1.6) , ylim=c(0.9,1.2) )
# ##  points( yhatLL[2349] , yhatLD[2349] , col="blue" , pch=18 , cex=2 )
# plot( yhatLH[k1:k2] , yhatLD[k1:k2] , col=cols  , 
#           xlim=c(0.06,0.15) , ylim=c(0.9,1.2) )
# ##  points( yhatLH[2349] , yhatLD[2349] , col="green" , pch=18 , cex=2 )
