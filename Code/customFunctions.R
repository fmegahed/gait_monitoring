###########################
######## Angles
# https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
angle <- function(x,y){
  dot.prod = x%*%y
  norm.x = norm(x,type="2")
  norm.y = norm(y,type="2")
  theta = acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

####### Bootstrap sampling
#http://users.stat.umn.edu/~helwig/notes/boot-Notes.pdf
boot.sampling <- function(x, B){
  x = as.matrix(x)
  n = nrow(x)
  bootSamples = replicate(B, x[sample.int(n, replace=TRUE),] )
}

# http://users.stat.umn.edu/~helwig/notes/boot-Notes.pdf
#https://web.as.uky.edu/statistics/users/pbreheny/764-F11/notes/12-6.pdf
var.boot <- function(x,ind){var(x[ind])}