thin.covariate <- function(n.thin,variable)
{
  n = dim(variable)[1]
  m = dim(variable)[2]
  var.thin <- sapply(seq(1,n,by=n.thin),
              function(i) if( i < n){
                  colSums(variable[i:min(i+n.thin,n),])
                } else {
                  variable[i,]
                })
  var.thin = sapply(seq(1,m,by=n.thin),
                function(i) if(i<m){
                  colSums(var.thin[i:min(i+n.thin,m),])
                } else {
                  var.thin[i,]
                })
  var.thin <- t(apply(var.thin,1,rev)/n.thin^2)
  return(var.thin)
}

load("data.RData")

n.thin = 10
lat.g <- grid.data$lat[seq(1,length(grid.data$lat),by=n.thin)]
lon.g <- grid.data$lon[seq(1,length(grid.data$lon),by=n.thin)]
elev.g      <- thin.covariate(n.thin,grid.data$elev)
west.dist.g <- thin.covariate(n.thin,grid.data$dist.west.coast)
east.dist.g <- thin.covariate(n.thin,grid.data$dist.east.coast)

str(lat.g)
str(lon.g)
str(elev.g)