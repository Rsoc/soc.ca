# Rotate cartesian coordinates around a given center for a given angle
# Can use 2 methods: polar or a transform matrix (both give the same result)
# Can also be used to translate points
# (c)2014 Johan Slotman & Maarten Paul OIC/ErasmusMC
# v 0.1
# v 0.2 - added formal arguments
# v 0.3 - fixed center offset bug 16-7-14
# v 0.4 - addes stretch and flip features 17-7-14
# v 0.5 - added rotate3d 17-7-14

rotate_coord <- function(x,y,angle, type=c("degrees","radial"), method=c("transform","polar","polar_extended"), center=c(0,0), translate=NULL, stretch=NULL, flip=FALSE){
  
  type <- match.arg(type)
  method <- match.arg(method)
  if(!(length(translate)==2 || is.null(translate))){stop("translation coordinates should be a vector of length 2")}
  if(!(is.logical(flip))){stop("Flip should be TRUE or FALSE")}
  
  if(flip){
    x <- -x
  }
  
  
  if(!is.null(stretch)){
    x <- x*stretch
    y <- y*stretch
    center <- center*stretch
    if(!is.null(translate)){translate<- translate*stretch}
  }
  
  
  x <- x-center[1]
  y <- y-center[2]
  
  
  if(type=="degrees"){angle <- angle*pi/180}
  if(type=="radial" && angle>(2*pi)){warning("Angle is bigger than 2pi are you sure it's in rads", call. = F)}
  
  if(method=="polar" || method=="polar_extended"){
    r <-sqrt(x^2+y^2)
    phi <- atan2(x,y)
    new_x <- r*sin(phi+angle)
    new_y <- r*cos(phi+angle)
    xy <- cbind(new_x,new_y)
  }
  
  if(method=="polar_extended"){
    switch(type,
           degrees={phi <- (phi+angle)*180/pi},
           radial={phi <- phi+angle}
    )
    ext_list <- list(Coordinates=xy, Angles=phi, Distance_from_center=r)
    return(invisible(ext_list))
    
  }
  
  
  if(method=="transform"){
    conversionmatrix <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)), ncol=2, nrow=2)
    xy <- cbind(x,y)%*%conversionmatrix
  }
  
  xy[,1] <- xy[,1]+center[1]
  xy[,2] <- xy[,2]+center[2]
  
  if(!is.null(translate)){
    xy[,1] <- xy[,1]+translate[1]
    xy[,2] <- xy[,2]+translate[2]
  }
  
  
  
  return(xy)
}

