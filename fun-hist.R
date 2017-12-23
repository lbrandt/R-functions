##################################################################################
#                                                                                #
#   fun.hist                                                                     #
#   Histogram                                                                    #
#                                                                                #
#   PC Tutorial Advanced Statistics 3                                            #
#   Winter 2016/2017                                                             #
#                                                                                #
##################################################################################


# Description



fun.hist = function(x,a,b,jnum){
  
  # Calculate bin width h
  h = (b-a)/jnum
  n = length(x)
  f = rep(0,times=n)
  
  for(j in 1:jnum){
    
    # Interval borders
    int.l = a + (j-1)*h
    int.u = a + j*h
    
    # Count obs in bin
    count = sum((x>=int.l)&(x<int.u))
    
    # Assign values to density estimator f
    for(i in 1:n){
      if((x[i]>=int.l)&(x[i]<int.u)){f[i] = count/(n*h)}
    }
    
    
  }
  return(f)
}





