##################################################################################
#                                                                                #
#   fun.ecdf                                                                     #
#   Empirical CDF                                                                #
#                                                                                #
#   PC Tutorial Advanced Statistics 3                                            #
#   Winter 2016/2017                                                             #
#                                                                                #
##################################################################################


# This function computes an Empirical Distribution function from a
# given data vector and plots the result. Additionally, it returns 
# the vector of cumulative relative frequencies.


fun.ecdf = function(x){
  n = length(x)
  x.sort = sort(x)
  f = rep(0, times=n)
  f[1] = 1/n
  
  for(i in 2:n){
    f[i] = f[i-1] + (1/n)
  }
  plot(x.sort,f,type="l",lwd=2,main = "Empirical distribution",xlab="Data",ylab="ECDF")
  return(f)
}



















