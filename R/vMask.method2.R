vMask.method2 <-
function( 
 data,    #Type of inputed data must be a matrix with dim=c(m,n)
 mu0 = mean(data),    #Target value for process mean, with defult  mean(data)
 d,    #The distance between ...
 h = 5*sd(data),   #The length of decision making with defult  5*sigma
 sleep = 1    #Sleep time (in secound)
 ){

  theta = atan(h/d)#in radians and not in degrees
  theta = rad2deg(theta)

  result <- vMask.method1(data, mu0, d, theta, sleep)

  return( list( 
#              d = d, 
               theta = theta, 
               c = result$c, 
               OutControl = result$OutControl,  #Out-Control subsamples
               InControl = result$InControl    #In-Control subsamples
        )  
   )

}
