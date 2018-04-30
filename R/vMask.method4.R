vMask.method4 <-
function( 
 data,#Type of inputed data must be a matrix with dim=c(m,n)
 mu0 = mean(data),#Target value for process mean, with defult  mean(data)
 k = 0.5*sd(data),#with defult 0.5*sigma  and it is depends on hypothesis the althernative hypothesis H1: mu = mu1 (mu1=mu0+k*sigma) 
 h = 5*sd(data),        #with defult  5*sigma  and it is "The length of decision making" and usully is in [4.5*sigma, 5*sigma].
 w = 2*sd(data),        #The ration of vertical unit into horisonal unit in CUSUM plot, with defult  2*sigma (this defoult couses theta is approximately 14 degrees).
 sleep = 1#Sleep time (in secound)
 ){

  d = h/k
  theta = atan( k/w )
  theta = rad2deg(theta)

  result <- vMask.method1(data, mu0, d, theta, sleep)

  return( list( 
               d = d, 
               theta = theta, 
               h = result$h, 
               c = result$c, 
               OutControl = result$OutControl,  #Out-Control subsamples
               InControl = result$InControl    #In-Control subsamples
        )  
   )

}
