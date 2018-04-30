vMask.method3 <-
function( 
 data,    #Type of inputed data must be a matrix with dim=c(m,n)
 mu0 = mean(data),    #Target value for process mean, with defult  mean(data)
 k = 0.5*sd(data), #with defult 0.5*sigma  and it is depends on hypothesis the althernative hypothesis H1: mu = mu1 (mu1=mu0+k*sigma) 
 alpha,    #The probability of type I error in testing hypotheses  H0: mu = mu0  vs. H1: mu = mu0 + k*sigma 
 beta = 0.001,    #The probability of type II error in testing hypotheses  H0: mu = mu0  vs. H1: mu = mu0 + k*sigma (its defult is the very small value 0.001)
 sleep = 1    #Sleep time (in secound)
 ){

  d = (-2/k^2) * log(alpha/(1-beta))
  theta = atan(k*sd(data)/2)
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
