vMask.method5 <-
function( 
 data,    #Type of inputed data must be a matrix with dim=c(m,n)
 mu0 = mean(data),    #Target value for process mean, with defult  mean(data)
 mu1,    #The althernative hypothesis H1: mu = mu1 (mu1=mu0+k*sigma) 
 sigma = sd(data),    #Supposed fixed (i.e. the process variation is assumed under control) and its defult is  sd(data)
 alpha,    #The probability of type I error in testing hypotheses  H0: mu = mu0  vs. H1: mu = mu0 + k*sigma 
 beta = 0.001,    #The probability of type II error in testing hypotheses  H0: mu = mu0  vs. H1: mu = mu0 + k*sigma (its defult is the very small value 0.001)
 sleep = 1    #Sleep time (in secound)
 ){

  k = (mu1-mu0) / sigma
  d = (-2/k^2) * log(alpha/(1-beta))
  theta = atan(k*sigma/2)
  theta = rad2deg(theta)

  result <- vMask.method3(data, mu0, k, alpha, beta, sleep)

  return( list( 
               d = d, 
               k = k, 
               theta = theta, 
               h = result$h, 
               c = result$c, 
               OutControl = result$OutControl,  #Out-Control subsamples
               InControl = result$InControl    #In-Control subsamples
        )  
   )

}
