vMask.method1 <-
function( 
 data,    #Type of inputed data must be a matrix with dim=c(m,n)
 mu0 = mean(data),    #Target value for process mean, with defult  mean(data)
 d,    #The distance between ...
 theta = 14,     #The angle (in degrees) between ... 
 sleep = 1    #Sleep time (in secound)
 ){

  if( class(data) == "matrix" ){
      m = dim(data)[1]#The size of samples
      n = dim(data)[2] #The size of each subsample
      RowMeans <- rowMeans(data)
  }

  if( class(data) == "numeric" | class(data) == "integer" ){
      m = length(data)#The size of samples
      n = 1 #The size of each subsample
      RowMeans <- data
  }

  theta <- deg2rad(theta)
  h = d * tan(theta)#The length of decision making
  c = cumsum( RowMeans - mu0 )

  ### For plotting the mask window
  x.win = c(0, m+d+5)
  y.win = c(min(c)-h*(m-1+d)/d, max(c)+h*(m-1+d)/d)

  BackColor = rep(1,m)  #For bg vector

  for(i in 1:m) {
  plot(x.win, y.win, type="n", #For plotting the window
   xlab = "i", ylab = expression(c[i]),
    main = list(paste("CUSUM control chart and V-Mask on point (", i,  ", c", i, ")")) )

lines(1:m, c, type="b", col=1, cex=1.3 , pch=21, bg=BackColor )

points(i, c[i], type="p", cex=2 ,col=1, pch=21, bg=7)

x.Mask <- c(i+d, 
1, 
i+d+4,
i+d+4,
1 )

y.Mask <- c(c[i], 
c[i]+h*(i-1+d)/d, 
c[i]+h*(i-1+d)/d, 
c[i]-h*(i-1+d)/d, 
c[i]-h*(i-1+d)/d )

polygon( x.Mask, y.Mask,#Plotting the mask
   border=2, col="coral1", lwd=1,
   density=c(10, 20), angle=c(-45, 45) )

lines( c(i,i+d), c(c[i],c[i]), col="gray60", lty=2 )
lines( c(i,i), c(c[i]+h,c[i]-h), col="gray60", lty=2 )
#abline( h=mean(c), col=2, lty=3 )

for(j in 1:i){

LCL <- c[i] - h*(i-j+d)/d
UCL <- c[i] + h*(i-j+d)/d
#points( c(i,i), c(LCL,UCL), col="red" )

if( !(LCL<=c[j] & c[j]<=UCL) ){
for(jj in 1:i){
  LCL <- c[i] - h*(i-jj+d)/d
  UCL <- c[i] + h*(i-jj+d)/d
  if( !(LCL<=c[jj] & c[jj]<=UCL) )
points(jj, c[jj], type="p", cex=1.6 ,col=1, pch=21, bg="turquoise1")
  }
BackColor[i] = 2
break
} else{
 BackColor[i] = "green3"
 }
}

if(sleep == "PressEnter"){
cat ("Press [enter] in 'R Console' to continue")
line <- readline()
}else { Sys.sleep(sleep) }  # Sleeping time for computer
}

  return( list( 
               h = h,
               c = c, 
               OutControl = which(BackColor==2), #Out-Control subsamples
               InControl = which(BackColor=="green3")  #In-Control subsamples
        )  
   )
}
