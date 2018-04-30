\name{vMask.method1}
\alias{vMask.method1}

\title{
Method 1 for V-Mask Implementation: Using \eqn{\mu0}, \eqn{d} and \eqn{\theta}
}
\description{
The V-mask method in \code{vMask} Package, is considered on the basis of variety of situations/information by different functions. 
One of these functions is \code{vMask.method1} which can plot the V-mask based on \eqn{\mu_0}, \eqn{d} and \eqn{\theta}.
}
\usage{
vMask.method1(data, mu0 = mean(data), d, theta = 14, sleep = 1)
}
\arguments{
  \item{data}{
Type of inputted data must be a matrix with \eqn{dim=c(m,n)}, in which \eqn{m} is size of samples and \eqn{n} is size of each subsample. Meanwhile, data can be a numerical vector where its elements are sub-sample means.
}
  \item{mu0}{
Target value for process mean, with default  \code{mean(data)}.
}
  \item{d}{
Suppose that \eqn{O} is the latest cumulative sum point (which duty of the V-mask is checking this point), and also \eqn{P} is the junction points of V-mask arms. Now, \eqn{h} is the distance between two points \eqn{O} and \eqn{P}.
}
  \item{theta}{
half of the angle formed by the V-mask arms (in degrees).
}
  \item{sleep}{
Sleeping time (in second) of the program between showing figures. This time needs for see the result of checking \eqn{i}-th point on CUSUM control chart by V-mask. Also, if \code{sleep="PressEnter"}, then the user must press key \code{[Enter]} in \code{'R Console'} window to continue/check the next cumulative summation point on chart with V-mask. 
}
}

\details{
The origin point of the V-Mask is placed over/top of the latest cumulative summation point and past points are examined to see if any fall above or below the arms of the V-mask. As long as all the previous points lie between the sides of the V-mask, the process is in control. Otherwise (even if one point lies outside) the process is suspected of being out of control.
}
\value{
\item{ h }{the vertical distance between the origin and the upper (or lower) arm of V-mask.}
\item{ c }{A vector of the cumulative differences between statistic (mean) values and the mean target value; i.e.  \eqn{c=(c_1, ..., c_m)} where \eqn{c_i=\sum_{j=1}^{i} (\bar{x}_j - \mu_0) }.}
\item{ OutControl }{The number of Out-of-control points in CUSUM chart.}
\item{ InControl }{The number of In-control points in CUSUM chart.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}

\examples{
### Example 1: (Data matrix)
m = 10	#The size of samples
n = 3 	#The size of each subsample
set.seed(123)
Data = matrix(rnorm(m*n, 1.5,3), nrow=m)
head(Data, 4)

vMask.method1( data=Data, d=4, theta=30, sleep="PressEnter")
vMask.method1( data=Data, d=4, theta=30, sleep=0)    #Check with sleep=1
vMask.method1( data=Data, d=4.5, theta=30, sleep=0)
vMask.method1( data=Data, mu0=0, d=4.5, theta=45, sl=0)

### Example 2: (Vectoral data) 
set.seed(123)
vMask.method1( data=rnorm(20, 1,2), mu0=0, d=8, theta=45, sl=0)
}

\keyword{ CUSUM Control Chart }
\keyword{ V-mask }

\seealso{
vMask.method2
}
