\name{vMask.method2}
\alias{vMask.method2}
\title{
Method 2 for V-Mask Implementation: Using \eqn{\mu0}, \eqn{d} and \eqn{h}
}
\description{
The V-mask method in \code{vMask} Package, is considered on the basis of variety of situations/information by different functions. 
One of these functions is \code{vMask.method1} which can plot the V-mask based on \eqn{\mu_0}, \eqn{d} and \eqn{h}.
}
\usage{
vMask.method2(data, mu0 = mean(data), d, h = 5 * sd(data), sleep = 1)
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
  \item{h}{
The vertical distance between the origin and the upper (or lower) arm of V-mask.
}
  \item{sleep}{
Sleeping time (in second) of the program between showing figures. This time needs for see the result of checking \eqn{i}-th point on CUSUM control chart by V-mask. Also, if \code{sleep="PressEnter"}, then the user must press key \code{[Enter]} in \code{'R Console'} window to continue/check the next cumulative summation point on chart with V-mask. 
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
\item{ theta }{half of the angle formed by the V-mask arms (in degrees).}
\item{ c }{A vector of the cumulative differences between statistic (mean) values and the mean target value; i.e.  \eqn{c=(c_1, ..., c_m)} where \eqn{c_i=\sum_{j=1}^{i} (\bar{x}_j - \mu_0) }.}
\item{ OutControl }{The number of Out-of-control points in CUSUM chart.}
\item{ InControl }{The number of In-control points in CUSUM chart.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}

\examples{
### Example 1:
m = 15	#The size of samples
n = 4 	#The size of each subsample
set.seed(100)
Data = matrix(rnorm(m*n, 2,5), nrow=m)
head(Data)

vMask.method2( data=Data, d=1, h=1, s=0 )
( res <- vMask.method2( data=Data, mu0=2, d=2, h=2, sl=0) )
Data[res$Out,]	#Out of control subsamples


### Example 2:
vMask.method2( data=Data, d=1, h=1, s=0 )
vMask.method1( data=Data, d=1, theta=45, s=0 )  #Similar results
}

\keyword{ CUSUM Control Chart }
\keyword{ V-mask }

\seealso{
vMask.method1
}
