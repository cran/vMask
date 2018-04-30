\name{vMask.method4}
\alias{vMask.method4}
\title{
Method 4 for V-Mask Implementation: Using \eqn{\mu0}, \eqn{k}, \eqn{h} and \eqn{w}
}
\description{
The V-mask method in \code{vMask} Package, is considered on the basis of variety of situations/information by different functions. 
One of these functions is \code{vMask.method1} which can plot the V-mask based on \eqn{\mu_0}, \eqn{k} and \eqn{h} and \eqn{w}.
}
\usage{
vMask.method4(data, mu0 = mean(data), k = 0.5 * sd(data), h = 5 * sd(data), 
w = 2 * sd(data), sleep = 1)
}
\arguments{
  \item{data}{
Type of inputted data must be a matrix with \eqn{dim=c(m,n)}, in which \eqn{m} is size of samples and \eqn{n} is size of each subsample. Meanwhile, data can be a numerical vector where its elements are sub-sample means.
}
  \item{mu0}{
The target value for process mean, with default  \code{mean(data)}.
}
  \item{k}{
With default \eqn{0.5 \sigma}  and it depends on the form of the alternative hypothesis \eqn{H_1:  \mu = \mu_1 \  (\mu_1=\mu_0+k \sigma)}. 
}
  \item{h}{
The length of decision making (\eqn{h}) is equal to the vertical distance between the origin and the upper (or lower) arm of V-mask. Its default is \eqn{h = 5  sd(data)}, since usually \eqn{h \in [4.5 \sigma, 5 \sigma]} is proposed.
}
  \item{w}{
The ration of vertical unit into horizontal unit in CUSUM plot, with default  \eqn{w = 2 \times sd(data)} (this default causes \eqn{\theta} to be approximately equal to 14 degrees).
}
  \item{sleep}{
Sleeping time (in second) of the program between showing figures. This time needs for see the result of checking \eqn{i}-th point on CUSUM control chart by V-mask. Also, if \code{sleep="PressEnter"}, then the user must press key \code{[Enter]} in \code{'R Console'} window to continue/check the next cumulative summation point on chart with V-mask. 
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
\item{ d }{The distance between two following points: (1) \eqn{O}, i.e. the latest cumulative sum point (which duty of the V-mask is checking this point), and (2) \eqn{P}, i.e. the junction points of V-mask arms.}
\item{ theta }{half of the angle formed by the V-mask arms (in degrees).}
\item{ h }{the vertical distance between the origin and the upper (or lower) arm of V-mask.}
\item{ c }{A vector of the cumulative differences between statistic (mean) values and the mean target value; i.e.  \eqn{c=(c_1, ..., c_m)} where \eqn{c_i=\sum_{j=1}^{i} (\bar{x}_j - \mu_0) }.}
\item{ OutControl }{The number of Out-of-control points in CUSUM chart.}
\item{ InControl }{The number of In-control points in CUSUM chart.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}

\examples{
m = 35	#The size of samples
n = 2 	#The size of each subsample
set.seed(1234)
Data = matrix(rnorm(m*n, 0,1), nrow=m)
head(Data)

vMask.method4( data=Data, s=0 )
vMask.method4( data=Data, k=.3, h=2.5, w=1, s=0 )
vMask.method4( data=Data, k=.3, h=2.5, w=2, s=0 ) #Compare with the previous line
}

\keyword{ CUSUM Control Chart }
\keyword{ V-mask }

\seealso{
vMask.method3
}
