\name{vMask.method5}
\alias{vMask.method5}
\title{
Method 5 for V-Mask Implementation: Using \eqn{\mu0}, \eqn{\mu1}, \eqn{\sigma}, \eqn{\alpha} and \eqn{\beta}
}
\description{
The V-mask method in \code{vMask} Package, is considered on the basis of variety of situations/information by different functions. 
One of these functions is \code{vMask.method1} which can plot the V-mask based on \eqn{\mu_0}, \eqn{\mu_1}, \eqn{\sigma} and the probabilities of type I and II errors of testing null hypotheses  \eqn{H_0: \mu = \mu_0}  vs. alternative hypothesis \eqn{H_1: \mu = \mu_0 + k \sigma}.
}
\usage{
vMask.method5(data, mu0 = mean(data), mu1, sigma = sd(data), alpha, beta = 0.001,
 sleep = 1)
}
\arguments{
  \item{data}{
Type of inputted data must be a matrix with \eqn{dim=c(m,n)}, in which \eqn{m} is size of samples and \eqn{n} is size of each subsample. Meanwhile, data can be a numerical vector where its elements are sub-sample means.
}
  \item{mu0}{
The target value for process mean, with default  \code{mean(data)}.
}
  \item{mu1}{
The mean under the alternative hypothesis, i.e., \eqn{\mu_1 = \mu_0 + k \sigma}.
}
  \item{sigma}{
The standard deviation of the process, with default  \code{sd(data)}.
}
  \item{alpha}{
The probability of type I error in testing hypotheses  \eqn{H_0: \mu = \mu_0}  vs.  \eqn{H_1: \mu = \mu_0 + k \sigma}. 
}
  \item{beta}{
The probability of type II error in testing hypotheses  \eqn{H_0: \mu = \mu_0}  vs.  \eqn{H_1: \mu = \mu_0 + k \sigma}. The default of \code{beta} is the very small probability value \code{0.001}.
}
  \item{sleep}{
Sleeping time (in second) of the program between showing figures. This time needs for see the result of checking \eqn{i}-th point on CUSUM control chart by V-mask. Also, if \code{sleep="PressEnter"}, then the user must press key \code{[Enter]} in \code{'R Console'} window to continue/check the next cumulative summation point on chart with V-mask. 
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
\item{ d }{The distance between two points: (1) \eqn{O}, i.e the latest cumulative sum point (which duty of the V-mask is checking this point), and (2) \eqn{P}, i.e. the junction points of V-mask arms.}
\item{ k }{Difference/shift in the mean of process after a probably change. Its default is considered \eqn{0.5 \sigma} and it depends on the form of the alternative hypothesis \eqn{H_1: \mu = \mu_1 \ (\mu_1=\mu_0+k \sigma)}.}
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
m = 26	#The size of samples
n = 2 	#The size of each subsample
set.seed(2345)
Data = matrix(rnorm(m*n, 0,1), nrow=m)
head(Data)

vMask.method5( data=Data, mu0=0, mu1=1,  alpha=.1, beta=.01, sl=0 )
vMask.method5( data=Data, mu0=0, mu1=.5, alpha=.1, beta=.01, sl=0 )
}

\keyword{ CUSUM Control Chart }
\keyword{ V-mask }

\seealso{
vMask.method3, vMask.method6
}
