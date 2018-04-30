\name{deg2rad}
\alias{deg2rad}
\title{
Degree Convertor to Radian
}
\description{
This is a simple function for converting degreed into radians.
}
\usage{
deg2rad(deg)
}
\arguments{
  \item{deg}{
A numeric value for angle in degrees.
}
}
\value{
A numeric value for angle in radians.
}

\examples{
sin(30) 	  # -0.409 (!)
sin(deg2rad(30))  # 0.5
}
