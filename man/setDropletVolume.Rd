% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{setDropletVolume}
\alias{setDropletVolume}
\alias{setDropVolume}
\alias{setDropletVol}
\alias{setDropVol}
\title{Set the droplet volume in microlitres (ul).}
\usage{
setDropletVolume(volume = 0.00085)
}
\arguments{
\item{volume}{The new volume of each droplet. Defaults to 0.00085, which is
the default given by Bio-Rad.}
}
\value{
Sets the internal droplet volume.
}
\description{
Change the volume of droplets used in the ddPCR experiment.
}
\examples{
## Change the droplet volume.
ddpcr$dropletVolume
setDropletVolume(0.00091)
ddpcr$dropletVolume

}
\author{
Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
}
