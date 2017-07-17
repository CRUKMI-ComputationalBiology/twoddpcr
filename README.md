# twoddpcr

[twoddpcr](https://bioconductor.org/packages/twoddpcr/) is an R/Bioconductor 
package for Droplet Digital PCR (ddPCR) analysis. The package is named for its 
ability to classify two channel ddPCR data (the amalgamation of "2-d" and 
"ddPCR").
                
Example usage of the package with a sample dataset is included in the package's 
[vignette](https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html), 
where the Appendix also explains how to extract raw droplet data from Bio-Rad's 
QuantaSoft.


## Installing the `twoddpcr` package

The package can be installed from Bioconductor using:

```
source("https://bioconductor.org/biocLite.R")
biocLite("twoddpcr")
```

Alternatively, it can be installed from GitHub using:

```
library(devtools)
install_github("CRUKMI-ComputationalBiology/twoddpcr")
```

Another alternative is to install the package from source:

```
install.packages("</path/to/twoddpcr/>", repos=NULL, type="source")
```


## Loading the `twoddpcr` package

Once the package has been installed, it can be loaded in the usual way:

```
library(twoddpcr)
```


## `ddpcrPlate` objects

The package revolves around the use of `ddpcrPlate` objects, which stores 
droplet amplitude data for the wells in the plate. The `ddpcrPlate` object also 
keeps track of the various classifications of the droplets.

To use your own droplet data and store it in a variable named `plate`, use:

```
plate <- ddpcrPlate("amplitudes/")
```

Here, we have assumed that your droplet amplitude CSVs are stored in the 
`amplitudes` directory inside the working directory.


## Classifying using the k-means clustering

If your data has four distinct clusters, the k-means clustering algorithm can 
be used:

```
plate <- kmeansClassify(plate)
```

This method can also take the `centres` parameter as a matrix to adjust the 
approximate centres of each cluster. This parameter can also be the number of 
clusters instead of a matrix. See `?kmeansClassify` for more details.


## Plotting classifications

Once the droplets have been classified, they can be plotted using the 
`dropletPlot` method. To use this, the names of the available classification 
methods can be obtained by:

```
getCommonClassMethod(plate)
```

There should be a `kmeans` entry for k-means clustering. To plot this, use:

```
dropletPlot(plate, cMethod="kmeans")
```


## Adding "rain"

Droplets between the main clusters of droplets will possibly have ambiguous 
classifications. These droplets are colloquially known as "rain". To do this, 
use:

```
plate <- mahalanobisRain(plate, maxDistances=list(NN=35, NP=30, PN=30, PP=30))
```

Some trial-and-error is required for setting the `maxDistances` parameter.


## Summaries

Since ddPCR samples are partitioned into droplets, the molecules are assumed to 
be Poisson distributed. With this, the actual number of starting fragments can 
be estimated:

```
plateSummary(plate, cMethod="kmeansMahRain")
```


## Shiny-based GUI for non-R users

A Shiny app is included in the package, which provides a GUI that allows 
interactive use of the package for ddPCR analysis. This can be run from an 
interactive R session using:

```
shinyVisApp()
```

It can also be accessed at 
[http://shiny.cruk.manchester.ac.uk/twoddpcr/](http://shiny.cruk.manchester.ac.uk/twoddpcr/).

If you wish to run the app from your own server, instructions are given in the 
[vignette](https://bioconductor.org/packages/devel/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#shiny-based-gui-for-non-r-users).


# Citing `twoddpcr`

If you use the `twoddpcr` package in your work, please cite the [Bioinformatics 
paper](http://dx.doi.org/10.1093/bioinformatics/btx308). The citation can be 
obtained within R using:

```
citation("twoddpcr")
```

