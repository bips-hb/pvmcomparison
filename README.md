# PharmacoVigilance Comparison Study

This repository contains all the `R` code for a comparison study of 27 statistical methods in their ability 
to detect associations between drugs and adverse events in spontaneous reporting data. The code heavily relies
on the following two `R` packages: 

* `SRSim`, see www.github.com/bips-hb/srsim, and 
* `PVM`, see www.github.com/bips-hb/pvm.  

The code is published under the GPL-3 license. 

### Shiny 

All the results can be explored interactively at www.srs.bips.eu.

### Installation
 
To install, simply type in `R`

```R
devtools::install_github("bips-hb/pvmcomparison")
```

### Usage

In order to generate all the results as presented in the paper and at www.srs.bips.eu, install the package 
and run the `run.R` script: 

```R
source("run.R")
```

### References

Please cite 

__Adverse Event Discovery for Spontaneous Reporting Systems: A Systematic Comparison__\
*L.J. Dijkstra, M. Garling, R. Foraita & I. Pigeot*\
To be submitted (2018)

### Contact

Louis Dijkstra\
Leibniz Institute for Prevention Research & Epidemiology  
E-mail: dijkstra (at) leibniz-bips.de
