# PharmacoVigilance Comparison Study

This repository contains all the `R` code for a comparison study of 27 statistical measures in their ability 
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

### Example Simulated Data Set

The file `example-SRS.rds` contains an example of a simulated spontaneous reporting data set. The number of reports is 50,000. There are no innocent bystanders 
and the average odds ratio between drugs and adverse drug reactions (ADRs) are drawn for a truncated normal distribution with mean 1.5. The odds ratios can not be smaller than 1. The object stored in `example-SRS.rds` has six entries: 

* `$sr` - a binary matrix with 50,000 rows and 1,000 columns. Each row represents a report. The first 500 columns represent the drugs. The last 500 columns represent athe adverse drug events (ADEs) and the ADRs. In case that the entry is 1, the report contains that particular drug or that particular ADE/ADR 

* `$prob_drugs` - a vector with 500 entries. Each entry represents the baseline probability of that drug being prescribed when no other drugs are described

* `$prob_events` - a vector with 500 entries. Each entry represents the baseline probability of that ADE/ADR occuring when no other drugs are prescribed  

`$nodes` - a table with a 1,000 rows and 7 columns. Each row represents either a drug or an ADE/ADE. The columns represent: 

* `label` - the label, e.g., `drug7` or `event44` 
* `in_degree` - the number of nodes in the directed acyclic graph (DAG) pointing to that variable (in this setting, either 0 or 1). 
* `id` - an integer ID (1 to 1,000)
* `parent_id` - the ID of the node pointing to this node (1 to 1,000). If no node points to this variable, the value is set to `-1`
* `margprob` - probability of the drug being prescribed when no other drugs are prescribed, or the probability of that ADE/ADR occurs when no drugs are prescribed 
* `beta0` - the intercept of the logistic regression function
* `beta1` - the odds ratio with the parent node 

`$tables` is a data frame with 250,000 rows and contains all 2 x 2 tables for each drug and ADE/ADR pair. It has the following columns: 

* `drug_id` - the ID of the drug
* `event_id` - the ID of the event
* `prob_drug` - the baseline probability of the drug being prescribed when no other drugs are prescribed 
* `prob_event` - the probability of the ADE/ADR occuring when no drugs are prescribed
* `or` - the true odds ratio 
* `a`, `b`, `c` and `d`: the entries of the 2 x 2 table

### References

Please cite 



__Adverse Drug Reaction or Innocent Bystander? A Systematic Comparison of Statistical Discovery Methods for Spontaneous Reporting Systems__\
*L.J. Dijkstra, M. Garling, R. Foraita & I. Pigeot*\
Pharmacoepidemiology and Drug Safety (2020)\
DOI:10.1002/PDS.4970

### Contact

Louis Dijkstra\
Leibniz Institute for Prevention Research & Epidemiology  
E-mail: dijkstra (at) leibniz-bips.de
