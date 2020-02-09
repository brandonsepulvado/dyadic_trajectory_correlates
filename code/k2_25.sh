#!/bin/bash

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 15
#$ -q long

module load gcc
module load R/3.6.0
setenv R_LIBS /scratch365/dheryadi/R-3.6
R CMD BATCH k2_25.R  k2_25_20200203.out