#!/bin/bash

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 24
#$ -q long
#$ -N kshape_24

module load gcc
module load R/3.6.0
setenv R_LIBS /scratch365/dheryadi/R-3.6
R CMD BATCH kshape_24.R  kshape_24.out