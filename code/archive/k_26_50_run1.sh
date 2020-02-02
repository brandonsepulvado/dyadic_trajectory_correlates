#!/bin/bash

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 24
#$ -q long
#$ -N k26-50_1run

module load gcc
module load R/3.6.0
setenv R_LIBS /scratch365/dheryadi/R-3.6
R CMD BATCH k_26_50_run1.R  k_26_50_run1.out