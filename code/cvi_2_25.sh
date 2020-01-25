#!/bin/bash

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 24
#$ -q long
#$ -N calc_cvi

module load gcc
module load R/3.6.0
setenv R_LIBS /scratch365/dheryadi/R-3.6
R CMD BATCH cvi_2_25.R  cvi_2_25.out