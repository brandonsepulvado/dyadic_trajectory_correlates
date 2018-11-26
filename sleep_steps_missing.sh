#!/bin/csh

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 4
#$ -q long
#$ -N sleep_remove_missing

module load R
R CMD BATCH  sleep_steps_missing_input.R  sleep_missing_output.out