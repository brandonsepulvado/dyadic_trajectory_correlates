#!/bin/csh

#$ -M bsepulva@nd.edu
#$ -m abe
#$ -pe smp 4
#$ -q long
#$ -N sleep_steps_filtered_date_diffs

module load R/3.5.1
R CMD BATCH  get_date_diffs_filtered.R  get_date_diffs_filtered.out