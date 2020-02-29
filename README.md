# Behavioral Trajectory Similarity in Social Networks and Its Correlates

## Collaborators

Brandon Sepulvado, NORC at the University of Chicago  
David Hachen, University of Notre Dame  
Omar Lizardo, UCLA  
Michael Lee Wood, BYU  
Ethan Fridmanski, University of Notre Dame  
Cheng Wang, Wayne State University  
Matthew J. Chandler, Princeton University  


## NetHealth Study
This work was funded by the National Institutes of Health (NIH) grant 1R01HL117757 and supported in part by the Notre Dame Center for Research Computing (ND CRC). We specifically acknowledge the assistance of [Dodi Heryadi](https://crc.nd.edu/about/people/dodi-heryadi/).

## Workflow

The paper's analysis occur across several files; this section describes the workflow. While certain components of the data manipulation and analysis may be conducted on a personal computer, the actual implementation of the k-shape algorithm should be done on a high performance cluster (or in the cloud).  

The scripts are written with a particular organization of the project directory in mind. There should be a main directory that includes the .Rproj file, and, within this directory, there should be three subdirectories: input, output, and code. The input data (see below) should be downloaded within the input folder; the code should be in the code folder; the output from these files will be saved to the output folder. Note that the scripts to run in a high performance environment are an exception to this structure. They should be executed, and the files they output should be saved in the output subdirectory. 

### Data Location

The data may be found on Harvard's Dataverse at this [link](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQIH2H). These files should be included in the input subdirectory.

### Data Preparation 

The files in this section must be executed before those in the following sections will run. 

**data_import_clean.R** is the first file that should be run. It imports daily observations for each participant in the study and then calls **create_dyad_trends.R**, which creates daily observations for each dyad. In this latter information is included multiple variables: *vertex_1*, *vertex_2*, *date*, *steps_v1* (steps on that date for vertex one), *steps_v2* (steps on that date for vertex two), and *abs_diff* (the absolute difference between steps for vertices one and two).  

**create_ts_object.R** creates processed time series input for the k-shape algorithm. Dyads are excluded if they are missing too many observations. Currently, the dyad must have no missing data for at least 75% of the dates between 2015-09-01 and 2015-12-19. Linear interpolation is used for missing observations of the remaining dyads. The output for this script is an object called data_interpolated, which includes four columns: *id_dyad* (a unique identifier for each dyad that is just the identifiers for each dyad concatenated together), *datadate*, *abs_diff*, and *dyad_data* (a list column that contains datadate and abs_diff for each dyad; as such it is redudant with the previous two variables).  

### Data Descriptives

**descriptives.R** provides descriptives on dyadic observations, including NAs, over the course of the Fall 2015 semester. None of the following scripts depends upon this file. 

### Analyses

*Note*: files in this section should be run in a high performance environment, such as that provided by the [Notre Dame Center for Research Computing](https://crc.nd.edu/) (ND CRC).  

**k2_25.R** runs k-shape for a range of cluster numbers (i.e., 2-24); k2_25.sh submits the previous file to the ND CRC.

**cvi_2_25.R** gets cluster validity indices (CVIs) across the cluster numbers to provide heuristics to help determine the best number of clusters; **cvi_2_25.sh** submits the previous file to the ND CRC.  

**plot_cvi_results.R** takes the output of the previous file and plots them. The visualization splits the seven cluster validity indices into two panes for which the indicators should be either maximized or minimized, and it includes [min-max normalization](https://en.wikipedia.org/wiki/Feature_scaling#Standardization_(Z-score_Normalization)/) so that the CVIs fall on the same scale.  

**kshape_24_21.R** runs the k-shape algorithm for 21 and 24 clusters, as they were the two best candidate cluster numbers, given the results of the cluster validity indices. The required input object is the data_intperolated object from the **create_ts_object.R** file. **kshape_24_21.sh** submits the previous file to the ND CRC.  

**analysis_clusters.R** does two things. (1) It provides descriptives on the two k-shape objects (i.e., for 21 and 24 clusters) in the previous file. (2) It generates homophily variables used in logistic regressions to identify relationships between sociodemographic characteristics and the dyadic trajectory clusters, and (3) it performs these logistic regressions. 

**edge_regressions.R** must be run after analysis_clusters.R because it relies upon one of the objects. This file performs logistic regressions (regular, with Firth correction, and Firth Logistic regression with Intercept Correction--FLIC).