# vtc
R Code to perform data import and normalization of volume time curves as provided by TomTec Image Arena. Accompanying code for the Linden et al. 2025 publication.

The code has two functionalities. 
Step 1: TxtToCsv.R: Reads the TXT file, extract time and total volume and save as csv. Sample.txt is provided
Step 2: CreateNormCurve.R: Reads the csv file, do the normalization as explained in the paper, perform derivation and export two Excel files. A normalized volume time curve and a normalized derivative of the volume time curve. Sample.csv can be used

Step 3: Further downstream analysis (visualization, calculation of distances etc.) can be performed with the cleaned and normalized Excel files (sample_VTC.xlsx and sample_DVDT.xlsx). No code is provided for this due to many different possibilities.

###################################

In case of questions, contact Michael Neidlin: michael.neidlin@gmail.com
08.09.2025
