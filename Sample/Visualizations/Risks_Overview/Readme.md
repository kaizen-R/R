# Intro
This example allows to read in a "Risk Register" in CSV (Excel exported CSV2 format, as I am European and the "," is a decimal separator).

# What is this about?

This script takes a "Risk Register" table and helps with a (rather simplified) demo visualization.

## What you will get:

It takes input from the user as a file input (see below for correct input format).
Then it updates a "projects filter", to allow to filter the output by project name.

It then creates 3 output visualizations:
* A Risk Plot, where Probability and Impact are shown in x & y axis respectively. Because risks will normally have values of probability & impact will be integers in {1, 2, 3} (encoded for "low", "medium", "high" respectively), two risks with the same values for X & Y would overlap. We therefore throw in some noise to the position, so that two risks do not completely overlap.
* A Risk's details text box, showing the details about the risk dot nearest to the clicked position in the first visualization (thereby making it a bit more interactive)
* A Risks created count timeline, thereby allowing to see how many risks were created over time.

## To be noted about this Demo code:
This demo includes quite a few sample things:
* reactiveVal() for updating dataset used by the dashboard
* filter for project selection (we could use similar approaches for risk type, etc.)
* file input for a better UX, as opposed to having her editing the code
* sample tryCatch() code for controlling incorrect input (INCOMPLETE, but this is a demo)
* interactive Risk visualization in GGPlot as scatterplot of probability * impact with Jitter (noise) added to positions to avoid overlap
* nearest point selection based on x-y clicked position

## Notes on future work:

This has MUCH room for improvement, as one could:
* add many more error checks
* add compatibility for input in different formats (namely: Excel & CSV instead of CSV2)
* highlight the clicked risk
* review the positionning around the x & y axis, as this visualization seems to indicate two risks with the same probability & impact have different values, which is not true.
* add more filters: Filter by risk type, radio button for showing all risks or only risks that are not yet closed (variable "Date_closed" empty or not)...
* better wrapping around risk details text
* ...
And a long list of more details to be considered

# Input data format
The risk register must have a header with the following headers (variables), here shown with the semi-colon separator:

Risk_ID;Date_identified;Project_Name;Risk_Name;Risk_type;Description;Probability;Impact;Reaction;Reaction_details;Date_closed

A sample CSV file could be as the one attached here (done in CSV2 on purpose, in spite of not being beautiful in GitHub :))

Note that you could change the input to accept CSV with "," as a separator by editing the read.csv2() and changing it to read.csv().
