# Intro

Everything done here... Could be done in Excel, I guess. This is just an exercise :)

This example allows to read in a "Risk Register" in CSV (Excel exported CSV2 format, as I am European and the "," is a decimal separator).

# What is this about?

This script takes a "Risk Register" table and helps with an interactive (rather simplified) demo visualization.

## What you will get:

It takes input from the user as a file input (see below for correct input format).
Then it updates a "projects filter", to allow to filter the output by project name.

It then creates 3 output visualizations:
* A Risk Plot, where Probability and Impact are shown in x & y axis respectively. Because risks will normally have values of probability & impact will be integers in {1, 2, 3} (encoded for "low", "medium", "high" respectively), two risks with the same values for X & Y would overlap. We therefore throw in some noise to the position, so that two risks do not completely overlap.
* A Risk's details text box, showing the details about the risk dot nearest to the clicked position in the first visualization (thereby making it a bit more interactive)
* A Risks created count timeline, thereby allowing to see how many risks were created over time.

## To be noted about this Demo code:
This demo includes quite a few sample things:
* use of a global variable to keep a copy of a temporary when to be used where needed (using "<<-"), which is not great in theory but helps sometimes.
* use of reactiveVal() & observeEvent() for updating dataset used by the dashboard
* filter for project selection (we could use similar approaches for risk type filter, etc.)
* file input for a better UX, as opposed to having the user editing the code to get to the right file
* sample tryCatch() code for controlling incorrect input (INCOMPLETE, but this is a demo)
* interactive Risk visualization in GGPlot as scatterplot of probability * impact with Jitter (noise) added to positions to avoid overlap
* nearest point selection based on x-y clicked position

## Notes on future work:

This has MUCH room for improvement, as one could:
* add many more error checks
* give better feedback (that is, even SOME feedback) to the user about incorrect output
* add compatibility for input in different formats (namely: Excel & CSV instead of CSV2)
* highlight the clicked risk
* review the positionning around the x & y axis, as this visualization seems to indicate two risks with the same probability & impact have different values, which is not true.
* add more filters: Filter by risk type, radio button for showing all risks or only risks that are not yet closed (variable "Date_closed" empty or not)...
* better wrapping around risk details text
* ...
And a long list of more details to be considered

# Input data format

In this case we focus on a "Risk Register" dataset. I created one from scratch for this exercise, related to three hypothetical IT Security-related projects.
Keeping a risk register is good practice in Project Management. One needs to keep track of certain things, like "when a risk is identified", how many risks do we have on the table right now, the types of risk (related to the team, costs, scope...). And a dashboard can help report on those things visually (we are wired to ingest data in a more "visual" format than plain tables of data...).

The risk register in this case must have a header, with the following variables (although some of them are not used in the code), here shown with the semicolon separator:

Risk_ID;Date_identified;Project_Name;Risk_Name;Risk_type;Description;Probability;Impact;Reaction;Reaction_details;Date_closed

A sample CSV file could be as the one attached here (done in CSV2 on purpose, in spite of not being beautiful in GitHub :))
Please check it before using the script.

Note that dates MUST be in the format YYYY/MM/DD (which is just a choice I made a long time ago to default to, as it helps order such columns numerically to do so, e.g. for files in a folder...).
Note that you could change the input to accept CSV with "," as a separator by editing the read.csv2() and changing it to read.csv().
