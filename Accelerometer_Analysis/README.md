# Accelerometer Behaviour Analysis Report Generator
A HTML report generator of accelerometer data for pre-processing and determining variables for feature generation.
Created by Ryley Delaney February 2025

## Table of Contents  
- [What the Repository is for](#what-the-repository-is-for)
- [Scripts](#scripts)
- [Packages](#packages)
- [Setup](#setup)  
- [Usage](#usage)  



## What the Repository is for
This Repository contains scripts that allows the user to create a HTML document from accelerometer data that displays various information to help with feature generation. The document contains some preliminary information about the dataset used and a series of plots and tables. It was created as a part of a volunteering project during my undergrad with a PhD student at the University of the Sunshine Coast.



## Scripts
#### AccelerometerAnalysis_Main.R
- The main script of the repository, loads all packages, sources all functions as well as the GenerateBehaviourReport.Rmd. Part of the script involves receiving inputs from the user on including outliers in the Behaviour Duration box plot.
#### Functions.R
- Contains all the functions that are necessary for AccelerometerAnalysis_Main.R to run.
#### GenerateBehaviourDurationReport.Rmd
- The R markdown file that calls on the plot generator functions and creates the final HTML report.
#### Theme_BehaveWhiskers.R
- Contains a basic theme that's used for the Behaviour Generation Report.

## Packages
### What packages are used in this project? What is necessary to be installed??

## Setup
The Accelerometer Analysis Report generator requires a brief setup of directory structure. All Scripts should be placed in the working directory inside a folder called Scripts.
<br>
For Example: C:\Users\user\Desktop\AccelerometerData\Scripts
## Usage
