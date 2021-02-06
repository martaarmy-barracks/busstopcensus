# Welcome to the Bus Stop Census GitHub Page! 

## Disclaimer
Please read this from start to finish before exploring the repository. This README describes the tools needed to run the code included in this repository, the files contained in the repository, and any miscellaneous details that will help you understand what is in this repository. Ignore at your own risk. 

## What is the purpose of this repository?
The purpose of this repository is to house the data, code, figures, and products for the Bus Stop Census data anaylsis. MARTA Army has been collecting data on MARTA bus stops from February 4th, 2020 to Dec 31st, 2020. With data collection over, the task remains to clean, validate, and analyze the data. The repository documents this process and provides an organized and replicable framework for the public to use for their own analyses.

## R and RStudio Set Up Instructions
To get started, follow these instructions to install R and RStudio adapted from [Copyright (c) Software Carpentry](http://software-carpentry.org).

### R and RStudio
- Download and install R, a free software environment for statistical computing and graphics from [CRAN](https://cran.rstudio.com/), the Comprehensive R Archive Network. It is highly recommended to install a precompiled binary distribution for your operating system – use the links up at the top of the CRAN page linked to above!

- Install RStudio's IDE (stands for integrated development environment), a powerful user interface for R: http://www.rstudio.com/ide/download/
  - RStudio includes a text editor, so you do not have to install another stand-alone editor.
  - RStudio includes an interface to Git and GitHub. You will still need to install Git (covered below) but RStudio provides a basic GUI for interacting with Git(Hub).
  
### Launching RStudio
Do whatever is appropriate for your OS to launch RStudio. You should get a window similar to the interface you see [here](https://rstudio.com/products/rstudio/). 

Put your cursor in the pane labelled Console, which is where you interact with the live R process. Create a simple object with code like x <- 2 * 4 (followed by enter or return). Then inspect the x object by typing x followed by enter or return. Obviously you should see the value 8 print to screen. If yes, you are good to go.

To make sure that RStudio and Git can talk to each other, please follow the instructions in the corresponding section below.

### Communication between RStudio and Git
Obviously, RStudio can only act as a GUI front-end for Git if Git has been successfully installed and RStudio can find it.

A basic test for successful installation of Git is to simply type `git` at the shell command line. If you get a complaint about Git not being found it means installation was unsuccessful or that it is not being found, i.e. it is not on your `PATH`.

If Git appears to be installed, launch RStudio. Quit and re-launch RStudio if there's any doubt in your mind about whether you opened RStudio before or after installing Git.

### Using GitHub in RStudio
To clone this repository onto your local environment, follow these steps:
1. On GitHub, navigate to the Code tab of the repository.
2. On the right side of the screen, click `Clone or download`.
3. Click the `Copy to clipboard` icon to the right of the repository URL.
4. Open RStudio on your local environment.
5. Click `File`, `New Project`, `Version Control`, `Git`.
6. Paste the repository URL and enter TAB to move to the Project directory name field.
7. Click `Create Project`.
8. Finish reading the README.md file

### Required Packages
R is an extensible system where people can share useful code in the form of *packages*. All the packages required for this project will load in the `.Rprofile` file upon opening the project. All you need to do is install the packages. Run the code below to install all of the required packages:
```
pkgs <- c("chron", "data.table", "dplyr", "ggplot2", # package names
            "here", "jsonlite", "leaflet", "tidyr")
install.packages(pkgs, dependencies = TRUE) # install packages
```

### Further Resources
The above will get your basic setup ready but here are some links if you are interested in reading a bit further.

- How to Use RStudio:
  - http://www.rstudio.com/ide/docs/
- Getting Started with GitHub
  - https://guides.github.com/activities/hello-world/
- More RStudio Support:
  - https://support.rstudio.com/hc/en-us
- Using Version Control with RStudio
  - http://www.rstudio.com/ide/docs/version_control/overview
- R Installation and Administration
  - http://cran.r-project.org/doc/manuals/R-admin.html
- R FAQ:
  - http://cran.r-project.org/doc/FAQ/R-FAQ.html
- More about add-on packages in the R Installation and Administration Manual
  - http://cran.r-project.org/doc/manuals/R-admin.html#Add_002don-packages
  
## Repository Organization
The repository is organized into the following structure (as suggested [here](https://jhuadvdatasci.substack.com/p/jhu-ads-2020-week-3-organizing-a)):
- README.md
- .gitignore
- Bus Stop Census.Rproj
- .RProfile
- data/
  - raw_data/
  - tidy_data/
  - codebook.md
- code/
  - raw_code/
  - final_code/
- figures/
  - exploratory_figures/
  - explanatory_figures/
- products/
  - writing
  - presentations
 
 The following subsections contain descriptions of the main components in this file structure.
 
### README.md
This is what you are reading now. It contains everything you need to know to understand the contents of the repository and how to use it.
  
### Bus Stop Census.Rproj
This is the R project file for the Bus Stop Census analysis. An R Project is simply a working directory designated with a .RProj file. When you open a project (using File/Open Project in RStudio or by double–clicking on the .Rproj file outside of R), the working directory will automatically be set to the directory that the .RProj file is located in.

### .RProfile
This is a script that runs every time R starts. The `.RProfile` included in this repository will run each time the `Bus Stop Census.RProj` file is opened. The project `.RProfile` script is important because it loads the libraries needed to run the code as well as the GTFS data that is the key to linking bus stop IDs with their associated route, coordinate, and schedule information.

### data/raw_data
This folder contains the raw data used in the analysis. The raw data means it:
- No software was ran on the data
- The data was not manipulated any way
- No data was removed from the data set
- The data was not summarized in any way

### data/processed_data
This folder contains cleaned and validated data. This is the data that will be used as the starting point for all Bus Stop Census analyses.

### data/codebook.md
This is a document detailing the survey design and the methods of data collection.

### code/raw_code
The conglomeration of code used in the cleaning, validation, and analysis of the data. As this code will consist of the attempts to clean, explore, and visualize the data, it may be slightly disorganized.

### code/final_code
This is the bread and butter of the repository. The `final_code` folder contains code that is organized, easy to follow, and reproducible. This code has been curated from the `raw_code` folder to keep only what is important for our overall anaylsis of the Bus Stop Census data.

### figures/exploratory_figures
These are figures created to determine the structure, quirks, and summaries of the data. Because these figures are created mostly for those analyzing the data, these figures may be simple and not included in the final analysis figures.

### figures/explanatory_figures
These are figures intended to be shared. This subsection describes which code in the `final_code` folder produces each figure.

### products
This file contains any complete data analytics products that will be shared with our partners, MARTA, local governments, and the public.
