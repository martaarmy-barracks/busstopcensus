# Welcome to the Bus Stop Census GitHub Page! 

## Disclaimer
Please read this from start to finish before exploring the repository. This README describes the tools needed to run the code included in this repository, the files contained in the repository, and any miscellaneous details that will help you understand what is in this repository. Ignore at your own risk. 

## What is the purpose of this repository?
The purpose of this repository is to house the data, code, figures, and products for the Bus Stop Census data anaylsis. MARTA Army has been collecting data on MARTA bus stops from February 4th, 2020 to Dec 31st, 2020. With data collection over, the task remains to clean, validate, and analyze the data. The repository documents this process and provides an organized and replicable framework for the public to use for their own analyses.

## R and RStudio Set Up Instructions
To get started, follow these instructions to install R and RStudio adapted from [Copyright (c) Software Carpentry](http://software-carpentry.org).

### R and RStudio
- Download and install [R, a free software environment for statistical computing and graphics](https://www.r-project.org/) from [CRAN](https://cran.rstudio.com/), the Comprehensive R Archive Network. It is highly recommended to install a precompiled binary distribution for your operating system – use the links up at the top of the CRAN page linked to above!

- Install RStudio's IDE (stands for integrated development environment), a powerful user interface for R: http://www.rstudio.com/ide/download/
  - RStudio includes a text editor, so you do not have to install another stand-alone editor.
  - RStudio includes an interface to Git and GitHub. You will still need to install Git (covered elsewhere) but RStudio provides a basic GUI for interacting with Git(Hub).
  
### Launching RStudio
Do whatever is appropriate for your OS to launch RStudio. You should get a window similar to the interface you see [here](https://rstudio.com/products/rstudio/). 

Put your cursor in the pane labelled Console, which is where you interact with the live R process. Create a simple object with code like x <- 2 * 4 (followed by enter or return). Then inspect the x object by typing x followed by enter or return. Obviously you should see the value 8 print to screen. If yes, you are good to go.

To make sure that RStudio and Git can talk to each other, please follow the instructions in the corresponding section below.

### Further Resources
The above will get your basic setup ready but here are some links if you are interested in reading a bit further.

- How to Use RStudio:
  - http://www.rstudio.com/ide/docs/
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
The repository is organized into the following structure (as suggested [here](https://jhuadvdatasci.substack.com/p/jhu-ads-2020-week-3-organizing-a):
- README.md
- .gitignore
- project.Rproj
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
