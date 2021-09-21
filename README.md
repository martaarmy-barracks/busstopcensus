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
pkgs <- c("chron", "data.table", "dplyr", "geosphere", "ggplot2", # package names
           "googlesheets4", "here", "jsonlite", "leaflet", 
           "maps", "stringdist", "stringr", "tidyr")
install.packages(pkgs, dependencies = TRUE) # install packages

library(devtools)
install_github('mhudecheck/revgeo')
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
    - gtfs/
    - survey/
    - validation/
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
This folder contains cleaned and validated data. This is the data that will be used as the starting point for all Bus Stop Census analyses. The 
The following is a list of all the fields and the question they match with or description if not related to a survey question:
- **Record_ID**
  - This is a unique ID number given to all survey responses. This record ID is used to identify specific responses during the cleaning process.
- **Stop_ID**
  - This is the Stop ID the survey is submitted for. Most Stop IDs are prefilled by the Bus Stop Census online surveying platform.
- **Stop_Lat**
  - Latitude of the bus stop from GTFS data
- **Stop_Lon**
  - Longitude of the bus stop from GTFS data
- **Timestamp**
- **Email_Masked**
  - What is your email address?
    - The real emails have been masked to protect the privacy of the surveyor.
- **Main_Street**
  - What street or road is the bus stop located on?
    - The main street is the street the route runs along. In other words, it is the street the bus drives on. This field is prefilled by the Bus Stop Census online surveying platform.
- **Nearest_Landmark**
  - What is the nearest cross street or landmark?
    - This field can either be a nearby landmark/address or a cross street that runs perpendicular to the main street. This field is prefilled by the Bus Stop Census online surveying platform.
- **Routes**
  - What routes serve this bus stop?
    - This field is prefilled by the Bus Stop Census online surveying platform.
- **Direction** 
  - What direction is the bus heading from this stop?
- **Seating**
  - Does the stop have a bench or other seating?
- **Shelter**
  - Does this stop have a shelter?
- **Trash_Can**
  - Does this bus stop include a trash can?
- **Litter**
  - Does the bus stop have any of these cleanliness issues? *Litter at the stop*
- **Grafitti**
  - Does the bus stop have any of these cleanliness issues? *Graffiti (unauthorized) or tagging on bus stop amenities*
- **Overflow**
  - Does the bus stop have any of these cleanliness issues? *Overflowing or poorly maintained trash can*
- **Dirty_Seating**
  - Does the bus stop have any of these cleanliness issues? *Dirty seating area*
- **Other**
  - Does the bus stop have any of these cleanliness issues? *Other*
- **Line_of_Sight**
  - If you had to flag the bus down, would you have to step into the roadway or lean into traffic?
- **Route_Number**
  - What wayfinding information is present at the stop? Select all that apply. *Route Numbers*
- **Route_Schedule**
  - What wayfinding information is present at the stop? Select all that apply. *Route Schedule*
- **Route_Map**
  - What wayfinding information is present at the stop? Select all that apply. *Route Map*
- **Customer_Service**
  - What wayfinding information is present at the stop? Select all that apply. *Customer Service Contact Information*
- **None_Of_The_Above**	
  - What wayfinding information is present at the stop? Select all that apply. *None of the above*
- **Wayfinding Accessbility**
  - Is the wayfinding information  located at the eye level of a person using a wheelchair?
- **Lighting**
  - Is the stop well lit at night?
- **Sidewalk**
  - Is there a paved sidewalk to the boarding area of the bus?
- **Obstacles**
  - Are there any obstacles at or on the path to this bus stop that would limit the mobility of a person using a wheelchair or stroller?
- **Obstacle_Desc**
  - If there are obstacles, please briefly describe them.
- **Boarding_Area**
  - What is the surface of the boarding area?
- **Main_Street_Crosswalk**
  - Is there a clearly painted crosswalk within 100 feet of the bus stop? (Select all that apply.) *Yes, on the main street*
- **Cross_Street_Crosswalk**
  - Is there a clearly painted crosswalk within 100 feet of the bus stop? (Select all that apply.) *Yes, on the cross street*
- **Worn_Faded**
  - Is there a clearly painted crosswalk within 100 feet of the bus stop? (Select all that apply.) *Yes, and crosswalk paint is faded or worn away*
- **No_Crosswalk**
  - Is there a clearly painted crosswalk within 100 feet of the bus stop? (Select all that apply.) *No, no painted crosswalk within 100 feet*
- **Traffic_Light**
  - What features does the crosswalk(s) have? Select all that apply. *Traffic Light*
- **Curb_Cuts**
  - What features does the crosswalk(s) have? Select all that apply. *Curb cuts for wheelchairs*
- **Crosswalk_Signals**
  - What features does the crosswalk(s) have? Select all that apply. *Crosswalk signals with push button*
- **Crossing_Audio**
  - What features does the crosswalk(s) have? Select all that apply. *Crossing audio overlays for visually impaired*
- **Tactile_Guide**
  - What features does the crosswalk(s) have? Select all that apply. *Tactile guide strips for visually impaired*
- **Informal_Pathways**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *Pedestrians using informal pathways where sidewalks do not exist*
- **Compete_For_Seat**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *Pedestrians competing for seating at the bus stop*
- **Cross_Midblock**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *Pedestrians crossing the roadway at midblock locations*
- **Catch_The_Bus**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *Pedestrians running across roadways to catch the bus*
- **Dangerous_Motorists**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *Dangerous motorist behavior around bus stop (e.g., speeding or not yielding to pedestrians)*
- **First_Visit**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *None of the above (first visit to this stop)*
- **Regular_User_None**
  - Have you observed any of the following pedestrian behavior at this stop? Select all that apply *None of the above (occasional or frequent user of this stop)*
- **On_Site_Survey**
  - Did you complete this survey at the physical location of the bus stop?
- **Additional_Comments**	
  - Do you have any additional observations or anecdotes for this bus stop?


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
