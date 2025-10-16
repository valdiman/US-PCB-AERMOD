## README file
# XX Airborne PCB Study

The scripts presented here were developed to predict airborne PCB emissions from the waters of the Portland Harbor Superfund Site. These estimated emissions are then used as input sources in AERMOD to model the dispersion and concentrations of PCBs in the surrounding air of Portland. Additional scripts were developed to visualize the data through plots, build a distance-from-water model to predict airborne PCB concentrations, and analyze sample similarities using PCB congener profiles and cosine theta testing.

--------------------------
SHARING/ACCESS/ATTRIBUTION LICENSE INFORMATION
--------------------------

Licenses/restrictions: licensed under the 2-Clause BSD License - see the [LICENSE](LICENSE) file for details.

----------------------
General Information
----------------------

Deposit Title: Portland Harbor Airborne PCB Study

This README file was generated on August 1, 2025 by Andres Martinez.

Contributor information:

Andres Martinez, PhD University of Iowa - Department of Civil & Environmental Engineering Iowa Superfund Research Program (ISRP) andres-martinez@uiowa.edu ORCID: 0000-0002-0572-1494

Alexis R. Slade, University of Iowa - Department of Civil & Environmental Engineering Iowa Superfund Research Program (ISRP) alexis-slade@uiowa.edu ORCID: 0000-0001-5492-2201

Principal Investigator: Keri Hornbuckle, PhD Principal Investigator email: keri-hornbuckle@uiowa.edu

This work was supported by the National Institutes of Environmental Health Sciences (NIEHS) grant #P42ES013661. The funding sponsor did not have any role in study design; in collection, analysis, and/or interpretation of data; in creation of the dataset; and/or in the decision to submit this data for publication or deposit it in a repository.

Subject: Polychlorinated Biphenyls; AERMOD; Portland Harbor Superfund Site; Atmospheric dispersion; Airborne PCBs; air-water flux; GC-Ms/MS; PUF air passive sampling

GeoLocation: Air samplers where collected in Portland, Oregon (45.5051° N, 122.6750° W).

--------
PREREQUISITES & DEPENDENCIES
--------

This section of the ReadMe file lists the necessary software required to run codes in "R".

Software:

Any web browser (e.g., Google Chrome, Microsoft Edge, Mozilla Firefox, etc.)
R-studio for easily viewing, editing, and executing "R" code as a regular "R script" file: https://www.rstudio.com/products/rstudio/download/

--------
SOFTWARE INSTALLATION
--------

This section of the ReadMe file provides short instructions on how to download and install "R Studio". "R Studio" is an open source (no product license required) integrated development environment (IDE) for "R" and completely free to use. To install "R Studio" follow the instructions below:

Visit the following web address: https://www.rstudio.com/products/rstudio/download/
Click the "download" button beneath RStudio Desktop
Click the button beneath "Download RStudio Desktop". This will download the correct installation file based on the operating system detected.
Run the installation file and follow on-screen instructions.

--------
R FILES AND STRUCTURE
--------

It is recommended to create a project in R (e.g., PortlandHarborProject.Rproj). Download the project file (.Rproj) and the R subfolder where the scripts are located, and the Subfolders.R file. Run first the Subfolder.R file, which will generate all the subfolders for this project. The structure of this project includes an R subfolder where all the R scripts are located, as previously indicated. There is a Data subfolder where the data are storage, and then an Output subfolder, where the results are located.

--------
Data
--------

The final dataset of airborne PCB concentrations is available at: Slade, Alexis; Martinez, Andres; Mathieu-Campbell, Martine; Watkins, Shannon; Cohen, Cassie; Hornbuckle, Keri C (2025):Airborne polychlorinated biphenyl congener concentrations using PUF-PAS from the Portland Harbor Superfund Site, Portland, OR, 2022 [dataset publication series]. [dataset].  PANGAEA, doi: https://doi.org/10.1594/PANGAEA.983837

The dataset can be downloaded using the script found at 'R/ReadData/ReadAirConcentrationDataPangaea.R'.

Air, water temperatures and wind speed were obtained from USGS and NOAA stations. The scripts to dowload the data are found in 'R/ReadData/ReadNOAAData.R' and 'R/ReadData/ReadUSGGData.R'.

The PCB water concentration is included in the flux scripts.




