Installing system dependencies necesary to run ‘ecochange’
================
Wilson Lara-Henao - Victor Gutierrez-Velez
r Sys.Date()

## Windows 10

The following setup is based on the tutorial “R and the OSGeo4W package”
(<https://obrl-soil.github.io/r-osgeo4w-windows>). The users must find
and install from internet the binary OSGeo4W64. This binary allows
implementation of geospatial software necessary to run R, including
GDAL/OGR, GRASS, Python and SAGA. Here we guess that R and Rstudio are
already installed in the user’s computer and they know how to use
gdal-related packages in R.

1.  Install the OSGeo4W package to its default location (C:\\OSGeo4W64).
2.  In Windows, we must find the system environment variables and go to
    Advanced/Environment Variables.
3.  Set up the User variables. The users must be sure the file paths do
    exist in the OSGoeo4W file. They might need to change file numbers
    accordingly:

<!-- -->

    GISRC = %USERPROFILE%\AppData\Roaming\GRASS7\rc
    GRASS_ADDON_PATH = %USERPROFILE%\AppData\Roaming\GRASS7\addons

4.  Set up the following system variables:

<!-- -->

    OSGEO4W_ROOT = C:\OSGeo4W64
    GDAL_DATA = %OSGEO4W_ROOT%\share\gdal
    GDAL_DRIVER_PATH = %OSGEO4W_ROOT%\bin\gdalplugins
    GEOTIFF_CSV = %OSGEO4W_ROOT%\share\gdal
    GISBASE = %OSGEO4W_ROOT%\apps\grass\grass78
    GRASS_PYTHON = %OSGEO4W_ROOT%\bin\pythonw3.exe
    PROJ_LIB = %OSGEO4W_ROOT%\share\proj
    PYTHONHOME = %OSGEO4W_ROOT%\apps\Python37
    PYTHONPATH = %GISBASE%\etc\python
    SAGA = C:\OSGeo4W64\apps\saga-ltr

5.  Edit path (in the system variables) by adding the following:

<!-- -->

    %SAGA%
    %SAGA%\modules
    %PYTHONHOME%
    %OSGEO4W_ROOT%\bin
    %OSGEO4W_ROOT%\apps\qgis
    %GISBASE%\bin
    %GISBASE%\scripts
    %GISBASE%\lib

# Linux (Ubuntu 20.04)

This setup is based on the question “Installing libgdal-dev on Ubuntu
20.04”
(<https://askubuntu.com/questions/1267844/installing-libgdal-dev-on-ubuntu-20-04>).
Open a terminal (Ctrl + alt + t) and do:

    sudo apt update.
    sudo apt install libpq-dev
    sudo apt install gdal-bin
    sudo apt install libgdal-dev

# Mac

This setup is based on the tutorial “How to install GDAL on macOS”
(<https://medium.com/@vascofernandes_13322/how-to-install-gdal-on-macos-6a76fb5e24a4>).Open
a terminal and install Homebrew:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Change ownership of user directories:

    sudo chown -R $(whoami) /usr/local/share/zsh /usr/local/share/zsh/site-functions

And make sure that your user has write permission:

    chmod u+w /usr/local/share/zsh /usr/local/share/zsh/site-functions

then run:

    brew install gdal

It can take a while. Update the “pip” Python package manager:

    pip3 install --upgrade pip

Install GDAL for Python:

    pip3 install gdal

# Testing the GDAL installation in R

Open R and test the gdal functionality using few system2 calls, for
instance:

``` r
system2('gdalwarp')
system2('gdal_calc.py')
```

The system outputs must be help messages.

# R dependencies

The R package ‘ecochange’ relies on diverse dependences. It uses such
dependencies to improve execution performance during spatial analysis.
The users must be sure that all these are correctly installed.

``` r
# install.packages('ecochange')

dependencies <- c('ecochange','raster','rgdal','parallel', 'R.utils', 'rvest','xml2',
            'tidyverse','landscapemetrics','sf','dplyr','httr','getPass','gdalUtils',
            'gdalUtilities','rgeos','viridis','rasterVis','rlang', 'rasterDT')
#
# #Whole values in the following list must be TRUE
sapply(dependencies, require, character.only = TRUE)
```

## References
