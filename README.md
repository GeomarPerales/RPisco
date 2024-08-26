# RPisco : RPisco is a package with tools for manipulate PISCO databases.

## 1. Overview

RPisco is a package with tools for manipulating PISCO climatic databases from SENAMHI (Peru). PISCO stands for Peruvian Interpolated Data of SENAMHI’s Climatological and Hydrological Observations. RPisco provides functions for extracting values from single or multiple stations, summing monthly and annual values, and calculating monthly and annual averages.

To use RPisco, Download [PISCO climatic databases](https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/) from [IRI/LDEO Climate Data Library](https://iridl.ldeo.columbia.edu/) and [Piscoeo_pm files](https://figshare.com/articles/dataset/Reference_crop_evapotranspiration_PISCOeo_pm_/15215106?backTo=/collections/A_reference_evapotranspiration_gridded_database_based_on_FAO_Penman-Monteith_in_Peru_during_1981-2016/5633182) from [Figshare](https://figshare.com/).

## 2. Installation

Paso 1: Install **devtools**:
```	
> install.packages("devtools")
```
Paso 2: From Github:
```	
> library(devtools)
> install_github("GeomarPerales/RPisco")		
```
## 3. Contents

RPisco contains multiple tools for manipulating PISCO climatic databases from SENAMHI (Peru).

## 4. Credits

"RPisco was developed by Geomar Perales. For any issues or suggestions, please contact: perales.geomar@gmail.com."


## 5. References

Aybar C., Lavado W., Huerta A., Fernandez C., Vega F. and Felipe O. (2018). PISCOp V.2.1: Construction of a High Spatial Temporal Gridded Rainfall Dataset for Peru. In preparation.

RStudio Team (2020). RStudio: Integrated Development for R. RStudio, PBC, Boston, MA URL http://www.rstudio.com/
