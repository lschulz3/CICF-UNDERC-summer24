# Lisa Schulz - CICF-UNDERC - Summer Project 2024

## LiDAR-Based Mapping and the Parameterization of a Hydrological Model

### This repository includes code I created in the last 10 weeks working on this project. 

---

## Description of R files:

### dtm_catalog.R
#### The final version of the code we used to create the digital terrain model. Previous versions did not use the LAScatalog function from the lidR package and the lack of a buffer between squares of point cloud data lead to empty spots (with missing data) in the final product. This file also includes filtering out "birds" in the sky and points that appeared below the surface of the point cloud which leads to a smoother, more accurate model.

### dsmcatalog.R
#### Similar to the dtm file except that the rasterize_canopy function from the lidR packaged is used to create a model of the canopy.

### watershed_delineation.R
#### Learning how to delinate watersheds from the dtm. Following along this tutorial with our data: https://vt-hydroinformatics.github.io/rgeowatersheds.html#create-flow-accumulation-and-pointer-grids

### WTE_test.R
#### While creating the lake water budget model, we were interested in how to model the groundwater. This file uses known water table elvation data collected at UNDERC to plot groundwater data across time and across lakes (at different elevations). 

### lake_elv_testing.R
#### The flyover that collected the LiDAR data for the DTM gave us the lake surface elevation for that point in time. As lake volume changes so will the lake surface elevation. Using data collected by UNDERC, we can calculate the lake surface elevations across time.

### LWBmodel_getting_data.R 
#### Gathers, organizes, and interpolates missing data needed as forcings for the model. Sources include PRISM climate group and data collected by UNDERC researchers onsite.

### LWBmodel_ODE_setup.R
#### The ODE function to calculate changes in volumes of lakes across time. The main equation is dV.dt = P_lake + GWin - GWout + Runoff - E_lake - S_flow_out + subsurface_flow #[m^3 day^-1]. Each sub-equation is also calculated along with the rates of change of soil layer depths and water table elevation. Citations for some of the equations and constant parameters are in the citations file.

---
#### Funded by NSF CI Compass (Grant #2127548) and UNDERC & CRC
