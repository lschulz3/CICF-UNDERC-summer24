
library(MFEUtilities)
library(deSolve)
library(prism)

source("~/Desktop/underc24/hydrology_model/LWBmodel_getting_data.R")

##getting constant parameters
params <- read.csv("/Users/Lisa/Downloads/paramTable.csv")
params <- params$value

##getting individual lake characteristics
longer_lake_list <- get_lake_chars2() #all the UNDERC lakes from the MFE database

##initial y input
initial_volumes = longer_lake_list$lake_surface_area * longer_lake_list$avg_depth #10^(-0.0589 + (1.1296*log10(A))) #[m^3]
W1_initial = 5 #[cm] this is half of the initial W2
W2_initial = params[8] #[cm]
W3_initial = 511 # [m] #params[11] #[cm]
initial_input = c(initial_volumes, W1_initial, W2_initial, W3_initial)

LWBode <- function(t, y, params, longer_lake_list){
  with(as.list(c(y, params, longer_lake_list)), {
    ##getting forcings
    Tair = Tair_approx(t) #Air temperature [C]
    T_d = T_d_approx(t) #Dew point temperature [C]
    P =  P_approx(t) #Precipitation [mm]
    Rs = Rs_approx(t) #Solar radiation [MJ m^-2 day^-1]
    wind_speed = wind_speed_approx(t) #daily average wind speed [m/s]
    
    perimeter <- longer_lake_list$perimeter #[m]
    avg_depth <- longer_lake_list$avg_depth #[m]
    Ac = longer_lake_list$catchment_area #[m^2]
    A = longer_lake_list$lake_surface_area #[m^2]
    
    ##Evaporation and Transpiration
    #simplified Penman evaporation
    RH = 100 - (5 * (Tair - T_d))
    lat_radians = 0.8071493292 #the latitude of underc in radians, using the lattitude of "underc" in getting data file
    month = get_month(t) #month at current time step
    N = 4 * lat_radians * sin((0.53 * month) - 1.65) + 12 #daylight hours
    Ra = 3 * N * sin((0.131 * N) - (.95 * lat_radians)) #extraterrestrial radiation [MJ m^-2 day ^-1]
    au = 1 #wind speed coefficient
    E_test = (0.051 * (1 - .08) * Rs * ((max(Tair + 9.5, 0))^(1/2))) - (2.4 * ((Rs/Ra)^2)) + (0.052 * (max(Tair + 20, 0)) * (1 - (min(RH,100)/100)) * (au - 0.38 + (.54 * wind_speed))) #[mm day^-1]
    E = ifelse(E_test > 0, E_test, 0)
    #simplified Penman transpiration
    ET_test = (0.038 * Rs * ((max(Tair + 9.5, 0))^(1/2))) - (2.4 * ((Rs/Ra)^2)) + (0.075 * (max(Tair + 20, 0)) * (1 - (RH/100))) #same restrictions as evap, reference crop is grass which might be off from trees [mm day^-1]
    ET = ifelse(ET_test > 0, ET_test, 0)
    T = ET - E
    
    ##Evaluating soil layer elevations and GW fluxes using VIC
    W1 = y[length(longer_lake_list$lake_id) + 1] #height of upper soil layer [cm]
    W2 = y[length(longer_lake_list$lake_id) + 2] #height of middle soil layer [cm]
    W3 = y[length(longer_lake_list$lake_id) + 3] #height of lower soil layer [cm]
    
    Wmax1 = params[5] #soil water holding capacity in upper soil layer [cm]
    bi = params[1] #infiltration shape [unitless]
    im = Wmax1 * (1 + bi) #[cm]
    i0 = im - im * (((im - (1 + bi) * W1)/im)^(1/(1 + bi))) #[cm]
    r = params[2] #residual soil moisture content [cm hr^-1]
    Ks = params[3] #Saturated hydraulic conductivity [cm hr^-1]
    Bp1 = params[4] #soil pore size distribution index [unitless]
    Q12 = max(min(Ks * ((W1 - r)/(Wmax1 - r))^((2/Bp1) + 3), (W1 - 0.5)), 0) #[cm H20 day-1]
    ev = params[12] #evaporation constant, depends on vegetation type
    Pin = (P - (P * ev))/10 #incoming water [cm/day]
    Q1 = ifelse(((P - P * ev)/10) > 0, ifelse((i0 +((P - P * ev)/10))>= im, ((P - P * ev)/10) - Wmax1 + W1 , ((P - P * ev)/10) - Wmax1 + W1 + Wmax1 * (1 - ((i0 + ((P - P * ev)/10))/im))^(1 + bi)), 0) #[cm H20 day-1] #calculate drainage 
    if((isTRUE(W1 + Pin) < Wmax1) == TRUE){
      export = Q1 + Q12 #[cm H20 day^-1]
    }else{
      export = Pin + Q12 #[cm H20 day^-1]
    }
    Wmax2 = 20 #params[6] change the params file!
    Bp2 = params[7]
    Q23 = max(min(Ks * ((W2 - r)/(Wmax2 - r))^((2/Bp2) + 3), (W2 - 0.5)), 0) #[cm H20 day-1]
    W20 = params[8] #reference height for this soil layer [cm]
    Tstar = params[9] #characteristic rate of water flow [day]
    Q2 = ifelse(W2 > W20, (W2 - W20)/Tstar, 0) #[cm H20 day-1] 
    Tstar2 = params[10]
    W30 = 511 #[m] #params[11]
    Q3 = ifelse(W3 > W30, ((W3 - W30)*100)/Tstar2, 0) #[cm H20 day-1] 
      
    GWflux = rep(NA, length = length(longer_lake_list$lake_id)) 
    GWout = GWflux #setting the rest of the flows to empty lists
    Runoff = GWflux
    P_lake = GWflux
    E_lake = GWflux
    dV.dt = GWflux
    S_flow_out = GWflux
    subsurface_flow = GWflux
    Qgw = GWflux
    
    V = y[1:length(longer_lake_list$lake_id)] #lake volume [m^3]
    lake_elvs_ini = longer_lake_list$lake_surface_elevation #initial lake elevations [m]
    lake_bot_elv = longer_lake_list$lake_surface_elevation - longer_lake_list$avg_depth #elevation of lake bottom [m]
    lake_elvs = (V / A) + lake_bot_elv #lake elevations [m]
    
    ##Looping through each lake starting at highest elevation
    for (i in 1:length(longer_lake_list$lake_id)){
      
      ##Calculating stream flow out of the lake 
      Vini = A[i] * avg_depth[i] #reference lake volume [m^3]
      T_star3 = params[13]
      S_flow_out[i] = max((V[i] - Vini) / T_star3, 0) #[m^3 day^-1]
      
      ##Groundwater in and out of lake 
      WTE = W3 #water table elevation
      Ks2 = ((1.2/100)*24)/100 #add this to the params file and get a better estimate [m day^-1]
      B = 1 # thickness of riverbed [m]
      ci = (Ks2)/B #conductance of lake bed when it discharges from the aquifer [day^-1]
      lake_surface_elv = lake_elvs[i]
      Qgw[i] = ci * (WTE - lake_surface_elv) # groundwatter flux [m day^-1]
      GWflux[i] = Qgw[i] * perimeter[i] * avg_depth[i] # [m^3 day^-1]
      
      ##Precipitation for a given lake
      P_lake[i] = (P/1000) * A[i] #[m^3 day^-1]
      
      ##Evaporation for a given lake
      E_lake[i] = (E/1000) * A[i] #[m^3 day^-1]
      
      ##VIC runoff and subsurface infiltration
      subsurface_flow[i] = (Q2/100) * Ac[i] #subsurface flow into lake from W2
      Runoff[i] = (Q1/100) * Ac[i] #runoff into lake [m^3 day^-1]
      
      ##Volume equation
      dV.dt[i] = P_lake[i] + GWflux[i] + Runoff[i] - E_lake[i] - S_flow_out[i] + subsurface_flow[i] #[m^3 day^-1]
      
    }
    
    ##UNDERC area (approximate)
    UNDERC_area = sum(Ac) #area of UNDERC property [m^2]
    
    ##differential equations for soil layers
    dW1.dt = Pin - export #[cm day^-1]
    dW2.dt = Q12 - Q23 - Q2 #[cm day^-1]
    dW3.dt = (Q23/100) + ((-sum(GWflux)/UNDERC_area)) - (Q3/100) #[m day^-1]
    
    return(list(c(dV.dt, dW1.dt, dW2.dt, dW3.dt), c(GWflux1=GWflux, lake_elvs1=lake_elvs[1], dW3.dt=dW3.dt, sumGWflux = sum(GWflux), Qgw1=Qgw[1], Q3 = Q3, Q23=Q23, subsurface_flow1=subsurface_flow[1], S_flow_out1=S_flow_out[1], Runoff1=Runoff[1], P_lake1=P_lake[1], E_lake1=E_lake[1], Q1=Q1, Q2=Q2, Q12=Q12)))
})
}

LAKE <- ode(y = initial_input, times = 0:7336, func = LWBode, parms = params, longer_lake_list = longer_lake_list)

plot(LAKE[,1], LAKE[,25], type = "l", xlab = "Days from start", ylab = "W3")

plot(LAKE[,1], LAKE[,23], type = "l", xlab = "Days from start", ylab = "W1 [cm]")

plot(LAKE[,1], LAKE[,24], type = "l", xlab = "Days from start", ylab = "W2 [cm]")
plot(LAKE[,1], LAKE[,13], type = "l", xlab = "Days from start", ylab = "CR volume")


for (i in 2:22){
  plot(LAKE[,1], LAKE[,i], type = "l", xlab = "Days from start", ylab = paste0("lake ",longer_lake_list$lake_id[i-1] ," volume"), main = paste0("lake ",longer_lake_list$lake_id[i-1] ," volume over time"))
}

matplot(LAKE[,1], cbind(LAKE[,2:22]), type = "l", lty = 1, col = height.colors(25), xlab = "Days from start", ylab = "Lake Volumes")
legend("topright", legend = c(1:21), col = height.colors(25), lty = 1)
  