# Code to estimate total PCB fluxes from Fox River
# using 20xx and 20xx water samples
# The code estimate the flux of each congener, and
# sum them to get total PCB
# Air data are not used in these calculations
# Monte Carlo simulation is included
# No needs of R packages

# Chemical properties -----------------------------------------------------
cp <- data.frame(
  Congener = c(
    "PCB1", "PCB2", "PCB3", "PCB4+10", "PCB5+8", "PCB6", "PCB7+9", "PCB11", "PCB12+13",
    "PCB14", "PCB15", "PCB16+32", "PCB17", "PCB18+30", "PCB19", "PCB20+21+28+31+33+50+53",
    "PCB22", "PCB23", "PCB24+27", "PCB25", "PCB26+29", "PCB34", "PCB35", "PCB36", "PCB37+42",
    "PCB38", "PCB39", "PCB40+41+64+71+72", "PCB43+49+52+69+73", "PCB44+47+65", "PCB45+51",
    "PCB46", "PCB48+59+62+75", "PCB54", "PCB55", "PCB56+60", "PCB57", "PCB58",
    "PCB61+66+70+74+76+93+95+98+100+102", "PCB63", "PCB67", "PCB68",
    "PCB77+85+110+111+115+116+117", "PCB78", "PCB79", "PCB80",
    "PCB81+86+87+97+107+108+109+112+119+124+125", "PCB82+135+144+151+154",
    "PCB83+99", "PCB84+92", "PCB88+91", "PCB89", "PCB90+101+113", "PCB94", "PCB96",
    "PCB103", "PCB104", "PCB105", "PCB106+118", "PCB114+122+131+133+142+146+165",
    "PCB120", "PCB121", "PCB123+139+140+147+149", "PCB126", "PCB127",
    "PCB128+162+166+167", "PCB129+137+138+158+160+163+164+176+178", "PCB130",
    "PCB132+153+161+168", "PCB134+143", "PCB136", "PCB141", "PCB145", "PCB148",
    "PCB150", "PCB152", "PCB155", "PCB156+157+172+197+200", "PCB159", "PCB169",
    "PCB170+190", "PCB171+173+202", "PCB174", "PCB175", "PCB177", "PCB179",
    "PCB180+193", "PCB181", "PCB182+187", "PCB183+185", "PCB184", "PCB186",
    "PCB188", "PCB189", "PCB191", "PCB192", "PCB194+205", "PCB195+208",
    "PCB196+203", "PCB198+199+201", "PCB204", "PCB206", "PCB207", "PCB209"),
  MW.PCB = c(
    rep(188.644, 3),
    rep(223.088, 8),
    rep(257.532, 13),
    rep(291.976, 1),
    rep(257.532, 2),
    rep(291.976, 15),
    rep(326.42, 1),
    rep(291.976, 3),
    rep(326.42, 16),
    rep(360.864, 1),
    rep(326.42, 2),
    rep(360.864, 15),
    rep(395.308, 16),
    rep(429.752, 5),
    rep(465.740544, 2),
    498.64),
  nOrtho.Cl = c(
    1, 0, 0, 2, 1, 1, 1, 0, 0, 0,
    0, 2, 2, 2, 3, 1, 1, 1, 2, 1,
    1, 1, 0, 0, 2, 0, 0, 2, 2, 2,
    3, 3, 2, 4, 1, 1, 3, 1, 1, 1,
    1, 1, 2, 0, 0, 0, 2, 2, 2, 3,
    3, 3, 2, 3, 4, 3, 4, 1, 1, 1,
    1, 2, 3, 0, 0, 2, 2, 2, 2, 3,
    4, 2, 4, 3, 4, 4, 4, 1, 1, 0,
    2, 3, 3, 3, 3, 4, 2, 3, 3, 3,
    4, 4, 4, 1, 2, 2, 1, 3, 3, 3,
    4, 3, 4, 4
  ),
  H0.mean = c(
    -3.526, -3.544, -3.562, -3.483, -3.518, -3.486, -3.49, -3.537, -3.595, -3.376,
    -3.649, -3.6, -3.428, -3.495, -3.355, -3.562, -3.719, -3.497, -3.393, -3.5,
    -3.526, -3.375, -3.3745, -3.473, -3.592, -3.634, -3.524, -3.565, -3.496, -3.638,
    -3.45, -3.47, -3.519, -3.242, -3.739, -3.82, -3.568, -3.602, -3.694, -3.615,
    -3.631, -3.424, -3.707, -3.787, -3.705, -3.426, -3.736, -3.835, -3.407, -3.6,
    -3.461, -3.526, -3.61, -3.407, -3.387, -3.298, -3.13, -4.003, -3.901, -3.845,
    -3.61, -3.253, -3.625, -4.087, -3.807, -3.984, -3.886, -3.817, -3.783, -3.639,
    -3.492, -3.76, -3.328, -3.367, -3.296, -3.369, -3.418, -4.053, -3.808, -4.186,
    -4.059, -3.763, -3.772, -3.651, -3.787, -3.56, -3.969, -3.638, -3.693, -3.696,
    -3.339, -3.434, -3.353, -3.177, -3.876, -3.718, -3.926, -3.884, -3.853, -3.644,
    -3.463, -4.059, -3.772, -3.948),
  H0.error = rep(0.662, 104),
  Kow.mean = c(
    4.46, 4.69, 4.69, 4.65, 5.07, 5.06, 5.06, 5.28, 5.29, 5.28,
    5.3, 5.16, 5.25, 5.24, 5.02, 5.67, 5.58, 5.57, 5.44, 5.67,
    5.66, 5.66, 5.82, 5.88, 5.76, 5.76, 5.89, 5.95, 5.84, 5.75,
    5.53, 5.53, 5.78, 5.21, 6.11, 6.11, 6.17, 6.17, 6.2, 6.17,
    6.2, 6.26, 6.48, 6.35, 6.42, 6.48, 6.29, 6.2, 6.39, 6.04,
    6.13, 6.07, 6.38, 6.13, 5.71, 6.22, 5.81, 6.65, 6.74, 6.65,
    6.79, 6.64, 6.67, 6.89, 6.95, 6.74, 6.83, 6.8, 6.92, 6.55,
    6.22, 6.82, 6.25, 6.73, 6.32, 6.22, 6.41, 7.18, 7.24, 7.42,
    7.27, 7.11, 7.11, 7.17, 7.08, 6.73, 7.36, 7.11, 7.17, 7.2,
    6.85, 6.69, 6.82, 7.71, 7.55, 7.52, 7.8, 7.56, 7.65, 7.2,
    7.3, 8.09, 7.74, 8.18),
  Kow.error = rep(0.32, 104)
)

# Update names
{
  Congener <- cp$Congener
  MW.PCB <- cp$MW.PCB
  nOrtho.Cl <- cp$nOrtho.Cl
  H0.mean <- cp$H0.mean
  H0.error <- cp$H0.error
  Kow.mean <- cp$Kow.mean
  Kow.error <- cp$Kow.error
}

# Water concentrations ----------------------------------------------------
# Read data from Data Folder, FoxRiver.csv
fx <- read.csv("Data/FoxRiver/FoxRiver_temp.csv")
# Select Site Name
# Operable Unit 1
fx.ou1 <- fx[fx$SiteName == "Operable Unit 1", ]


# Calculate averages
{
  wc.values <- wc[, 8:166]
  C.PCB.water.ave <- sapply(wc.values, mean)
  C.PCB.water.error <- sapply(wc.values, sd)
}

# Meteorological data -----------------------------------------------------



# Data obtained from ReadNOAAData.R data and ReadUSGSData.R codes
# 2018-08
{
  tair.mean <- 21.3 # C, data from NOAA
  tair.error <- 5.27 # C, data from NOAA
  twater.mean <- 23.24 # C, data from USGS
  twater.error <- 1.29 # C, data from USGS
  u.mean <- 2.35 # m/s, data from NOAA
  u.error <- 1.58 # m/s, data from NOAA
  # Modify u @6.7 m to @10 m
  u10.mean <- (10.4/(log(6.7) + 8.1))*u.mean
  u10.error <- (10.4/(log(6.7) + 8.1))*u.error 
  P.mean <- 1016 # mbar, data from NOAA
  P.error <- 3.23 # mbar, data from NOAA
}

# Flux calculations -------------------------------------------------------

# Flux function
final.result = function(MW.PCB, H0.mean, H0.error, 
                        C.PCB.water.ave, C.PCB.water.error, nOrtho.Cl,
                        Kow.mean, Kow.error) {
  # fixed parameters
  
  R <- 8.3144 # [Pa m3/K/mol]
  T <- 298.15 # [K]
  
  F.PCB.aw <- NULL
  # number of replicates for Monte Carlo simulation
  for (replication in 1:1000) {
    
    # Random parameters
    # Parameters for calculating Delta Uaw
    a <- rnorm(1, 0.085, 0.007)
    b <- rnorm(1, 1, 0.5)
    c <- rnorm(1, 32.7, 1.6)
    # Parameter for calculating Delta Uoa
    a2 <- rnorm(1, 0.13, 0.02) 
    b2 <- rnorm(1, 2.9, 1.2)
    c2 <- rnorm(1, 47.8, 4.3)
    # Henry's law constant 
    H0 <- rnorm(1, H0.mean, H0.error) # [Pa m3/mol]
    # Octanol-water partition coefficient
    Kow <- rnorm(1, Kow.mean, Kow.error) # [Lwater/Loctanol] 
    # PCB water concentration
    # Concentrations can be changed
    C.PCB.water <- abs(rnorm(1, C.PCB.water.ave, C.PCB.water.error)) # [pg/L]
    # DOC (Spencer et al 2012)
    DOC <- abs(rnorm(1, 2, 0.3)) # [mg/L]
    # Water temperature
    T.water <- rnorm(1, twater.mean, twater.error) # [C]
    # Air temperature
    T.air <- rnorm(1, tair.mean, tair.error) # [C]
    # atmospheric pressure
    P <- rnorm(1, P.mean, P.error) # [mbar]
    # Wind speed @10 m
    u <- abs(rnorm(1, u10.mean, u10.error)) # [m/s] missing
    
    # Computed values
    # Henry's law constant (HLC) corrections
    # PCB internal energy for the transfer of water to air transfer
    DeltaUaw <- (a*MW.PCB-b*nOrtho.Cl+c)*1000 # [J/mol]
    # Transform HLC to K
    K <- 10^(H0)*101325/(R*T) # [Lwater/Lair]
    # Correct K using water temperature
    K.air.water <- K*exp(-(DeltaUaw/R)*(1/(T.water + 273.15) - 1/T)) # [Lwater/Lair]
    # Final K (corrected by air and water temperature)
    K.final <- K.air.water*(T.water + 273.15)/(T.air + 273.15) # [Lwater/Lair]
    
    # KDOC calculation and correction
    # PCB internal energy for the transfer of octanol-air
    DeltaUoa <- (-a2*MW.PCB+b2*nOrtho.Cl-c2)*1000 # [J/mol]
    # PCB internal energy for the transfer of octanol-water
    DeltaUow <- DeltaUoa + DeltaUaw # [J/mol]
    # Octanol-water partition coefficient corrected by water temperature
    Kow.water.t <- 10^(Kow)*exp(-(DeltaUow/R)*(1/(T.water + 273.15)-1/T)) # [Loctanol/Lwater]
    # DOC-water partition coefficient
    Kdoc.t <- 0.06*Kow.water.t # [Lwater/kgdoc]
    
    # Freely dissolved water concentration calculations
    C.PCB.water.f <- C.PCB.water/(1 + Kdoc.t*DOC/1000^2) # [ng/m3]
    
    # Air-water mass transfer calculations
    # (1) Air side mass transfer calculations
    # Water diffusivity in air corrected by air
    # temperature and atmospheric pressure
    D.water.air <- (10^(-3)*1013.25*((273.15+T.air)^1.75*((1/28.97) +
                                                            (1/18.0152))^(0.5))/P/(20.1^(1/3) + 9.5^(1/3))^2) # [cm2/s]
    # PCB diffusivity in air
    D.PCB.air <- D.water.air*(MW.PCB/18.0152)^(-0.5) # [cm2/s]
    # Water vapor exchange velocity in air (from eq. 20-15)
    V.water.air <- 0.2*u + 0.3 # u @10 meter [cm/s]
    # Air side mass transfer
    V.PCB.air <- V.water.air*(D.PCB.air/D.water.air)^(2/3) # [cm/s]
    
    # (2) Water side mass transfer calculations
    # Viscosity of water at water temperature
    visc.water <- 10^(-4.5318-220.57/(149.39 - (273.15 + T.water)))
    # Water density corrected at water temperature
    dens.water <- (999.83952+16.945176*T.water - 7.9870401*10^-3*T.water^2
                   - 46.170461*10^-6*3 + 105.56302*10^-9*T.water^4 -
                     280.54253*10^-12*T.water^5)/(1 + 16.87985*10^-3*T.water)
    # Kinematic viscosity of water
    v.water <- visc.water/dens.water*10000 # [cm2/s]
    # CO2 diffusivity in water at water temperature
    diff.co2 <- 0.05019*exp(-19.51*1000/(273.15 + T.water)/R) # [cm2/s]
    # PCB diffusivity in water 
    D.PCB.water <- diff.co2*(MW.PCB/44.0094)^(-0.5) # [cm2/s]
    # PCB Schmidt number in water
    Sc.PCB.water <- v.water/D.PCB.water
    # CO2 Schmidt number in water
    Sc.co2.water <- v.water/diff.co2
    # k600 calculations, u in [m/s], k600 originally [cm/h]
    k600 <- (4.46 + 7.11*u)/60/60 #[cm/s]
    # Water side mass transfer (from eq. 20-24)
    if(u > 5){
      V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-0.5)  
    } else {
      V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-2/3)
    } # [cm/s]
    
    # Air-water mass transfer
    mtc.PCB <- ((1/V.PCB.water+1/(V.PCB.air*K.final)))^(-1) # [cm/s]
    # Flux calculations
    F.PCB.aw <- c(F.PCB.aw, mtc.PCB*(C.PCB.water.f)*(60*60*24)/100) # [ng/m2/d]
    
  }
  
  F.PCB.aw
  
}

# Final calculations ------------------------------------------------------

Num.Congener <- length(Congener)

result <- NULL
for (i in 1:Num.Congener) {
  result <- rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
                                       C.PCB.water.ave[i], C.PCB.water.error[i], nOrtho.Cl[i],
                                       Kow.mean[i], Kow.error[i]))
}

# Sum of all congeners per repetition
final.result <- data.frame(colSums(result, na.rm = TRUE))

# Summary of the total PCBs
mmm <- mean(final.result$colSums.result.)
sss <- sd(final.result$colSums.result.)
q2.5 <- quantile(final.result$colSums.result., 0.025)
q97.5 <- quantile(final.result$colSums.result., 0.975)
tPCBFlux <- c(mmm, sss, q2.5, q97.5)
names(tPCBFlux) <- c("Mean (ng/m2/d)", "Std (ng/m2/d)",
                     "2.5%CL (ng/m2/d)", "97.5%CL (ng/m2/d)")

print(tPCBFlux)

# Plots -------------------------------------------------------------------
# Histogram
{
  hist(as.numeric(final.result[,1]), main = "Volatilization Flux Total PCBs",
       xlab = "Volatilization Flux Total PCB (ng/m2/d)", border = "blue", col = "green",
       xlim = c(min(as.numeric(final.result[,1])), max(as.numeric(final.result[,1]))))
  abline(v = median(as.numeric(final.result[,1])), col = "blue", lwd = 3)
  abline(v = quantile(as.numeric(final.result[,1]), 0.025), col = "red", lwd = 3)
  abline(v = quantile(as.numeric(final.result[,1]), 0.975), col = "red", lwd = 3)
  abline(v = 0, col = "black", lwd = 3)
}

