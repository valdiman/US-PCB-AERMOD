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
    "PCB1", "PCB2", "PCB3", "PCB4", "PCB5", "PCB6", "PCB7", "PCB8", "PCB9", "PCB10",
    "PCB11", "PCB12+13", "PCB14", "PCB15", "PCB16", "PCB17", "PCB18+30", "PCB19", "PCB20+28", "PCB21+33",
    "PCB22", "PCB23", "PCB24", "PCB25", "PCB26+29", "PCB27", "PCB31", "PCB32", "PCB34", "PCB35",
    "PCB36", "PCB37", "PCB38", "PCB39", "PCB40+41+71", "PCB42", "PCB43", "PCB44+47+65", "PCB45+51", "PCB46",
    "PCB48", "PCB49+69", "PCB50+53", "PCB52", "PCB54", "PCB55", "PCB56", "PCB57", "PCB58", "PCB59+62+75",
    "PCB60", "PCB61+70+74+76", "PCB63", "PCB64", "PCB66", "PCB67", "PCB68", "PCB72", "PCB73", "PCB77",
    "PCB78", "PCB79", "PCB80", "PCB81", "PCB82", "PCB83+99", "PCB84", "PCB85+116+117", "PCB86+87+97+109+119+125", "PCB88+91",
    "PCB89", "PCB90+101+113", "PCB92", "PCB93+95+98+100+102", "PCB94", "PCB96", "PCB103", "PCB104", "PCB105", "PCB106",
    "PCB107", "PCB108+124", "PCB110+115", "PCB111", "PCB112", "PCB114", "PCB118", "PCB120", "PCB121", "PCB122",
    "PCB123", "PCB126", "PCB127", "PCB128+166", "PCB129+138+160+163", "PCB130", "PCB131", "PCB132", "PCB133", "PCB134+143",
    "PCB135+151+154", "PCB136", "PCB137", "PCB139+140", "PCB141", "PCB142", "PCB144", "PCB145", "PCB146", "PCB147+149",
    "PCB148", "PCB150", "PCB152", "PCB153+168", "PCB155", "PCB156+157", "PCB158", "PCB159", "PCB161", "PCB162",
    "PCB164", "PCB165", "PCB167", "PCB169", "PCB170", "PCB171+173", "PCB172", "PCB174", "PCB175", "PCB176",
    "PCB177", "PCB178", "PCB179", "PCB180+193", "PCB181", "PCB182", "PCB183+185", "PCB184", "PCB186", "PCB187",
    "PCB188", "PCB189", "PCB190", "PCB191", "PCB192", "PCB194", "PCB195", "PCB196", "PCB197+200", "PCB198+199",
    "PCB201", "PCB202", "PCB203", "PCB204", "PCB205", "PCB206", "PCB207", "PCB208", "PCB209"),
  MW.PCB = c(
    rep(188.644, 3),
    rep(223.088, 11),
    rep(257.532, 21),
    rep(291.976, 30),
    rep(326.42, 29),
    rep(360.864, 30),
    rep(395.308, 21),
    rep(429.752, 10),
    rep(465.740544, 3),
    498.64),
  nOrtho.Cl = c(
    1, 0, 0, 2, 1, 1, 1, 1, 1, 2,
    0, 0, 0, 0, 2, 2, 2, 3, 1, 1,
    1, 1, 2, 1, 1, 2, 1, 2, 1, 0,
    0, 0, 0, 0, 2, 2, 2, 2, 3, 3,
    2, 2, 3, 2, 4, 1, 1, 1, 1, 2,
    1, 1, 1, 2, 1, 1, 1, 1, 2, 0,
    0, 0, 0, 0, 2, 2, 3, 2, 2, 3,
    3, 2, 2, 3, 3, 4, 3, 4, 1, 2,
    1, 1, 2, 1, 2, 1, 1, 1, 2, 1,
    1, 0, 0, 2, 2, 2, 3, 3, 2, 3,
    3, 4, 2, 3, 2, 3, 3, 4, 2, 3,
    3, 4, 4, 2, 4, 1, 2, 1, 2, 1,
    2, 2, 1, 0, 2, 3, 2, 3, 3, 4,
    3, 3, 4, 2, 3, 3, 3, 4, 4, 3,
    4, 1, 2, 2, 2, 1, 3, 3, 4, 3,
    4, 4, 3, 4, 2, 3, 4, 4, 4),
  H0.mean = c(
    -3.526, -3.544, -3.562, -3.483, -3.622, -3.486, -3.424, -3.518, -3.49, -3.373,
    -3.537, -3.595, -3.376, -3.649, -3.6, -3.428, -3.495, -3.355, -3.544, -3.62,
    -3.719, -3.497, -3.5, -3.5, -3.526, -3.393, -3.562, -3.407, -3.375, -3.3745,
    -3.473, -3.818, -3.634, -3.524, -3.503, -3.592, -3.475, -3.638, -3.45, -3.47,
    -3.519, -3.452, -3.366, -3.496, -3.242, -3.739, -3.82, -3.568, -3.602, -3.517,
    -3.816, -3.694, -3.615, -3.565, -3.693, -3.631, -3.424, -3.441, -3.284, -3.989,
    -3.787, -3.705, -3.426, -3.844, -3.835, -3.603, -3.6, -3.716, -3.736, -3.461,
    -3.526, -3.61, -3.585, -3.523, -3.407, -3.387, -3.298, -3.13, -4.003, -3.783,
    -3.798, -3.768, -3.707, -3.574, -3.574, -3.845, -3.901, -3.61, -3.253, -3.901,
    -3.759, -4.087, -3.807, -3.984, -3.886, -3.817, -3.616, -3.693, -3.691, -3.639,
    -3.548, -3.492, -3.731, -3.483, -3.76, -3.502, -3.529, -3.328, -3.727, -3.625,
    -3.367, -3.296, -3.369, -3.783, -3.075, -4.053, -3.782, -3.808, -3.545, -3.881,
    -3.754, -3.56, -3.959, -4.186, -4.059, -3.763, -3.924, -3.772, -3.651, -3.527,
    -3.787, -3.671, -3.56, -3.969, -3.638, -3.59, -3.696, -3.339, -3.434, -3.693,
    -3.353, -3.177, -3.95, -3.876, -3.718, -4.174, -3.926, -3.884, -3.619, -3.644,
    -3.884, -3.651, -3.853, -3.463, -4.059, -4.059, -3.772, -3.777, -3.948),
  H0.error = rep(0.662, 159),
  Kow.mean = c(
    4.46, 4.69, 4.69, 4.65, 4.97, 5.06, 5.07, 5.07, 5.06, 4.84, 5.28, 5.29, 5.28, 5.3, 5.16, 
    5.25, 5.24, 5.02, 5.67, 5.6, 5.58, 5.57, 5.35, 5.67, 5.66, 5.44, 5.67, 5.44, 5.66, 5.82, 
    5.88, 5.83, 5.76, 5.89, 5.98, 5.76, 5.75, 5.75, 5.53, 5.53, 5.78, 5.85, 5.62, 5.84, 5.21, 
    6.11, 6.11, 6.17, 6.17, 5.95, 6.11, 6.2, 6.17, 5.95, 6.2, 6.2, 6.26, 6.26, 6.04, 6.36, 6.35, 
    6.42, 6.48, 6.36, 6.2, 6.39, 6.04, 6.3, 6.29, 6.13, 6.07, 6.38, 6.35, 6.13, 6.13, 5.71, 6.22, 
    5.81, 6.65, 6.64, 6.71, 6.73, 6.48, 6.76, 6.45, 6.65, 6.74, 6.79, 6.64, 6.64, 6.74, 6.89, 6.95, 
    6.74, 6.83, 6.8, 6.58, 6.58, 6.86, 6.55, 6.64, 6.22, 6.83, 6.67, 6.82, 6.51, 6.67, 6.25, 6.89, 
    6.67, 6.73, 6.32, 6.22, 6.92, 6.41, 7.18, 7.02, 7.24, 7.08, 7.24, 7.02, 7.05, 7.27, 7.42, 7.27, 
    7.11, 7.33, 7.11, 7.17, 6.76, 7.08, 7.14, 6.73, 7.36, 7.11, 7.2, 7.2, 6.85, 6.69, 7.17, 6.82, 
    7.71, 7.46, 7.55, 7.52, 7.8, 7.56, 7.65, 7.27, 7.2, 7.62, 7.24, 7.65, 7.3, 8, 8.09, 7.74, 7.71, 8.18),
  Kow.error = rep(0.32, 159)
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
# Average of the three highest samples
# This approach was used in the paper to estimate emissions
# and used in AERMOD
# WCPCB_OR-POH003 8/22/18, WCPCB_OR-POH004 8/23/18 &
# WCPCB_OR-POH005 8/21/18
wc <- data.frame(
  SampleID = c("WCPCB_OR-POH003-20180822.1", "WCPCB_OR-POH004-20180823.1", "WCPCB_OR-POH005-20180821.1"),
  LocationName = c("Willamette River at Kingsley Community Garden", 
                   "Willamette River at south Burlington Northern Railroad Bridge 5.1",
                   "Willamette River at NW Front Ave"),
  LocationID = c("WCPCB_OR-POH003", "WCPCB_OR-POH004", "WCPCB_OR-POH005"),
  SampleDate = c("8/22/2018", "8/23/2018", "8/21/2018"),
  Latitude = c(45.6068885, 45.5759428, 45.5583833),
  Longitude = c(-122.7841601, -122.7451604, -122.7186394),
  Units = rep("pg/L", 3),
  PCB1 = c(1.1425, 1.201096437, 1.565),
  PCB2 = c(0.2115, 0.184649888, 0.17325),
  PCB3 = c(0.385, 0.378769001, 0.5375),
  PCB4 = c(15.85, 15.62422128, 35.5),
  PCB5 = c(0.11675, 0.097931722, 0.07375),
  PCB6 = c(1.145, 0.862197857, 1.4525),
  PCB7 = c(0.4375, 0.33889858, 0.6125),
  PCB8 = c(4.975, 3.688013955, 6.175),
  PCB9 = c(0.3375, 0.266633441, 0.28),
  PCB10 = c(0.9175, 0.88711687, 1.415),
  PCB11 = c(11.5, 8.721654622, 7.45),
  `PCB12+13` = c(0.3425, 0.279092948, 0.4725),
  PCB14 = c(0.033, 0.026663344, 0.02725),
  PCB15 = c(1.965, 1.305756292, 2.045),
  PCB16 = c(2.8, 2.24520309, 3.025),
  PCB17 = c(6.45, 6.279591328, 8.525),
  `PCB18+30` = c(6.55, 5.606777972, 7.325),
  PCB19 = c(13.425, 15.12584102, 27),
  `PCB20+28` = c(7.375, 4.535260404, 5.925),
  `PCB21+33` = c(7.225, 5.35758784, 7.225),
  PCB22 = c(2.4925, 1.612260154, 1.99),
  PCB23 = c(0.0295, 0.01741839, 0.02575),
  PCB24 = c(0.1125, 0.090456018, 0.1145),
  PCB25 = c(2.3375, 2.105656616, 1.91),
  `PCB26+29` = c(1.3475, 1.001744331, 1.4775),
  PCB27 = c(0.9525, 1.123847496, 1.6325),
  PCB31 = c(5.575, 3.613256915, 4.925),
  PCB32 = c(2.3575, 2.432095689, 3.95),
  PCB34 = c(0.0585, 0.05681535, 0.045),
  PCB35 = c(0.11425, 0.05731373, 0.07625),
  PCB36 = c(0.03825, 0.015325193, 0.0475),
  PCB37 = c(1.1475, 0.682780962, 0.8725),
  PCB38 = c(0.02775, 0.01642163, 0.024225),
  PCB39 = c(0.065, 0.070769998, 0.05525),
  `PCB40+41+71` = c(3.825, 2.840767506, 2.49),
  PCB42 = c(1.7575, 1.308248193, 1.0775),
  PCB43 = c(0.3275, 0.17069524, 0.141),
  `PCB44+47+65` = c(28.5, 24.09668577, 26.25),
  `PCB45+51` = c(97, 79.74084226, 104.75),
  PCB46 = c(0.945, 0.742586594, 0.8425),
  PCB48 = c(1.1475, 0.787440817, 0.7575),
  `PCB49+69` = c(5.9, 5.108397707, 4.45),
  `PCB50+53` = c(2.95, 3.139795664, 3.275),
  PCB52 = c(9.675, 8.347869424, 7.375),
  PCB54 = c(1.27, 1.497632694, 1.715),
  PCB55 = c(0.014275, 0.020358834, 0.063),
  PCB56 = c(1.685, 1.402940444, 1.0575),
  PCB57 = c(0.03425, 0.018714179, 0.0117),
  PCB58 = c(0.02575, 0.02172938, 0.01225),
  `PCB59+62+75` = c(0.68, 0.515823573, 0.4525),
  PCB60 = c(0.7675, 0.66035385, 0.475),
  `PCB61+70+74+76` = c(6.375, 4.958883628, 4.15),
  PCB63 = c(0.17325, 0.141789185, 0.09825),
  PCB64 = c(2.65, 2.005980563, 1.5975),
  PCB66 = c(3.85, 3.139795664, 2.2875),
  PCB67 = c(0.0895, 0.060553202, 0.063),
  PCB68 = c(16.375, 9.269872913, 9.075),
  PCB72 = c(0.111, 0.108896088, 0.0555),
  PCB73 = c(0.193, 0.156740593, 0.15875),
  PCB77 = c(0.2295, 0.216546225, 0.1375),
  PCB78 = c(0.014075, 0.020059806, 0.012525),
  PCB79 = c(0.0115, 0.125841017, 0.010225),
  PCB80 = c(0.01285, 0.018290556, 0.011425),
  PCB81 = c(0.02095, 0.015574383, 0.01),
  PCB82 = c(0.58, 0.503364067, 0.32),
  `PCB83+99` = c(3.3, 3.040119611, 2.005),
  PCB84 = c(1.84, 1.749314727, 1.1525),
  `PCB85+116+117` = c(0.835, 0.784948916, 0.5175),
  `PCB86+87+97+109+119+125` = c(3.6, 3.314228757, 2.1925),
  `PCB88+91` = c(1.3375, 1.345626713, 0.875),
  PCB89 = c(0.0785, 0.068776476, 0.04575),
  `PCB90+101+113` = c(5.925, 5.681535011, 3.725),
  PCB92 = c(1.305, 1.255918266, 0.7575),
  `PCB93+95+98+100+102` = c(7.525, 7.201594817, 4.85),
  PCB94 = c(0.12475, 0.154497882, 0.08375),
  PCB96 = c(0.1395, 0.159730875, 0.1055),
  PCB103 = c(0.236, 0.269125343, 0.1695),
  PCB104 = c(0.05125, 0.057562921, 0.0425),
  PCB105 = c(0.97, 0.844754548, 0.6225),
  PCB106 = c(0.01335, 0.007376028, 0.016375),
  PCB107 = c(0.2725, 0.254173935, 0.20175),
  `PCB108+124` = c(0.1335, 0.106653377, 0.0785),
  `PCB110+115` = c(6.025, 5.382506853, 3.825),
  PCB111 = c(0.00945, 0.010690257, 0.014625),
  PCB112 = c(0.00915, 0.006279591, 0.0139),
  PCB114 = c(0.05375, 0.050834787, 0.04075),
  PCB118 = c(2.625, 2.317468228, 1.745),
  PCB120 = c(0.022625, 0.030650386, 0.013775),
  PCB121 = c(0.011825, 0.012285074, 0.0148),
  PCB122 = c(0.04075, 0.034139048, 0.032),
  PCB123 = c(0.05525, 0.043608273, 0.0305),
  PCB126 = c(0.01605, 0.00884625, 0.0172),
  PCB127 = c(0.014975, 0.008298031, 0.01665),
  `PCB128+166` = c(0.4325, 0.401196113, 0.2525),
  `PCB129+138+160+163` = c(3.3, 3.563418889, 2.2275),
  PCB130 = c(0.21375, 0.194119113, 0.126),
  PCB131 = c(0.04875, 0.031896337, 0.0275),
  PCB132 = c(1.41, 1.360578121, 0.9475),
  PCB133 = c(0.085, 0.096187391, 0.0645),
  `PCB134+143` = c(0.265, 0.247694991, 0.1675),
  `PCB135+151+154` = c(1.905, 2.108148517, 1.38),
  PCB136 = c(0.7525, 0.824819337, 0.565),
  PCB137 = c(0.09875, 0.086967356, 0.06925),
  `PCB139+140` = c(0.07775, 0.073760279, 0.0405),
  PCB141 = c(0.6275, 0.613007725, 0.46),
  PCB142 = c(0.015975, 0.012733616, 0.010625),
  PCB144 = c(0.1915, 0.217542985, 0.144),
  PCB145 = c(0.002155, 0.0021231, 0.0022475),
  PCB146 = c(0.665, 0.740094692, 0.4475),
  `PCB147+149` = c(4.3, 4.410665338, 2.9),
  PCB148 = c(0.021125, 0.025417393, 0.01655),
  PCB150 = c(0.028, 0.030401196, 0.019775),
  PCB152 = c(0.0116, 0.016247197, 0.01295),
  `PCB153+168` = c(3, 3.065038624, 2.0375),
  PCB155 = c(0.003875, 0.005457264, 0.004875),
  `PCB156+157` = c(0.2115, 0.189633691, 0.12625),
  PCB158 = c(0.265, 0.261649639, 0.173),
  PCB159 = c(0.02775, 0.027909295, 0.0073),
  PCB161 = c(0.0114, 0.008945926, 0.007575),
  PCB162 = c(0.01005, 0.008123598, 0.006675),
  PCB164 = c(0.25, 0.243458759, 0.16225),
  PCB165 = c(0.01255, 0.010117119, 0.00835),
  PCB167 = c(0.082, 0.070520807, 0.05175),
  PCB169 = c(0.012, 0.010191876, 0.007975),
  PCB170 = c(0.2875, 0.346374284, 0.211),
  `PCB171+173` = c(0.136, 0.134562671, 0.08425),
  PCB172 = c(0.07225, 0.074507849, 0.05025),
  PCB174 = c(0.4975, 0.493396462, 0.315),
  PCB175 = c(0.02035, 0.020333915, 0.012325),
  PCB176 = c(0.07275, 0.07600299, 0.0465),
  PCB177 = c(0.227, 0.20782457, 0.14725),
  PCB178 = c(0.12975, 0.135061052, 0.094),
  PCB179 = c(0.2825, 0.331422876, 0.204),
  `PCB180+193` = c(0.9125, 1.039122851, 0.6425),
  PCB181 = c(0.007325, 0.007874408, 0.0033),
  PCB182 = c(0.00475, 0.002616496, 0.0021375),
  `PCB183+185` = c(0.32, 0.363817593, 0.2195),
  PCB184 = c(0.002155, 0.002741091, 0.0022875),
  PCB186 = c(0.002155, 0.0021231, 0.0021375),
  PCB187 = c(0.76, 0.779965113, 0.38),
  PCB188 = c(0.002155, 0.0021231, 0.003),
  PCB189 = c(0.010075, 0.011487665, 0.00685),
  PCB190 = c(0.0745, 0.068527286, 0.04425),
  PCB191 = c(0.01485, 0.014353352, 0.006825),
  PCB192 = c(0.002155, 0.002434588, 0.0021375),
  PCB194 = c(0.12575, 0.108646898, 0.07825),
  PCB195 = c(0.0495, 0.058808871, 0.03),
  PCB196 = c(0.08575, 0.066782955, 0.04675),
  `PCB197+200` = c(0.0255, 0.029653626, 0.01855),
  `PCB198+199` = c(0.2395, 0.178170944, 0.12675),
  PCB201 = c(0.02925, 0.024570147, 0.012625),
  PCB202 = c(0.04975, 0.047595315, 0.033),
  PCB203 = c(0.159, 0.105656616, 0.084),
  PCB204 = c(0.002155, 0.0021231, 0.0021375),
  PCB205 = c(0.004875, 0.006952405, 0.004425),
  PCB206 = c(0.13575, 0.064789434, 0.055),
  PCB207 = c(0.024475, 0.01440319, 0.0113),
  PCB208 = c(0.059, 0.027909295, 0.017125),
  PCB209 = c(0.04725, 0.045850984, 0.02575),
  stringsAsFactors = FALSE, check.names = FALSE
)

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

