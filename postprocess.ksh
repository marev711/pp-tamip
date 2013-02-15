#! /usr/bin/ksh

set -xv

module load cdo  ##this command works only on korn shell
module load nco

year="$1"
month="$2"
exp="$3"
ens_exp="$4"
prefixGG=ICMGGb0if  
prefixSH=ICMSHb0if

nvariablesGG=77
nvariablesSH=8
set -A variablesGG 93 94 101 31 33 34 35 39 40 41 42 44 45 49 60 78 79 133 137 139 141 143 142 144 145 146 147 151 164 165 166 167 168 169 170 172 173 174 175 176 \
177 178 179 180 181 182 183 186 187 188 189 197 198 201 202 205 208 209 210 211 235 236 238 243 246 247 248 102 103 104 105 106 96 97 98 99 100
set -A namesGG_old "93" "94" "101" "ci" "rsn" "sst" "istl1" "swvl1" "swvl2" "swvl3" "swvl4" "ES" \
"smlt" "10fg" "pv" "var78" "var79" "Q" "TCWV" "stl1" "sd" "CP" "LSP" "SF" \
"bld" "SSHF" "SLHF" "MSL" "TCC" "U10M" "V10M" "T2M" "2d" "SSRD" "stl2" "lsm" "sr" \
"al" "STRD" "SSR" "STR" "TSR" "TTR" "EWSS" "NSSS" "E" "stl3" "lcc" "mcc" "hcc" \
"sund" "gwd" "src" "MX2T" "MN2T" "ro" "TSRC" "TTRC" "SSRC" "STRC" "SKT" "stl4" \
"tsn" "fal" "CLWC" "CIWC" "CC" "var102" "var103" "var104" "var105" \
"var106" "var96" "var97" "var98" "var99" "var100"
set -A namesGG "93" "94" "101" "ci" "rsn" "sst" "istl1" "swvl1" "swvl2" "swvl3" "swvl4" "sbl" \
"smlt" "10fg" "pv" "clwvi" "clivi" "hus" "prw" "stl1" "sd" "prc" "pr" "prsn" \
"bld" "hfss" "hfls" "psl" "clt" "uas" "vas" "ts" "2d" "rsds" "stl2" "lsm" "sr" \
"al" "rlds" "rsus" "rlus" "rsut" "rlut" "tauu" "tauv" "evspsbl" "stl3" "lcc" "mcc" "hcc" \
"sund" "gwd" "src" "tasmax" "tasmin" "ro" "rsutcs" "rlutcs" "rsuscs" "rldscs" "tas" "stl4" \
"tsn" "fal" "clw" "cli" "cl" "tntmp" "tntr" "tnta" "tnhusmp" \
"tnhusa" "mcu" "mcd" "reffclwc" "reffclic" "hur"
#clisccp has uncorrect name of the variable: ctptau=clisccp
set -A longnameGG "" "" "" "" "" "" "" "" "" "" "" "Surface Snow and Ice Sublimation Flux" \
"" "" "" "Condensed Water Path" "Ice Water Path" "Specific Humidity" \
"Water Vapor Path" "" "" "Convective Precipitation" "Precipitation" "Solid Precipitation" \
"" "Surface Upward Sensible Heat Flux" "Surface Upward Latent Heat Flux" "Sea Level Pressure" \
"Total Cloud Fraction" "Eastward Near-Surface Wind" "Northward Near-Surface Wind" \
"Surface Temperature" "" "Surface Downwelling Shortwave Radiation" "" "" "" "" \
"Surface Downwelling Longwave Radiation" "Surface Upwelling Shortwave Radiation" \
"Surface Upwelling Longwave Radiation" "TOA Outgoing Shortwave Radiation" \
"TOA Outgoing Longwave Radiation" "Surface Downward Eastward Wind Stress" \
"Surface Downward Northward Wind Stress" "Evaporation" "" "" "" "" "" "" "" \
"Daily Maximum Near-Surface Air Temperature" "Daily Minimum Near-Surface Air Temperature" \
"" "TOA Outgoing Clear-Sky Shortwave Radiation" "TOA Outgoing Clear-Sky Longwave Radiation" \
"Surface Upwelling Clear-Sky Shortwave Radiation" "Surface Downwelling Clear-Sky Longwave Radiation" \
"Near-Surface Air Temperature" "" "" "" "Mass Fraction of Cloud Liquid Water" "Mass Fraction of Cloud Ice" \
"Cloud Area Fraction" "Tendency of Air Temperature due to Diabatic Processes" \
"Tendency of Air Temperature due to Radiative Heating" "Tendency of Air Temperature due to Advection" \
"Tendency of Specific Humidity due to Model Physics" "Tendency of Specific Humidity due to Advection" \
"Updraft Convective Mass Flux on full levels" "Downdraft Convective Mass Flux on full levels" \
"Hydrometeor Effective Radius of Cloud Liquid Water" \
"Hydrometeor Effective Radius of Cloud Liquid Ice" "Relative Humidity"
set -A unitsGG "" "" "" "" "" "" "" "" "" "" "" "kg m**-2 s**-1" "" "" "" "kg m**-2" "kg m**-2" \
"1" "kg m**-2" "" "" "kg m**-2 s**-1" "kg m**-2 s**-1" "kg m**-2 s**-1" "" "W m**-2" \
"W m**-2" "Pa" "%" "m s**-2" "m s**-2" "K" "" "W m**-2" "" "" "" "" "W m**-2" "W m**-2" \
"W m**-2" "W m**-2" "W m**-2" "Pa" "Pa" "kg m**-2 s**-1" "" "" "" "" "" "" "" "K" "K" "" \
"W m**-2" "W m**-2" "W m**-2" "W m**-2" "K" "" "" "" "1" "1" "%" "K s**-1" "K s**-1" \
"K s**-1" "s**-1" "s**-1" "kg m**-2 s**-1" "kg m**-2 s**-1" "m" "m" "%"
set -A variablesSH 3 130 131 132 135 138 152 155
set -A namesSH_old "pt" "t" "u" "v" "w" "vo" "lnsp" "d"
set -A namesSH "pt" "ta" "ua" "va" "wap" "vo" "lnsp" "d"
set -A longnameSH "" "Air Temperature" "Eastward Wind" "Northward Wind" "omega (=dp/dt)" "" "" ""
set -A unitsSH "" "K" "m s**-1" "m s**-1" "Pa s**-1" "" "" ""
######################################################

names() {
 local var="$1"
 local exp="$2"
 ncatted -O -h -a experiment_id,global,o,c,${exp} ${var}
 ncatted -O -h -a institution,global,o,c,'EC-Earth (European Earth System Model)' ${var}
 ncatted -O -h -a source,global,o,c,'EC-EARTH 2.3 (2011); atmosphere: IFS (cy31R1+modifications, T159L62); land: HTessel' ${var}
 ncatted -O -h -a contact,global,o,c,'Alastair McKinstry <alastair.mckinstry@ichec.ie>' ${var}
 ncatted -O -h -a history,global,o,c,'AMIP simulation done by Frank Selten and Carlo Lacagnina' ${var}
 ncatted -O -h -a references,global,o,c,'Model described by Hazeleger et al. (Bull. Amer. Meteor. Soc., 2010, 91, 1357-1363). Also see http://ecearth.knmi.nl.' ${var}
 ncatted -O -h -a model_id,global,o,c,'EC-EARTH' ${var}
 ncatted -O -h -a forcing,global,o,c,'Nat,Ant' ${var}
 ncatted -O -h -a institute_id,global,o,c,'ICHEC' ${var}
}

names_amipac(){
 local var="$1"
 local namesGG_old="$2"
 local namesGG="$3"
 local longnameGG="$4"
 local unitsGG="$5"
 ncrename -O -v ${namesGG_old},${namesGG} ${var}
 ncatted -O -a long_name,${namesGG},a,c,"${longnameGG}" ${var}
 ncatted -O -a units,${namesGG},a,c,"${unitsGG}" ${var}
}

names_amip(){
 local var="$1"
 local namesGG_old="$2"
 local namesGG="$3"
 local longnameGG="$4"
 local unitsGG="$5"
 ncrename -O -v ${namesGG_old},${namesGG} ${var}
 ncatted -O -a long_name,${namesGG},o,c,"${longnameGG}" ${var}
 ncatted -O -a units,${namesGG},o,c,"${unitsGG}" ${var}
}

point_location() {
 local var="$1"
 local ave="$2"
 local suffix="$3"
 cdo sellonlatbox,289.6,290.4,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites1${suffix}
 cdo sellonlatbox,286.8,287.9,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites2${suffix}
 cdo sellonlatbox,284.6,285.4,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites3${suffix}
 cdo sellonlatbox,282.1,282.9,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites4${suffix}
 cdo sellonlatbox,279.6,280.4,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites5${suffix}
 cdo sellonlatbox,277.1,277.9,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites6${suffix}
 cdo sellonlatbox,274.5,275.4,-20.4,-19.6 ${var}${ave}${suffix} ${var}_cfSites7${suffix}
 cdo sellonlatbox,269.6,270.4,-18.1,-18.9 ${var}${ave}${suffix} ${var}_cfSites8${suffix}
 cdo sellonlatbox,264.4,265.6,-16.6,-17.4 ${var}${ave}${suffix} ${var}_cfSites9${suffix}
 cdo sellonlatbox,259.6,260.4,-15.1,-15.9 ${var}${ave}${suffix} ${var}_cfSites10${suffix}
 cdo sellonlatbox,254.6,255.4,-13.6,-14.4 ${var}${ave}${suffix} ${var}_cfSites11${suffix}
 cdo sellonlatbox,249.6,250.4,-12.1,-12.9 ${var}${ave}${suffix} ${var}_cfSites12${suffix}
 cdo sellonlatbox,244.6,245.4,-10.6,-11.4 ${var}${ave}${suffix} ${var}_cfSites13${suffix}
 cdo sellonlatbox,239.6,240.4,-9.1,-9.9 ${var}${ave}${suffix} ${var}_cfSites14${suffix}
 cdo sellonlatbox,234.5,235.3,-7.5,-8.6 ${var}${ave}${suffix} ${var}_cfSites15${suffix}
 cdo sellonlatbox,-123.4,-122.6,37.5,38.6 ${var}${ave}${suffix} ${var}_cfSites16${suffix}
 cdo sellonlatbox,234.6,235.4,34.6,35.4 ${var}${ave}${suffix} ${var}_cfSites17${suffix}
 cdo sellonlatbox,230.6,231.4,31.6,32.4 ${var}${ave}${suffix} ${var}_cfSites18${suffix}
 cdo sellonlatbox,226.6,227.4,28.5,29.4 ${var}${ave}${suffix} ${var}_cfSites19${suffix}
 cdo sellonlatbox,222.6,223.4,25.6,26.4 ${var}${ave}${suffix} ${var}_cfSites20${suffix}
 cdo sellonlatbox,218.6,219.4,22.6,23.4 ${var}${ave}${suffix} ${var}_cfSites21${suffix}
 cdo sellonlatbox,214.6,215.4,19.6,20.4 ${var}${ave}${suffix} ${var}_cfSites22${suffix}
 cdo sellonlatbox,210.4,211.6,16.6,17.4 ${var}${ave}${suffix} ${var}_cfSites23${suffix}
 cdo sellonlatbox,206.6,207.4,13.6,14.4 ${var}${ave}${suffix} ${var}_cfSites24${suffix}
 cdo sellonlatbox,202.5,203.4,10.6,11.4 ${var}${ave}${suffix} ${var}_cfSites25${suffix}
 cdo sellonlatbox,198.6,199.4,7.5,8.5 ${var}${ave}${suffix} ${var}_cfSites26${suffix}
 cdo sellonlatbox,194.6,195.4,4.6,5.4 ${var}${ave}${suffix} ${var}_cfSites27${suffix}
 cdo sellonlatbox,190.6,191.4,1.6,2.4 ${var}${ave}${suffix} ${var}_cfSites28${suffix}
 cdo sellonlatbox,186.6,187.4,-0.5,-1.4 ${var}${ave}${suffix} ${var}_cfSites29${suffix}
 cdo sellonlatbox,176.6,177.4,-0.5,-1.4 ${var}${ave}${suffix} ${var}_cfSites30${suffix}
 cdo sellonlatbox,166.5,167.3,-0.1,-0.9 ${var}${ave}${suffix} ${var}_cfSites31${suffix}
 cdo sellonlatbox,156.4,157.6,-1.6,-2.4 ${var}${ave}${suffix} ${var}_cfSites32${suffix}
 cdo sellonlatbox,147.0,147.8,-1.6,-2.5 ${var}${ave}${suffix} ${var}_cfSites33${suffix}
 cdo sellonlatbox,140.1,140.9,-4.35,-5.15 ${var}${ave}${suffix} ${var}_cfSites34${suffix}
 cdo sellonlatbox,135.0,135.9,-7.5,-8.5 ${var}${ave}${suffix} ${var}_cfSites35${suffix}
 cdo sellonlatbox,130.5,131.3,-12.0,-12.9 ${var}${ave}${suffix} ${var}_cfSites36${suffix}
 cdo sellonlatbox,-97.9,-97.1,36.0,36.8 ${var}${ave}${suffix} ${var}_cfSites37${suffix}
 cdo sellonlatbox,-157.0,-156.2,70.9,71.7 ${var}${ave}${suffix} ${var}_cfSites38${suffix}
 cdo sellonlatbox,-62.4,-61.6,-10.6,-11.4 ${var}${ave}${suffix} ${var}_cfSites39${suffix}
 cdo sellonlatbox,4.5,5.3,51.6,52.4 ${var}${ave}${suffix} ${var}_cfSites40${suffix}
 cdo sellonlatbox,144.6,145.4,-41.6,-42.4 ${var}${ave}${suffix} ${var}_cfSites41${suffix}
 cdo sellonlatbox,-51.4,-50.6,14.6,15.4 ${var}${ave}${suffix} ${var}_cfSites42${suffix}
 cdo sellonlatbox,-140.5,-139.4,29.6,30.4 ${var}${ave}${suffix} ${var}_cfSites43${suffix}
 cdo sellonlatbox,-145.4,-144.6,49.6,50.4 ${var}${ave}${suffix} ${var}_cfSites44${suffix}
 cdo sellonlatbox,-125.6,-124.8,7.5,8.5 ${var}${ave}${suffix} ${var}_cfSites45${suffix}
 cdo sellonlatbox,119.6,120.4,22.9,24.1 ${var}${ave}${suffix} ${var}_cfSites46${suffix}
 cdo sellonlatbox,-28.4,-27.6,38.6,39.4 ${var}${ave}${suffix} ${var}_cfSites47${suffix}
 cdo sellonlatbox,7.8,8.9,48.1,48.9 ${var}${ave}${suffix} ${var}_cfSites48${suffix}
 cdo sellonlatbox,116.4,117.2,32.0,33.1 ${var}${ave}${suffix} ${var}_cfSites49${suffix}
 cdo sellonlatbox,129.2,130.0,61.9,62.7 ${var}${ave}${suffix} ${var}_cfSites50${suffix}
 cdo sellonlatbox,91.5,92.3,30.8,31.9 ${var}${ave}${suffix} ${var}_cfSites51${suffix}
 cdo sellonlatbox,133.9,135.1,7.1,7.9 ${var}${ave}${suffix} ${var}_cfSites52${suffix}
 cdo sellonlatbox,13.5,14.6,51.8,52.6 ${var}${ave}${suffix} ${var}_cfSites53${suffix}
 cdo sellonlatbox,26.2,27.1,67.0,67.9 ${var}${ave}${suffix} ${var}_cfSites54${suffix}
 cdo sellonlatbox,-105.6,-104.6,53.6,54.4 ${var}${ave}${suffix} ${var}_cfSites55${suffix}
 cdo sellonlatbox,-63.0,-62.0,82.1,82.9 ${var}${ave}${suffix} ${var}_cfSites56${suffix}
 cdo sellonlatbox,-54.0,-53.0,-28.2,-29.0 ${var}${ave}${suffix} ${var}_cfSites57${suffix}
 cdo sellonlatbox,-24.4,-23.6,40.6,41.4 ${var}${ave}${suffix} ${var}_cfSites58${suffix}
 cdo sellonlatbox,-26.4,-25.6,34.6,35.4 ${var}${ave}${suffix} ${var}_cfSites59${suffix}
 cdo sellonlatbox,-29.4,-28.6,28.5,29.5 ${var}${ave}${suffix} ${var}_cfSites60${suffix}
 cdo sellonlatbox,-35.4,-34.6,11.6,12.4 ${var}${ave}${suffix} ${var}_cfSites61${suffix}
 cdo sellonlatbox,-56.9,-56.1,14.6,15.4 ${var}${ave}${suffix} ${var}_cfSites62${suffix}
 cdo sellonlatbox,-61.9,-61.1,17.4,18.6 ${var}${ave}${suffix} ${var}_cfSites63${suffix}
 cdo sellonlatbox,-119.9,-119.1,32.6,33.4 ${var}${ave}${suffix} ${var}_cfSites64${suffix}
 cdo sellonlatbox,-122.6,-121.4,31.0,32.0 ${var}${ave}${suffix} ${var}_cfSites65${suffix}
 cdo sellonlatbox,-85.5,-84.5,-2.1,-2.9 ${var}${ave}${suffix} ${var}_cfSites66${suffix}
 cdo sellonlatbox,-95.6,-94.4,-2.1,-2.9 ${var}${ave}${suffix} ${var}_cfSites67${suffix}
 cdo sellonlatbox,-105.4,-104.6,-2.1,-2.9 ${var}${ave}${suffix} ${var}_cfSites68${suffix}
 cdo sellonlatbox,-115.4,-114.6,-2.1,-2.9 ${var}${ave}${suffix} ${var}_cfSites69${suffix}
 cdo sellonlatbox,-125.4,-124.6,-2.1,-2.9 ${var}${ave}${suffix} ${var}_cfSites70${suffix}
 cdo sellonlatbox,-125.4,-124.6,17.4,18.6 ${var}${ave}${suffix} ${var}_cfSites71${suffix}
 cdo sellonlatbox,-69.4,-68.6,0.5,1.4 ${var}${ave}${suffix} ${var}_cfSites72${suffix}
 cdo sellonlatbox,61.6,62.4,12.6,13.4 ${var}${ave}${suffix} ${var}_cfSites73${suffix}
 cdo sellonlatbox,-14.8,-14.0,-7.47,-8.47 ${var}${ave}${suffix} ${var}_cfSites74${suffix}
 cdo sellonlatbox,149.6,150.4,36.4,37.5 ${var}${ave}${suffix} ${var}_cfSites75${suffix}
 cdo sellonlatbox,-22.5,-21.4,63.7,64.5 ${var}${ave}${suffix} ${var}_cfSites76${suffix}
 cdo sellonlatbox,-170.6,-169.8,56.55,57.65 ${var}${ave}${suffix} ${var}_cfSites77${suffix}
 cdo sellonlatbox,-59.4,-58.4,-61.8,-62.6 ${var}${ave}${suffix} ${var}_cfSites78${suffix}
 cdo sellonlatbox,11.45,12.45,78.53,79.33 ${var}${ave}${suffix} ${var}_cfSites79${suffix}
 cdo sellonlatbox,144.4,145.2,13.2,14.1 ${var}${ave}${suffix} ${var}_cfSites80${suffix}
 cdo sellonlatbox,68.8,69.8,-48.7,-49.7 ${var}${ave}${suffix} ${var}_cfSites81${suffix}
 cdo sellonlatbox,158.5,159.3,-54.2,-55.0 ${var}${ave}${suffix} ${var}_cfSites82${suffix}
 cdo sellonlatbox,-81.4,-80.6,26.6,27.5 ${var}${ave}${suffix} ${var}_cfSites83${suffix}
 cdo sellonlatbox,-168.1,-167.3,8.3,9.1 ${var}${ave}${suffix} ${var}_cfSites84${suffix}
 cdo sellonlatbox,89.6,90.4,11.6,12.4 ${var}${ave}${suffix} ${var}_cfSites85${suffix}
 cdo sellonlatbox,114.6,115.4,11.6,12.4 ${var}${ave}${suffix} ${var}_cfSites86${suffix}
 cdo sellonlatbox,-95.6,-94.4,9.5,10.5 ${var}${ave}${suffix} ${var}_cfSites87${suffix}
 cdo sellonlatbox,-23.6,-22.4,8.1,8.9 ${var}${ave}${suffix} ${var}_cfSites88${suffix}
 cdo sellonlatbox,-1.84,-1.04,50.74,51.54 ${var}${ave}${suffix} ${var}_cfSites89${suffix}
 cdo sellonlatbox,1.8,2.6,48.31,49.11 ${var}${ave}${suffix} ${var}_cfSites90${suffix}
 cdo sellonlatbox,93.3,94.1,-19.6,-20.6 ${var}${ave}${suffix} ${var}_cfSites91${suffix}
 cdo sellonlatbox,254.0,254.8,-58.1,-58.9 ${var}${ave}${suffix} ${var}_cfSites92${suffix}
 cdo sellonlatbox,-53.15,-52.35,47.27,48.07 ${var}${ave}${suffix} ${var}_cfSites93${suffix}
 cdo sellonlatbox,-177.0,-176.2,-39.75,-40.85 ${var}${ave}${suffix} ${var}_cfSites94${suffix}
 cdo sellonlatbox,72.0,72.8,-6.9,-7.7 ${var}${ave}${suffix} ${var}_cfSites95${suffix}
 cdo sellonlatbox,-10.28,-9.48,-39.75,-40.85 ${var}${ave}${suffix} ${var}_cfSites96${suffix}
 cdo sellonlatbox,188.7,189.5,37.8,38.7 ${var}${ave}${suffix} ${var}_cfSites97${suffix}
 cdo sellonlatbox,-150.0,-149.2,-17.1,-17.9 ${var}${ave}${suffix} ${var}_cfSites98${suffix}
 cdo sellonlatbox,-0.4,0.4,-55.4,-56.5 ${var}${ave}${suffix} ${var}_cfSites99${suffix}
 cdo sellonlatbox,273.1,273.9,-42.3,-43.2 ${var}${ave}${suffix} ${var}_cfSites100${suffix}
 cdo sellonlatbox,153.57,154.37,23.9,24.7 ${var}${ave}${suffix} ${var}_cfSites101${suffix}
 cdo sellonlatbox,167.5,168.3,-28.53,-29.53 ${var}${ave}${suffix} ${var}_cfSites102${suffix}
 cdo sellonlatbox,-40.5,-39.6,49.6,50.4 ${var}${ave}${suffix} ${var}_cfSites103${suffix}
 cdo sellonlatbox,87.57,88.37,65.38,66.18 ${var}${ave}${suffix} ${var}_cfSites104${suffix}
 cdo sellonlatbox,-0.4,0.4,-0.6,0.4 ${var}${ave}${suffix} ${var}_cfSites105${suffix}
 cdo sellonlatbox,2.1,2.9,3.0,4.0 ${var}${ave}${suffix} ${var}_cfSites106${suffix}
 cdo sellonlatbox,2.1,2.9,6.1,6.9 ${var}${ave}${suffix} ${var}_cfSites107${suffix}
 cdo sellonlatbox,1.6,2.4,9.1,9.9 ${var}${ave}${suffix} ${var}_cfSites108${suffix}
 cdo sellonlatbox,2.1,2.9,11.1,11.9 ${var}${ave}${suffix} ${var}_cfSites109${suffix}
 cdo sellonlatbox,1.8,2.6,13.0,14.1 ${var}${ave}${suffix} ${var}_cfSites110${suffix}
 cdo sellonlatbox,-1.9,-1.1,15.1,15.9 ${var}${ave}${suffix} ${var}_cfSites111${suffix}
 cdo sellonlatbox,2.1,2.9,17.5,18.6 ${var}${ave}${suffix} ${var}_cfSites112${suffix}
 cdo sellonlatbox,2.1,2.9,20.1,20.9 ${var}${ave}${suffix} ${var}_cfSites113${suffix}
 cdo sellonlatbox,5.1,5.9,22.6,23.4 ${var}${ave}${suffix} ${var}_cfSites114${suffix}
 cdo sellonlatbox,-17.4,-16.6,14.6,15.4 ${var}${ave}${suffix} ${var}_cfSites115${suffix}
 cdo sellonlatbox,-165.4,-164.6,75.6,76.4 ${var}${ave}${suffix} ${var}_cfSites116${suffix}
 cdo sellonlatbox,128.4,129.4,71.2,72.0 ${var}${ave}${suffix} ${var}_cfSites117${suffix}
 cdo sellonlatbox,109.6,110.4,87.6,88.4 ${var}${ave}${suffix} ${var}_cfSites118${suffix}
 cdo sellonlatbox,122.6,123.7,-74.5,-75.6 ${var}${ave}${suffix} ${var}_cfSites119${suffix}
 cdo sellonlatbox,-59.93,-58.93,12.66,13.66 ${var}${ave}${suffix} ${var}_cfSites120${suffix}
}

multilevel_output() {
 local var="$1"
 local ave="$2"
 local suffix="$3"
 local jend="$4"
 set -A names93 "sunlit" "cltisccp" "pctisccp" "albisccp" "tauisccp"
 set -A longname93 "sunlit" "ISCCP Total Cloud Fraction" "ISCCP Mean Cloud Top Pressure" "ISCCP Mean Cloud Albedo" \
 "ISCCP Mean Cloud Tau"
 set -A units93 "1" "%" "hPa" "1" "1"
 set -A names94 "cllcalipso" "clmcalipso" "clhcalipso" "cltcalipso"
 set -A longname94 "CALIPSO Low Level Cloud Fraction" "CALIPSO Mid Level Cloud Fraction" "CALIPSO High Level Cloud Fraction" \
 "CALIPSO Total Cloud Fraction"
 set -A units94 "%" "%" "%" "%"
 set -A names101 "rsdscs" "huss" "hurs" "cct" "ccb"
 set -A longname101 "Surface Downwelling Clear-Sky Shortwave Radiation" "Near-Surface Specific Humidity" \
 "Near-Surface Relative Humidity" "Air Pressure at Convective Cloud Top" "Air Pressure at Convective Cloud Base"
 set -A units101 "W m**-2" "1" "%" "Pa" "Pa"

 case ${var} in
  93)  ncks -O -d mlev,0 ${var}${ave}${suffix} ${names93[0]}${ave}${suffix} 
       ncwa -a mlev -O ${names93[0]}${ave}${suffix} ${names93[0]}${ave}${suffix} 

       ncks -O -d mlev,1 ${var}${ave}${suffix} temp1.nc
       ncwa -a mlev -O temp1.nc temp1.nc
       cdo mulc,100 temp1.nc temp2.nc
       cdo ifthen ${names93[0]}${ave}${suffix} temp2.nc ${names93[1]}${ave}${suffix}
       ncrename -O -v var${var},${names93[1]} ${names93[1]}${ave}${suffix}
       ncatted -O -a long_name,${names93[1]},a,c,"${longname93[1]}" ${names93[1]}${ave}${suffix}
       ncatted -O -a units,${names93[1]},a,c,"${units93[1]}" ${names93[1]}${ave}${suffix}
       ncatted -O -a ,global,d,, ${names93[1]}${ave}${suffix}
       ( names  ${names93[1]}${ave}${suffix} ${exp} )
       ( ave_function ${names93[1]} ${ave} ${suffix} )

      cdo setrtomiss,-1,0 ${names93[1]}_cfDay${suffix} isccp_tmp1_d_ml.nc
      cdo setrtomiss,-1,0 ${names93[1]}_Amon${suffix} isccp_tmp1_m_ml.nc
      for j in $(seq 2 `expr ${jend} - 1`); do 
       ncks -O -d mlev,${j} ${var}${ave}${suffix} temp${j}.nc
       ncwa -a mlev -O temp${j}.nc temp${j}.nc
       cdo ifthen ${names93[0]}${ave}${suffix} temp${j}.nc ${names93[j]}${ave}${suffix}
       ncrename -O -v var${var},${names93[j]} ${names93[j]}${ave}${suffix}
       ncatted -O -a long_name,${names93[j]},a,c,"${longname93[j]}" ${names93[j]}${ave}${suffix}
       ncatted -O -a units,${names93[j]},a,c,"${units93[j]}" ${names93[j]}${ave}${suffix}
       ncatted -O -a ,global,d,, ${names93[j]}${ave}${suffix}
       ( names ${names93[j]}${ave}${suffix} ${exp} )
       ( point_location ${names93[j]} ${ave} ${suffix} )
       cdo mul ${names93[j]}${ave}${suffix} ${names93[1]}${ave}${suffix} isccp_tmp_gridbox_h.nc
       cdo shifttime,-3hour isccp_tmp_gridbox_h.nc media.nc
       cdo daymean media.nc isccp_tmp_gridbox_d.nc 
       cdo monmean media.nc isccp_tmp_gridbox_m.nc
       cdo div isccp_tmp_gridbox_d.nc ${names93[1]}_cfDay${suffix} isccp_tmp${j}_d_ml.nc
       cdo ifthen isccp_tmp1_d_ml.nc isccp_tmp${j}_d_ml.nc ${names93[j]}_cfDay${suffix}
       cdo div isccp_tmp_gridbox_m.nc ${names93[1]}_Amon${suffix} isccp_tmp${j}_m_ml.nc
       cdo ifthen isccp_tmp1_m_ml.nc isccp_tmp${j}_m_ml.nc ${names93[j]}_Amon${suffix}
      done;;
##       cdo div ${names93[1]}_cfDay${suffix} ${names93[0]}_cfDay${suffix} isccp_tmp_d_div21.nc
##       ncks -O isccp_tmp_d_div21.nc ${names93[1]}_cfDay${suffix}
##       cdo div ${names93[1]}_Amon${suffix} ${names93[0]}_Amon${suffix} isccp_tmp_m_div21.nc
##       ncks -O isccp_tmp_m_div21.nc ${names93[1]}_Amon${suffix}
  94) for j in $(seq 0 `expr ${jend} - 1`); do 
       ncks -O -d mlev,${j} ${var}${ave}${suffix} ${names94[j]}${ave}${suffix}
       ncwa -a mlev -O ${names94[j]}${ave}${suffix} ${names94[j]}${ave}${suffix}
       cdo mulc,100 ${names94[j]}${ave}${suffix} tmp.nc
       mv tmp.nc ${names94[j]}${ave}${suffix}
       ncrename -O -v var${var},${names94[j]} ${names94[j]}${ave}${suffix}
       ncatted -O -a long_name,${names94[j]},a,c,"${longname94[j]}" ${names94[j]}${ave}${suffix}
       ncatted -O -a units,${names94[j]},a,c,"${units94[j]}" ${names94[j]}${ave}${suffix}
       ncatted -O -a ,global,d,, ${names94[j]}${ave}${suffix}
       ( names ${names94[j]}${ave}${suffix} ${exp} )
       ( ave_function ${names94[j]} ${ave} ${suffix} )
      done;;
 101) for j in $(seq 1 `expr ${jend} - 1`); do 
#      for j in $(seq 0 `expr ${jend} - 1`); do 
       ncks -O -d mlev,${j} ${var}${ave}${suffix} ${names101[j]}${ave}${suffix}
       ncwa -a mlev -O ${names101[j]}${ave}${suffix} ${names101[j]}${ave}${suffix}
       ncrename -O -v var${var},${names101[j]} ${names101[j]}${ave}${suffix}
       ncatted -O -a long_name,${names101[j]},a,c,"${longname101[j]}" ${names101[j]}${ave}${suffix}
       ncatted -O -a units,${names101[j]},a,c,"${units101[j]}" ${names101[j]}${ave}${suffix}
       ncatted -O -a ,global,d,, ${names101[j]}${ave}${suffix}
       ( names ${names101[j]}${ave}${suffix} ${exp} )
       ( ave_function ${names101[j]} ${ave} ${suffix} )
      done;;
 esac
}

cosp_parasol() { ##and calipso
 local var="$1"
 local ave="$2"
 local suffix="$3"
 multilevel_output ${var} ${ave} ${suffix} 4 ##lev=7 is radar and lidar combined, radar is not used in this run; correct mlev,7,11, because ncks starts from 0
 ncks -O -d mlev,7,11 ${var}${ave}${suffix} parasolRefl${ave}${suffix}
 ncrename -O -d mlev,sza parasolRefl${ave}${suffix}
 ncrename -O -v var${var},parasolRefl parasolRefl${ave}${suffix}
 ncatted -O -a long_name,parasolRefl,a,c,"PARASOL Reflectance" parasolRefl${ave}${suffix}
 ncatted -O -a units,parasolRefl,a,c,"1" parasolRefl${ave}${suffix}
 ncatted -O -a ,global,d,, parasolRefl${ave}${suffix}
 ( names parasolRefl${ave}${suffix} ${exp} )
 ( ave_function parasolRefl ${ave} ${suffix} )
}

cosp_isccp() {
 local var="$1"
 local ave="$2"
 local suffix="$3"
 local i1=5                ##should be from 6-49, but ncks starts from 0
 local i2=48
 multilevel_output ${var} ${ave} ${suffix} 5     ##correct even if ncks starts from 0, multilevel_output has been modified to take it into account
 for iq in $(seq 1 7); do
  ncks -O -d mlev,$i1,$i2,7 ${var}${ave}${suffix} isccp_tmp${iq}.nc
  for ir in $(seq 0 6); do
   ncks -O -d mlev,$ir isccp_tmp${iq}.nc isccp_tmp${iq}_h${ir}.nc
   ncrename -O -d mlev,ctp isccp_tmp${iq}_h${ir}.nc
   cdo ifthen sunlit_cf3hr${suffix} isccp_tmp${iq}_h${ir}.nc isccp_tmp${iq}_h${ir}_correct.nc
  done
  ncecat -O isccp_tmp${iq}_h[0-6]_correct.nc isccp_tmp${iq}_h_.nc
  ncrename -O -v var${var},clisccp isccp_tmp${iq}_h_.nc isccp_tmp${iq}_h.nc
  ncrename -O -d record,ctp isccp_tmp${iq}_h.nc
  ncatted -O -a long_name,clisccp,a,c,"ISCCP Cloud Area Fraction" isccp_tmp${iq}_h.nc
  ncatted -O -a units,clisccp,a,c,"%" isccp_tmp${iq}_h.nc
  ncap2 -O -s 'ctp[ctp]={1000,800,680,560,440,310,180}' isccp_tmp${iq}_h.nc isccp_tmp${iq}_h.nc
  ncatted -O -a long_name,ctp,c,c,"cloud top pressure" isccp_tmp${iq}_h.nc
  ncatted -O -a units,ctp,c,c,"hPa" isccp_tmp${iq}_h.nc
  ncatted -O -a axis,ctp,c,c,"Z" isccp_tmp${iq}_h.nc
  ncatted -O -a standard_name,ctp,c,c,"cloud top pressure" isccp_tmp${iq}_h.nc
  ncpdq -O -a time,ctp isccp_tmp${iq}_h.nc tmp.nc
  cdo mulc,100 tmp.nc isccp_tmp${iq}_h.nc
##  ( point_location '' '' isccp_tmp${iq}_h.nc)
  cdo shifttime,-3hour isccp_tmp${iq}_h.nc media.nc
##  cdo daymean media.nc isccp_tmp${iq}_d.nc
  cdo monmean media.nc isccp_tmp${iq}_m.nc
  i1=`expr $i1 + 1`
  i2=`expr $i2 + 1`
 done

 ncecat -O isccp_tmp[1-7]_h.nc clisccp${ave}${suffix}
## ncecat -O isccp_tmp[1-7]_d.nc clisccp_cfDay${suffix}
 ncecat -O isccp_tmp[1-7]_m.nc clisccp_Amon${suffix}
## for k in $(seq 1 120); do
##  ncecat -O _cfSites${k}isccp_tmp[1-7]_h.nc clisccp_cfSites${k}${suffix}
##  ncrename -O -d record,tau clisccp_cfSites${k}${suffix}
## done
 ncrename -O -d record,tau clisccp${ave}${suffix}
 ncatted -O -a ,global,d,, clisccp${ave}${suffix}
 ( names clisccp${ave}${suffix} ${exp} )
 ncrename -O -d record,tau clisccp_Amon${suffix}
## ncrename -O -d record,tau clisccp_cfDay${suffix}
##don't use ncap2 for the 3hr data, it uses too much memory and then crush
 ncap2 -O -s 'tau[tau]={0.0,.3,1.3,3.6,9.4,23.0,60.0}' clisccp_Amon${suffix} clisccp_Amon${suffix}
 ncatted -O -a long_name,tau,c,c,"cloud optical thickness" clisccp_Amon${suffix}
 ncatted -O -a units,tau,c,c,"1" clisccp_Amon${suffix}
 ncatted -O -a standard_name,tau,c,c,"cloud optical thickness" clisccp_Amon${suffix}
 ncpdq -O -a time,tau clisccp_Amon${suffix} clisccp_Amon${suffix}
 ncatted -O -a ,global,d,, clisccp_Amon${suffix}
 ( names clisccp_Amon${suffix} ${exp} )
}

ave_function() {
 local var="$1"
 local ave="$2"
 local suffix="$3"
 cdo shifttime,-3hour ${var}${ave}${suffix} media.nc
 cdo daymean media.nc ${var}_cfDay${suffix}
 cdo monmean media.nc ${var}_Amon${suffix}
 ( point_location ${var} ${ave} ${suffix} )
}

#########################################################

  cd $TEMP
  cd ${exp}${year}${month} 

#  cdo splitname ${prefixGG}+${year}${month} GG${year}${month}_
  for i in $(seq 0 `expr ${nvariablesGG} - 1`); do ##GG conversion and average
   cdo -R -r -f nc -t ecmwf copy GG${year}${month}_var${variablesGG[i]}.grb ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
   case ${variablesGG[i]} in
    93) ( cosp_isccp ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    94) ( cosp_parasol ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   101) ( multilevel_output ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc 5 );;
    44) cdo divc,10.8 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    78) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    79) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;   
    96) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    97) cdo mulc,-1 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    98) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
    99) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   100) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   102) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   103) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   104) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   105) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   106) ( names_amipac ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
#   129) cdo divc,9.81 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
#        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
#        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
#        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
#        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
#        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   133) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   137) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   142) cdo divc,10.8 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        cdo add tmp.nc ${namesGG[`expr ${i} - 1`]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp1.nc
        ncks -O tmp1.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   143) cdo divc,10.8 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        ncks -O tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   144) cdo divc,10.8 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        ncks -O tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   146) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        ncks -O tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   147) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        ncks -O tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   151) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   164) cdo mulc,100 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   165) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   166) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   167) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   169) cdo divc,10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        ncks -O tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   175) cdo divc,10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   176) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        cdo add tmp.nc rsds_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   177) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        cdo add tmp.nc rlds_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   178) cdo shifttime,-3hour ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo monmean media.nc tmp_m.nc
        cdo divc,-10800 tmp_m.nc tmp.nc #sign convention: up positive
        cdo add tmp.nc rsdt_Amon_EC-EARTH_amip_r3i1p1_${year}${month}.nc ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} );;
   179) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sing convention: up positive
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   180) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   181) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   182) cdo divc,-10.8 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   201) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        cdo shifttime,-3hour ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo daymax media.nc ${namesGG[i]}_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        cdo monmean ${namesGG[i]}_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc;;
   202) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        cdo shifttime,-3hour ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo daymin media.nc ${namesGG[i]}_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        cdo monmean ${namesGG[i]}_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc;;
   208) cdo shifttime,-3hour ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo monmean media.nc tmp_m.nc
        cdo divc,-10800 tmp_m.nc tmp.nc #sign convention: up positive
        cdo add tmp.nc rsdt_Amon_EC-EARTH_amip_r3i1p1_${year}${month}.nc ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} );;
   209) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
#   210) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc #sign convention: up positive
#        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
#        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
#        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
#        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   210) cdo divc,-10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc  rsnscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc #sign convention: up
        rm ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip rsnscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "rsnscs" "Surface net Clear-Sky Shortwave Radiation" \
        "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, rsnscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names rsnscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function rsnscs _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   211) cdo divc,10800 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        cdo add tmp.nc rlus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   235) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   246) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   247) ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   248) cdo mulc,100 ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
        mv tmp.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names_amip ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "${namesGG_old[i]}" "${namesGG[i]}" "${longnameGG[i]}" "${unitsGG[i]}" )
        ncatted -O -a ,global,d,, ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesGG[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
     *) cdo shifttime,-3hour ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo monmean media.nc ${namesGG[i]}_monthly_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc;;
   esac
  done 
  cdo add tntmp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnta_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names_amip tnt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tntmp tnt "Tendency of Air Temperature" "K s**-1" )
  ncatted -O -a ,global,d,, tnt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names tnt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
  ( ave_function tnt _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc )
  cdo add tnhusmp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnhusa_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnhus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names_amip tnhus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnhusmp tnhus "Tendency of Air Temperature" "K s**-1" )
  ncatted -O -a ,global,d,, tnhus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names tnhus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
  ( ave_function tnhus _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc )

########################calipso is on 40 levels, using ncks you get troubles with malloc()
param=95
grib_copy -w indicatorOfParameter=${param} ${prefixGG}+${year}${month} ICMGG${param}
cat > rules_file <<==
if ( level >= 23 ) {
  write "clcalipso";
}
==
grib_filter rules_file ICMGG${param}
cdo -R -f nc -t ecmwf copy clcalipso tmp.nc
cdo mulc,100 tmp.nc clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
ncrename -O -v var${param},clcalipso clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
ncatted -O -a long_name,clcalipso,a,c,"CALIPSO Cloud Area Fraction" clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
ncatted -O -a units,clcalipso,a,c,"%" clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
ncatted -O -a ,global,d,, clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
( names clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
( ave_function clcalipso _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc )
########################

##  cdo splitname ${prefixSH}+${year}${month} SH${year}${month}_
#cat > rulesSH <<-EOF
#write "SH${year}${month}_[shortName].grb";
#EOF
#  grib_filter rulesSH ${prefixSH}+${year}${month}

  for i in $(seq 0  `expr ${nvariablesSH} - 1`); do ##SH conversion and average
##   cdo -r -f nc -t ecmwf sp2gpl SH${year}${month}_var${variablesSH[i]}.grb SHcf3hr${year}${month}_${variablesSH[i]}.nc
   cdo -r -f nc -t ecmwf sp2gpl SH${year}${month}_${namesSH_old[i]}.grb ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
   case ${variablesSH[i]} in
   130) ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "T" "${namesSH[i]}" "${longnameSH[i]}" "${unitsSH[i]}" )
        ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "T_2" "ta_2" "${longnameSH[i]}" "${unitsSH[i]}" )
        ncatted -O -a ,global,d,, ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesSH[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   131) ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "U" "${namesSH[i]}" "${longnameSH[i]}" "${unitsSH[i]}" )
        ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "U_2" "ua_2" "${longnameSH[i]}" "${unitsSH[i]}" )
        ncatted -O -a ,global,d,, ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesSH[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   132) ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "V" "${namesSH[i]}" "${longnameSH[i]}" "${unitsSH[i]}" )
        ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "V_2" "va_2" "${longnameSH[i]}" "${unitsSH[i]}" )
        ncatted -O -a ,global,d,, ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesSH[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
   135) ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "W" "${namesSH[i]}" "${longnameSH[i]}" "${unitsSH[i]}" )
        ( names_amip ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "W_2" "wap_2" "${longnameSH[i]}" "${unitsSH[i]}" )
        ncatted -O -a ,global,d,, ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
        ( names ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
        ( ave_function ${namesSH[i]} _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc );;
     *) cdo shifttime,-3hour ${namesSH[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc media.nc
        cdo monmean media.nc ${namesSH[i]}_monthly_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc;;
   esac
  done
  grib_copy -w levelType=pl SH${year}${month}_z.grb SH${year}${month}_zpl.grb
  cdo -r -f nc -t ecmwf sp2gpl SH${year}${month}_zpl.grb zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  cdo divc,9.81 zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp.nc
  mv tmp.nc zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names_amip zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "Z" "zg" "Geopotential Height" "m" )
  ncatted -O -a ,global,d,, zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
  ( ave_function zg _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc )

  cdo splitname wap_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp
  ncks -O -d lev,5 tmpwap.nc wap500_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  cdo daymean wap500_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc wap500_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  cdo splitname ta_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp
  ncks -O -d lev,3 tmpta.nc ta700_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  cdo daymean ta700_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ta700_cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  cdo splitname lnsp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tmp
  cdo exp tmpLNSP_2.nc ps_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names_amip ps_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc "LNSP_2" "ps" "Surface Air Pressure" "Pa" )
  ncatted -O -a ,global,d,, ps_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  ( names ps_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${exp} )
  ( ave_function ps _cf3hr _EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc )
  

  tar -cf cfSites_EC-EARTH_${exp}_${ens_exp}_${year}${month}.tar *_cfSites*_EC-EARTH_*_${year}${month}.nc
  tar -cf cfDay_EC-EARTH_${exp}_${ens_exp}_${year}${month}.tar *_cfDay_EC-EARTH_*_${year}${month}.nc
  tar -cf Amon_EC-EARTH_${exp}_${ens_exp}_${year}${month}.tar *_Amon_EC-EARTH_*_${year}${month}.nc
  tar -cf monthly_EC-EARTH_${exp}_${ens_exp}_${year}${month}.tar *_monthly_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
##  for i in $(seq 0 `expr ${nvariablesGG} - 1 - 5`); do
##   if [[ $(((${i}+1)%5)) == 0 ]]; then
##   tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_$(((${i}+1)/5)).tar ${namesGG[`expr ${i} - 4`]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
##    ${namesGG[`expr ${i} - 3`]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[`expr ${i} - 2`]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
##    ${namesGG[`expr ${i} - 1`]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ${namesGG[i]}_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
##   fi
##  done
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_1.tar sbl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clwvi_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  clivi_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc zg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc hus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  prw_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc pr_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_2.tar prc_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc prsn_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  hfss_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc hfls_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc psl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  ps_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_3.tar clt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc uas_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  vas_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ts_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rsds_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_4.tar rlds_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rsus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  rlus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rlut_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tauu_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_5.tar tauv_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc evspsbl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  rlutcs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
#  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_6.tar rsuscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rldscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  tas_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clw_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cli_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_6.tar rsnscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rldscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  tas_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clw_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cli_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_7.tar cl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  tntmp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tntr_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnta_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_8.tar tnhusmp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnhusa_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  mcu_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc mcd_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc hur_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
#  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_9.tar \
#  cltisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc pctisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  albisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tauisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  cllcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clmcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  clhcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cltcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  rsdscs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc huss_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  hurs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cct_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  ccb_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
#  parasolRefl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_9.tar \
  cltisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc pctisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  albisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tauisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  cllcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clmcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  clhcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cltcalipso_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  huss_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  hurs_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc cct_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  ccb_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc clisccp_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  parasolRefl_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_10.tar ta_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ua_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  va_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_11.tar wap_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc wap500_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  ta700_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tnhus_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
## add in this .tar rsdt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc in case you have it
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_12.tar reffclwc_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  reffclic_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_13.tar ci_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc rsn_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  sst_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc istl1_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc swvl1_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_14.tar swvl2_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc swvl3_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  swvl4_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc smlt_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc 10fg_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_15.tar pv_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc stl1_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  sd_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc bld_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc 2d_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_16.tar stl2_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc lsm_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  sr_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc al_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc stl3_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_17.tar lcc_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc mcc_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  hcc_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc sund_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc gwd_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc
  tar -cf cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}_18.tar src_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc ro_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc \
  stl4_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc tsn_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc fal_cf3hr_EC-EARTH_${exp}_${ens_exp}_${year}${month}.nc

  ecp *.tar ectmp:/nml/amip/${exp}
  rm -f ICM* *.grb 
  rm *.nc *.tar rulesSH rules_file clcalipso
  cd ..
  rm -r ${exp}${year}${month}
#cd /climate/EC-EARTH/AMIP Bht$32qa   jhsy46Gayte% 

