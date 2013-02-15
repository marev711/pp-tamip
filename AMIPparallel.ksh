#! /usr/bin/ksh
#
# @ shell = /usr/bin/ksh
# @ notification = error
# @ job_name = interp
# @ output = interp_$(jobid).log
# @ error  = interp_$(jobid).err
# @ cpu_limit = 122000,120000
# @ job_cpu_limit = 125600,123600
# @ network.MPI = csss,not_shared,US,HIGH
# @ class = ns
# @ job_type = serial
# @ node = 1
# @ account_no = nlmonkli
# @ resources = ConsumableCpus(1) ConsumableMemory(3000Mb)
# @ total_tasks = 1
# @ ec_smt = yes
# @ queue

##convert Gaussian and spectral grid grib files to Gaussian lat-lon netcdf files 
##and average daily and monthly
##INPUT format 'grib file'
##OUTPUT format '.nc'

module load cdo  ##this command works only on korn shell
module load nco
set -xv

######################where to make eventual changes##############
prefixGG=ICMGGb0if  
prefixSH=ICMSHb0if
start_date=1984
end_date=1985
ens_exp=r3i1p1
exp=amip        #control
#exp=amip4K      #4k
#exp=amip4xco2   #4co2
#exp=amipFuture  #patterned
#note that if you change the values in variablesGG and variablesSH you will likely have to change the values in the part related to the transfer of the files in the end of the script
#################################################################

for year in $(seq ${start_date} ${end_date}); do
 for month in 07 08 09 10 11 12; do
  echo "PROCESSING: " ${year}${month}
  cd $TEMP
  mkdir ${exp}${year}${month}
  cd ${exp}${year}${month}
  
  ecp ectmp:/ruc/${exp}/${year}/ICMGGb0if+${year}${month}_aa .
  ecp ectmp:/ruc/${exp}/${year}/ICMGGb0if+${year}${month}_ab .
  cat ICMGGb0if+${year}${month}_aa ICMGGb0if+${year}${month}_ab > ICMGGb0if+${year}${month}
  rm ICMGGb0if+${year}${month}_aa ICMGGb0if+${year}${month}_ab
  ecp ectmp:/ruc/${exp}/${year}/ICMSHb0if+${year}${month} .
  ecp ectmp:/nml/amip/TISR/rsdt_Amon_EC-EARTH_amip_r1i1p1_${year}${month}.nc
  mv rsdt_Amon_EC-EARTH_amip_r1i1p1_${year}${month}.nc rsdt_Amon_EC-EARTH_amip_r3i1p1_${year}${month}.nc

cdo splitname ${prefixGG}+${year}${month} GG${year}${month}_
cat > rulesSH <<-EOF
write "SH${year}${month}_[shortName].grb";
EOF
grib_filter rulesSH ${prefixSH}+${year}${month}
wait


  cd $TEMP
  cd parallel
  prova.ksh ${year} ${month} ${exp} ${ens_exp} &


 done
done
wait

exit 0


