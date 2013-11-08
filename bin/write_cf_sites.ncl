cf_sites_file = "../def/cf_sites_location.txt"
cf_sites = asciiread(cf_sites_file, -1, "string")
cf_sites_no   = stringtointeger(str_get_field(cf_sites, 1, ","))
cf_sites_lon  = stringtofloat(str_get_field(cf_sites, 2, ","))
cf_sites_lat  = stringtofloat(str_get_field(cf_sites, 3, ","))
cf_sites_name = str_get_field(cf_sites, 4, ",")

data_path = "/nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-07-30/"
data_file = "235.128.nc"
f_data = addfile(data_path + "/" + data_file, "r")
skt = f_data->SKT
time = skt&time
time_len = dimsizes(time)

sites_array = new((/119, time_len/), float)
do time_idx = 0, time_len - 1
    do site_idx = 0, 118
print(site_idx)
        lat = cf_sites_lat(site_idx)
        lon = cf_sites_lon(site_idx)
if (site_idx .eq. 15) then
print(lat)
print(lon)
end if
        sites_array(site_idx, time_idx) = skt(time_idx, {lat}, {lon})
    end do
end do
