load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "indata.ncl"

cf_sites = asciiread(cf_sites_file, -1, "string")
cf_sites_no   = stringtointeger(str_get_field(cf_sites, 1, ","))
cf_sites_lon  = stringtofloat(str_get_field(cf_sites, 2, ","))
cf_sites_lat  = stringtofloat(str_get_field(cf_sites, 3, ","))
cf_sites_name = str_get_field(cf_sites, 4, ",")

f_data = addfile(data_path + "/" + data_file, "r")
skt = f_data->SKT
time = skt&time
time_len = dimsizes(time)

sites_array = new((/119, time_len/), float)
do time_idx = 0, time_len - 1
    do site_idx = 0, 118
        lat = cf_sites_lat(site_idx)
        lon = cf_sites_lon(site_idx)
        sites_array(site_idx, time_idx) = skt(time_idx, {lat}, {lon})
    end do
end do
sites_array!0 = "sites"
sites_array!1 = "time"
sites_array&time = time

; Update coordinate attributes
tmp_sites = sites_array&sites
sites_atts = getvaratts(tmp_sites)
delete_VarAtts(tmp_sites, sites_atts)
tmp_sites@units = "1"
tmp_sites@long_name = "site index"
do idx = 0, dimsizes(sites_atts) - 1
    delete_VarAtts(sites_array&sites, sites_atts(idx))
end do
sites_array&sites = tmp_sites

if (isfilepresent(outfilename)) then
    system("rm -f " + outfilename)
end if

fout = addfile(outfilename, "c")
fout->tas = sites_array
