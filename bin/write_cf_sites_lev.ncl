load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "indata.ncl"


cf_sites = asciiread(cf_sites_file, -1, "string")
cf_sites_no   = stringtointeger(str_get_field(cf_sites, 1, ","))
cf_sites_lon  = stringtofloat(str_get_field(cf_sites, 2, ","))
cf_sites_lat  = stringtofloat(str_get_field(cf_sites, 3, ","))
cf_sites_name = str_get_field(cf_sites, 4, ",")

f_data = addfile(data_path + "/" + data_file, "r")
fvar = f_data->$oldvarname$
fvar_lat = fvar&lat
fvar_lon = fvar&lon
time = fvar&time
time_len = dimsizes(time)

sp_fhandle = addfile(sp_file, "r")
sp = sp_fhandle->SP

sites_array = new((/119, 91, time_len/), float)
sp_array = new((/119, time_len/), float)
new_lat = new((/119/), float)
new_lon = new((/119/), float)
do time_idx = 0, time_len - 1
    do site_idx = 0, 118
        latv = cf_sites_lat(site_idx)
        lonv = cf_sites_lon(site_idx)
        sites_array(site_idx, :, time_idx) = fvar(time_idx, :, {latv}, {lonv})
        sp_array(site_idx, time_idx) = sp(time_idx, {latv}, {lonv})
    end do
end do
sites_array!0 = "sites"
sites_array!1 = "lev"
sites_array!2 = "time"
sites_array&time = time

do site_idx = 0, 118
    sites_array&sites(site_idx) = site_idx + 1
    latv = cf_sites_lat(site_idx)
    lonv = cf_sites_lon(site_idx)
    tmp = fvar(0, 0, {latv}, {lonv})
    new_lon(site_idx) = stringtofloat(tmp@lon)
    new_lat(site_idx) = stringtofloat(tmp@lat)
    delete(tmp)
end do
sites_array@lat = new_lat
sites_array@lon = new_lon
sites_array@lev = ispan(1, 91, 1)

sp_array!0 = "sites"
sp_array!1 = "time"
sp_array&time = time

; Update coordinate attributes
new_att = (/"units", "long_name"/)
new_att_var = (/"1", "site index"/)

tmp_sites = sites_array&sites
tmp_sp_sites = sp_array&sites

sites_atts = getvaratts(tmp_sites)
sites_sp_atts = getvaratts(tmp_sp_sites)

delete_VarAtts(tmp_sites, sites_atts)
delete_VarAtts(tmp_sp_sites, sites_sp_atts)

tmp_sites@units = "1"
tmp_sites@long_name = "site index"
tmp_sp_sites@units = "1"
tmp_sp_sites@long_name = "site index"

do idx = 0, dimsizes(sites_atts) - 1
    delete_VarAtts(sites_array&sites, sites_atts(idx))
    delete_VarAtts(sp_array&sites, sites_sp_atts(idx))
end do

delete_VarAtts(sites_array&time, (/"long_name", "axis", "_FillValue"/))
delete_VarAtts(sp_array&time, (/"long_name", "axis", "_FillValue"/))

sites_array&sites = tmp_sites
sp_array&sites = tmp_sp_sites

if (isfilepresent(outfilename)) then
    system("rm -f " + outfilename)
end if

fout = addfile(outfilename, "c")
fout->$oldvarname$ = sites_array

if (isfilepresent("134.sites.nc")) then
    system("rm -f 134.sites.nc")
end if

fspout = addfile("134.sites.nc", "c")
fspout->sp = sp_array