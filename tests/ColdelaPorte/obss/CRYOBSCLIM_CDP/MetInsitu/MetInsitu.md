# MetInsitu
## FORCING_1993080106_2017080106_insitu.nc
```
netcdf FORCING_1993080106_2017080106_insitu {
dimensions:
	time = UNLIMITED ; // (210387 currently)
	Number_of_points = 1 ;
variables:
	double flag(time, Number_of_points) ;
		flag:_FillValue = -9999999. ;
		flag:long_name = "in situ flag" ;
		flag:units = "0 or 1" ;
	double time(time) ;
		time:_FillValue = -9999999. ;
		time:long_name = "time" ;
		time:units = "seconds since 1993-08-01 06:00:00" ;
	double LAT(Number_of_points) ;
		LAT:_FillValue = -9999999. ;
		LAT:long_name = "latitude" ;
		LAT:units = "degrees_north" ;
	double LON(Number_of_points) ;
		LON:_FillValue = -9999999. ;
		LON:long_name = "longitude" ;
		LON:units = "degrees_east" ;
	double ZS(Number_of_points) ;
		ZS:_FillValue = -9999999. ;
		ZS:long_name = "altitude" ;
		ZS:units = "m" ;
	double aspect(Number_of_points) ;
		aspect:_FillValue = -9999999. ;
		aspect:long_name = "slope aspect" ;
		aspect:units = "degrees from north" ;
	double slope(Number_of_points) ;
		slope:_FillValue = -9999999. ;
		slope:long_name = "slope angle" ;
		slope:units = "degrees from horizontal" ;
	double ZREF(Number_of_points) ;
		ZREF:_FillValue = -9999999. ;
		ZREF:long_name = "Reference_Height" ;
		ZREF:units = "m" ;
	double UREF(Number_of_points) ;
		UREF:_FillValue = -9999999. ;
		UREF:long_name = "Reference_Height_for_Wind" ;
		UREF:units = "m" ;
	int station(Number_of_points) ;
		station:_FillValue = -9999999 ;
		station:long_name = "OMM code of the station" ;
		station:units = "" ;
	double FORC_TIME_STEP ;
		FORC_TIME_STEP:_FillValue = -9999999. ;
		FORC_TIME_STEP:long_name = "Forcing_Time_Step" ;
		FORC_TIME_STEP:units = "s" ;
	double FRC_TIME_STP ;
		FRC_TIME_STP:_FillValue = -9999999. ;
		FRC_TIME_STP:long_name = "Forcing_Time_Step" ;
		FRC_TIME_STP:units = "s" ;
	double Tair(time, Number_of_points) ;
		Tair:_FillValue = -9999999. ;
		Tair:long_name = "Near Surface Air Temperature" ;
		Tair:units = "K" ;
	double Qair(time, Number_of_points) ;
		Qair:_FillValue = -9999999. ;
		Qair:long_name = "Near Surface Specific Humidity" ;
		Qair:units = "Kg/Kg" ;
	double Wind_DIR(time, Number_of_points) ;
		Wind_DIR:_FillValue = -9999999. ;
		Wind_DIR:long_name = "Wind Direction" ;
		Wind_DIR:units = "deg" ;
	double Wind(time, Number_of_points) ;
		Wind:_FillValue = -9999999. ;
		Wind:long_name = "Wind Speed" ;
		Wind:units = "m/s" ;
	double Rainf(time, Number_of_points) ;
		Rainf:_FillValue = -9999999. ;
		Rainf:long_name = "Rainfall Rate" ;
		Rainf:units = "kg/m2/s" ;
	double Snowf(time, Number_of_points) ;
		Snowf:_FillValue = -9999999. ;
		Snowf:long_name = "Snowfall Rate" ;
		Snowf:units = "kg/m2/s" ;
	double LWdown(time, Number_of_points) ;
		LWdown:_FillValue = -9999999. ;
		LWdown:long_name = "Surface Incident Longwave Radiation" ;
		LWdown:units = "W/m2" ;
	double DIR_SWdown(time, Number_of_points) ;
		DIR_SWdown:_FillValue = -9999999. ;
		DIR_SWdown:long_name = "Surface Incident Direct Shortwave Radiation" ;
		DIR_SWdown:units = "W/m2" ;
	double SCA_SWdown(time, Number_of_points) ;
		SCA_SWdown:_FillValue = -9999999. ;
		SCA_SWdown:long_name = "Surface Incident Diffuse Shortwave Radiation" ;
		SCA_SWdown:units = "W/m2" ;
	double CO2air(time, Number_of_points) ;
		CO2air:_FillValue = -9999999. ;
		CO2air:long_name = "Near Surface CO2 Concentration" ;
		CO2air:units = "kg/m3" ;
	double PSurf(time, Number_of_points) ;
		PSurf:_FillValue = -9999999. ;
		PSurf:long_name = "Surface Pressure" ;
		PSurf:units = "Pa" ;
	double NEB(time, Number_of_points) ;
		NEB:_FillValue = -9999999. ;
		NEB:long_name = "Nebulosity" ;
		NEB:units = "between 0 and 1" ;
	double HUMREL(time, Number_of_points) ;
		HUMREL:_FillValue = -9999999. ;
		HUMREL:long_name = "Relative Humidity" ;
		HUMREL:units = "%" ;
	double theorSW(time, Number_of_points) ;
		theorSW:_FillValue = -9999999. ;
		theorSW:long_name = "Surface Incident total Shortwave radiation" ;
		theorSW:units = "W/m2" ;
```