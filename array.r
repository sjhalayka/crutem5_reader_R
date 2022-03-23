
if(file.exists("stat4.postqc.CRUTEM.5.0.1.0-202109.txt"))
{
	f = file("stat4.postqc.CRUTEM.5.0.1.0-202109.txt", "r")

	min_years_per_slope = 20

	num_stations_read = 0

	while(TRUE)
	{
		line = readLines(f, n = 1)
  
		if(length(line) == 0)
			break

		# Station data are fixed width columns
		station_id = substr(line, 1, 6)
		name = substr(line, 22, 41)
		country = substr(line, 43, 53)
		first_year = substr(line, 57, 60)
		last_year = substr(line, 61, 64)

		station_id_int = as.integer(station_id)
		first_year_int = as.integer(first_year)
		last_year_int = as.integer(last_year)

		num_years = 1 + last_year_int - first_year_int

		# Store year data records
		year_station_ids = c()
		year_years = c()
		year_jan = c(); year_feb = c(); year_mar = c();
		year_apr = c(); year_may = c(); year_jun = c();
		year_jul = c(); year_aug = c(); year_sep = c();
		year_oct = c(); year_nov = c(); year_dec = c();

		for(i in 1:num_years)
		{
			# Year data columns are delimited by space(s)
			year_line = readLines(f, n = 1)
			year_tokens = strsplit(year_line, " +")

			year = year_tokens[[1]][1]
			t_anomalies = list(1, 12)

			# For each month in the year
			for(j in 1:12)
			{
				t_anomalies[[j]] = year_tokens[[1]][j + 1]

				if(t_anomalies[[j]] == "-999")
					t_anomalies[[j]] = NA
			}

			# Store all of this station's year data, if it's not all NAs that is
			if(is.na(t_anomalies[[1]]) && is.na(t_anomalies[[2]]) && is.na(t_anomalies[[3]]) &&
			is.na(t_anomalies[[4]]) && is.na(t_anomalies[[5]]) && is.na(t_anomalies[[6]]) &&
			is.na(t_anomalies[[7]]) && is.na(t_anomalies[[8]]) && is.na(t_anomalies[[9]]) &&
			is.na(t_anomalies[[10]]) && is.na(t_anomalies[[11]]) && is.na(t_anomalies[[12]]))
			{
				#print("skipping empty year")
			}
			else
			{
				year_station_ids = c(year_station_ids, station_id_int)
						year_years = c(year_years, year)
						year_jan = c(year_jan, t_anomalies[[1]]); year_feb = c(year_feb, t_anomalies[[2]]); year_mar = c(year_mar, t_anomalies[[3]])
						year_apr = c(year_apr, t_anomalies[[4]]); year_may = c(year_may, t_anomalies[[5]]); year_jun = c(year_jun, t_anomalies[[6]]); 
						year_jul = c(year_jul, t_anomalies[[7]]); year_aug = c(year_aug, t_anomalies[[8]]); year_sep = c(year_sep, t_anomalies[[9]]); 
						year_oct = c(year_oct, t_anomalies[[10]]); year_nov = c(year_nov, t_anomalies[[11]]); year_dec = c(year_dec, t_anomalies[[12]]);
			}
		}

		num_stations_read = num_stations_read + 1

		if(num_stations_read %% 1000 == 0)
			print(paste(as.character(num_stations_read), "stations processed."))

		# Only consider a station if it has enough year data
		if(length(year_station_ids) < min_years_per_slope)
		{
			#print("skipping station -- not enough data")
			next
		}


		#print(as.character(station_id_int))
		#print(name)
		#print(country)
		#print(as.character(first_year_int))
		#print(as.character(last_year_int))

		# For each year
		#for(i in 1:lengths(year_station_ids))
		#{
		#	print(year_station_ids[[i]])
		#}



	}

	print(paste(as.character(num_stations_read), "stations processed altogether."))
	



	close(f)
}