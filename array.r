
if(file.exists("stat4.postqc.CRUTEM.5.0.1.0-202109.txt"))
{
	f = file("stat4.postqc.CRUTEM.5.0.1.0-202109.txt", "r")

	min_samples_per_station = 12*20 # 12 months * 20 years
	min_year = 1900 # Try 1980 for short term warming
	max_year = 2022

	trends = c()

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

		# Get ready to store year data records
		year_years = c()
		year_jan = c(); year_feb = c(); year_mar = c();
		year_apr = c(); year_may = c(); year_jun = c();
		year_jul = c(); year_aug = c(); year_sep = c();
		year_oct = c(); year_nov = c(); year_dec = c();

		# For each year
		for(i in 1:num_years)
		{
			# Year data columns are delimited by space(s)
			year_line = readLines(f, n = 1)
			year_tokens = strsplit(year_line, " +")

			year = as.integer(year_tokens[[1]][1])

			t_anomalies = list(1, 12)

			# For each month in the year, get temp anomaly
			for(j in 1:12)
			{
				token = year_tokens[[1]][j + 1]

				if(token == "-999") # -999 indicates a missing record
					token = NA

				t_anomalies[[j]] = as.integer(token)
			}

			# Store the year's data
			# We will worry about NA data later
			year_years = c(year_years, year)
			year_jan = c(year_jan, t_anomalies[[1]]); year_feb = c(year_feb, t_anomalies[[2]]); year_mar = c(year_mar, t_anomalies[[3]]);
			year_apr = c(year_apr, t_anomalies[[4]]); year_may = c(year_may, t_anomalies[[5]]); year_jun = c(year_jun, t_anomalies[[6]]); 
			year_jul = c(year_jul, t_anomalies[[7]]); year_aug = c(year_aug, t_anomalies[[8]]); year_sep = c(year_sep, t_anomalies[[9]]); 
			year_oct = c(year_oct, t_anomalies[[10]]); year_nov = c(year_nov, t_anomalies[[11]]); year_dec = c(year_dec, t_anomalies[[12]]);
		}
		
		# Update the status
		num_stations_read = num_stations_read + 1

		if(num_stations_read %% 1000 == 0)
			print(paste(as.character(num_stations_read), "stations processed."))

		
		#print(station_id)

		# Gather up xy point data for all years for this station,
		# where x is the year and y is the temp anomaly
		x = c()
		y = c()

		for(i in 1:length(year_years))
		{
			#print(year_years[[i]])

			# Skip this year if not in range
			if(year_years[[i]] < min_year || year_years[[i]] > max_year)
				next

			x = c(x, year_years[[i]]); y = c(y, year_jan[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_feb[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_mar[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_apr[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_may[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_jun[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_jul[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_aug[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_sep[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_oct[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_nov[[i]]);
			x = c(x, year_years[[i]]); y = c(y, year_dec[[i]]);
		}
	
		# No years were in the range, go to the next station
		if(length(x) == 0)
			next


		# Get valid xy point count
		valid_xy_count = 0
	
		# For each xy point
		for(i in 1:length(x))
		{
			# Go to next xy point if this one 
			# contains invalid data
			if(is.na(x[[i]]) || is.na(y[[i]]))
				next

			valid_xy_count = valid_xy_count + 1
		}


		# Go to next station if this one hasn't enough valid xy data
		if(valid_xy_count < min_samples_per_station)
		{
			#print(paste("Not enough data for station ", name, country))
			next
		}
		else
		{
			# Save this station's trend
			trends = c(trends, coefficients(lm(y~x))[[2]])
		}
	}


	# Done
	print(paste(as.character(num_stations_read), "stations processed altogether."))
	print(paste(as.character(length(trends)), "stations used."))

	print(paste("Mean: ", mean(trends)))
	print(paste("+/-: ", sd(trends)))

	close(f)
}