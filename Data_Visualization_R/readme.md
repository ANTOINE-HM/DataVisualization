# To load the data into R

Use the function readRDS (it is part of R's basic functions) : provide the path to the .rds file and it will be loaded into a tibble

There are three files:

- wasabi_all_artists_3000.rds : contains data describing 3000 artists
- albums_all_artists_3000.rds : contains data describing ~7700 albums belonging to the aforementioned artists (use the artist id variable to find the corresponding artist)
- songs_all_artists_3000.rds: contains data describing ~76000 songs from the aforementioned albums (use the album id variable to find the corresponding album)


Then use the methods seen in class to manipulate the data

- Due to the size of the data, avoid using the preview feature of RStudio, as it will take long or will not be able to display the data, particularly the file containing the songs
- Try to inspect the data using functions such as
	- class(df$var) : provides the type of the variable given as parameter
	- colnames(df) : gives the names of all variables in the dataset
	- head(df) : extracts the first 10 rows of the dataset ; alternatively head(df, N) where N is the number of rows you want to extract
	- glimpse(df) : gives an overview of the dataset (variables, types, values)
- After inspection, work with relatively small chunks of the data until you define the right manipulation to apply across the dataset

# To reuse the R script to fetch more or different data

The script fetch_and_transform.R uses a loop to fetch data from wasabi in chunks of 200 data records. It transforms the resulting data into three tables : artists, albums, and songs.

You can modify the script however you see fit, for example:

- If you want to recover more or less data: change the number of times the loop is executed (currently is 15 so that 15 * 200 = 3000)
- If you want to recover data from a different part of the database: change the offset (currently it recovers data between the indices 7200 and 7500)
- If you want to change the type of information it returns : change the url to the GET method (see wasabi API for available urls)
	- Note : this modification might require changing the whole transformation part of the script to adapt it to the new data (variables or format change...)




