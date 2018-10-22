using CSV
using DataFrames

function findOutage()
	# Load CSV files
	book3 = CSV.File("C:\\Users\\Terence\\AppData\\Local\\Julia-1.0.1\\bin\\book3.csv"; header=0) |> DataFrame
	book4 = CSV.File("C:\\Users\\Terence\\AppData\\Local\\Julia-1.0.1\\bin\\book4.csv"; header=0) |> DataFrame

	println("================== In book3 but not in book4 ===================")
	# Create Dictionary (a hashmap) so we don't have to use a double loop
	book4Dict = Dict()
	
	# loop through each row in book4
	for row in eachrow(book4)
		# adding each dicitonary entry with its "unique ID" : 1st_Cell_in_Row + "|" + 3rd_Cell_in_Row
		book4Dict[string(row[1]) * "|" * row[3]] = 1
	end

	
	for row in eachrow(book3)
	   # check each entry from book3 against book4Dict
	   if !haskey(book4Dict, string(row[1]) * "|" * row[3])
		   # output things not found in book4Dict
		   println(row[1], " ", row[2], " ", row[3])
	   end
	end
	
	println("================== In book4 but not in book3 ===================")
	book3Dict = Dict()

	for row in eachrow(book3)
		book3Dict[string(row[1]) * "|" * row[3]] = 1
	end

	for row in eachrow(book4)
	   if !haskey(book3Dict, string(row[1]) * "|" * row[3])
		   println(row[1], " ", row[2], " ", row[3])
	   end
	end
end

# run function and time its performance
@time findOutage()