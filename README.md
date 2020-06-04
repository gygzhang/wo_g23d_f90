# HOW TO USE
	1. First compile the project 
	```c
		> make
		```
	2. Then export the LD_LIBIRARY_PATH
		> export LD_LIBIRARY_PATH=/where/eccodes/locate/lib:$LD_LIBRARY_PATH
	3. Run the program by executing:
		> make run

# HOW TO GET HELP? 
	You can type ./grapes23ddat -h or ./grapes23ddat --help to know how to use 
	the option. 

# ADDITION
	This project is build and run via gnu compiler, intel compiler usage is not 
	been test yet(because eccodes library has an serious conflict with intel compiler)
