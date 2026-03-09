# nixie

a language made to compile to 65c02 machine code


## features

* [x] functions
* [x] variables
* [ ] structures (like c structs)
* [x] declaring variables as zero page variables
* [ ] header files/file importing
* [ ] preprocessor definitions
* [ ] first class functions
* [x] declaring interrupts

## Building with CMake

To build the project, follow these steps:

1. Create a build directory:
   ```bash
   mkdir build
   cd build
   ```
2. Run CMake to configure the project:
   ```bash
   cmake ..
   ```
3. Build the project:
   ```bash
   cmake --build .
   ```

Make sure you have CMake installed on your system.

### uh guys i dont know why its saying ac1dbitches is commiting i wrote everything 
turns out it was a commit email issue, lesson learned do not put nonsense as commit email

### this needs to get deslopified
due to poor design practices and a lack of prior knowledge there is a shocking amount of true slop and very inconsistent code.  
this is mainly in the code generator as i was unaware of the what in the world i was doing