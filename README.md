# Description
This is a collection of tools to inspect the CloudCoeff.bin
binary files containing the CRTM cloud scattering LUTs.
Single-scattering properties for the following particle types
in the CRTM can be retrieved in the IR and MW:
- `rain`
- `ice`
- `snow`
- `graupel`
- `hail`

Numerical values are saved in ASCII format and python scripts
are provided to reconstruct the phase function from the 
retrieved Legendre expansion coefficients.. 

# Getting Started

## Requirements
This tool requires a linked version of the CRTM library.
The code has been tested with CRTM REL-2.1.3 to 2.3.0.
A Python installation with NumPy and Matplotlib modules is required for the plotting scripts.

## Compiling the Code
1. Set the environmental variable `FCC` to your Fortran compiler of choice.
2. Run `make` in the `./build` folder.

## Using the Code

### Preliminaries
- If you are using the code, please cite the corresponding paper by Stegmann et al., (2018) in any publication (journal paper, presentation, poster,...) where you are using the code's results.
- If you plan to develop or modify the code, please create a `feature` branch from `develop` or fork the repository.

### Running the Code
To run the program extracting the scattering properties, place the CloudCoeff.bin under scrutiny in the `INSPECT_CloudCoeff/fix` folder as a first step. 
Subsequently execute the ASCII extraction program in the `/bin` folder by running the executable `./InspectCloudCoeff.x` and follow the instructions on the screen.
To reconstruct either the MW or IR phase functions, execute either one of the `plot_legendre_*.py` Python scripts in the `/scripts` folder and enter the number of streams for the Legendre expansion.

# LICENSE:
Please see the file `LICENSE.txt` for details.

# References
Stegmann, P. G., G. Tang, P. Yang, B. T. Stegmann (2018): "A stochastic model for density-dependent microwave Snow- and Graupel scattering coefficients of the NOAA JCSDA community radiative transfer model.", J. Quant. Spec. Rad. Trans. 211, 9-24.
