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
retrieved Legendre exoansion coefficients.. 

# Getting Started

## Requirements
This tool requires a linked version of the CRTM library.
The code has been tested with CRTM REL-2.1.3 to 2.3.0.

## Compiling the Code
1. Set the environmental variable `FCC` to your Fortran compiler of choice.
2. Run `make` in the `./build` folder.

## Using the Code

# LICENSE:
Please see the file `LICENSE.txt` for details.

# References
Stegmann, P. G., G. Tang, P. Yang, B. T. Stegmann (2018): "A stochastic model for density-dependent microwave Snow- and Graupel scattering coefficients of the NOAA JCSDA community radiative transfer model.", J. Quant. Spec. Rad. Trans. 211, 9-24.
