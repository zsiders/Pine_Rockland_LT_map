# Creation of a map of South Florida Pine Rockland
## For Lauren Trotta's manuscript -- reproduction prohibited
### Plotting historical and current fragments of Pine Rockland habitat with sampling localities

## Work in progress -- unfinalized

#### The data is found in two folders:
- Locality Data
- Map Layers

Locality Data contains a CSV file of the lat-long locations of parks used to generate subsampled phylogenies

Map Layers contains distributions of South Florida Pine Rocklands: Rocks2 is the historical distribution, Rocklands is the current, and dade_pine_rockland_habitat_final is Miami-Dade Pine Rockland habitat. 

#### The mapping routine is contained in pr_map.R

The routine is basically:
1) Read in the data
2) Supplement the data with Florida county boundaries and major Florida waterbodies
3) Align all layers to the same coordinate system:
	- tranverse mercator
	- NAD83
	- units in feet
4) Project the locality information
5) Plot
 