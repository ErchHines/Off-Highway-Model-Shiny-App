# Off-Highway-Model-Shiny-App

### R Files included in this project 

`load.R` 		Loads the data from the data folder
`clean.R` 		Prepares the data for use in app,  creates NonHwyClean.RData
`global.R`		Contains all the functions used in running the model
`server.R`		Shiny server file
`ui.R`			Shiny file that contains the user interface
`styles.css`	Custom css needed for the filters and dowload buttons

### Data files included in this project

##### Files that need to be updated annually

`AgBigStateFuel.csv`	These files come from the USDA
`AgRegionFuel.csv`

`AviationPadd.csv`		EIA Prime Supplier Sales Volume - Aviation Gasoline
`AviationState.csv`

`GasBigStateRetail.csv`	EIA Retail gasoline price data
`GasPaddRetail.csv`

`BeaDpi.csv`			Bureau of Economic Analysis

`FfrTable 2-3.csv` 		GSA Federal Fuel Report
`FfrTable 4-3.csv`

`Mf2.csv`				FHWA Highway Statistics Data
`Mf21.csv`
`Mv7.csv`
`Mv9.csv`
`Vm1.csv`
`Vm2.csv`

`StateRecBoating.csv`	USCG Annual Boating Safety Survey

#### Files that needed to be updated every 5 years

`AgCensus.csv`			USDA Agricultural Census

#### Files that need to be periodically checked for updates

`BoatMpg.csv`			USCG National Recreational Boating Survey

`OffRdByVehicle.csv`	
`RecV.csv`
`StateOffRoadRec.csv`

#### Files that do not need to be updated

`Vius 2002.csv` 		These files come from a US Census survey on vehicle use that was discontinued in 2002
`ScmRatio.csv`

`StatePaddFips.csv`		This is a reference files that contain the PADD, FIPS, and USDA Regions that states belong to


