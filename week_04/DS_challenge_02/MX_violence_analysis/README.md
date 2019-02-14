In 2016, the New York Times published an [article](https://www.nytimes.com/2016/05/27/world/americas/mexican-militarys-high-kill-rate-raises-human-rights-fears.html?_r=1) detailing the "lethality" of Mexican armed forces in their fight against organized crime and drug cartels ongoing since 2006. Based on data that had been released by the Mexican Government until 2014, the article concludes that

> Mexico’s armed forces are exceptionally efficient killers — stacking up bodies at extraordinary rates. [...] The Mexican Army kills eight enemies for every one it wounds. [...] For the nation’s elite marine forces, the discrepancy is even more pronounced: The data they provide says they kill roughly 30 combatants for each one they injure.

At the end of January 2017, the [Drug Policy Program (PPD)](http://www.politicadedrogas.org/) at CIDE, a public university in Mexico, released a dataset that allegedly expands on a database that the Mexican government released with data on executions, confrontations, and aggressions related to alleged members of organized crime that happened between 2006-2010.

PPD claims that a lengthier and more detailed database was leaked anonymously to the Program, and that it is allegedly the original database for the government dataset. This new data released by PPD spans 11 additional months to cover from December 2006 to November 2011, roughly 80% of the Administration of then President Felipe Calder&oacute;n. According to PPD, it also includes new variables that were not previously available in the original government dataset.

Upon releasing this new data, PPD's director concludes that  

> 86.1% of dead civilians who presumably participated in confrontations with federal armed forces were killed in events of "perfect lethality" where there were only dead and no wounded. [...] Mexico has the terrible situation of having lethality indices of 2.6. The lethality index of the Federal Police is 2.6 dead for every wounded, the Navy's reaches 17.3 dead for every wounded, and the Army's is 9.1 dead for every wounded.

This repository contains the code to 

## Project Structure

The project structure is as follows:

```
MX_violence_analysis\

| -- src
	| -- data
			| -- Gather_ViolenceData_171220.R
			| -- LibraryInstaller.R
			| -- DefineFunctions.R

| -- data  
	| -- raw
			| -- A-A.xlsx
			| -- A-E.xlsx
			| -- tabla9-A-A.xlsx
			| -- tabla9-A-E-xlsx

	| -- external
			| -- ARCH535.csv
			| -- LookupAuthorityNames.csv

	| -- processed
			| -- AllViolenceData_171220.csv

| -- references  
			| -- README.md

```

A few things to note:

* `src/data/Gather_ViolenceData_171220.R` is the main script to shape the data. It makes use of `LibraryInstaller.R` which installs libraries if you don't already have them, and `DefineFunctions.R` which calls some processing functions to the main script
* `references/README.md` is the data dictionary for `data/processed/AllViolenceData_171220.csv`, which is your ready-to-use output file
* all raw data is stored in `data/raw` and external sources in `data/external`
