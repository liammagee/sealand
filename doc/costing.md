

# Economic Cost for Natural Disasters


$Cost_{Total} = Cost_{D} + Cost_{ID} + Cost_{IT}$


$Cost_{D} = Cost_{Agr} + Cost_{ResHousing} + Cost_{CommercialBuildings} + Cost_{Infrastructure}$

$Cost_{Agr} = Cost_{Fences} + Cost_{Equipment} + Cost_{CropsPastures} + Cost_{Livestock}$

$Cost_{ResHousing} = Cost_{ResStructure} + Cost_{ResContents}$

$Cost_{CommercialBuildings} = Cost_{ComStructure} + Cost_{ComContents}$


$Cost_{DProxy} = InsuredCost * Correction_{InsuredCost}$



$Cost_{ID} = Cost_{PubServices} + Cost_{Agistment} + Cost_{NetworkDisruption} + Cost_{BusinessDisruption} + Cost_{Clean_up} + Cost_{Accom} + Cost_{EmergencyRelief}$


$Cost_{IT} = Cost_{Environmental} + Cost_{DeathInjury} + Cost_{Dislocation} + Cost_{Memorabilia} + Cost_{HealthImpacts} + Cost_{CultureHeritage}$


# Base variables:

## Direct:

* Cost_{Infrastructure}
* Cost_{Fences}
* Cost_{Equipment}
* Cost_{CropsPastures}
* Cost_{Livestock}
* Cost_{ResStructure}
* Cost_{ResContents}
* Cost_{ComStructure}
* Cost_{ComContents}

* InsuredCost - Reasonable Proxy?


## Indirect:

* Cost_{PubServices}
* Cost_{Agistment}
* Cost_{NetworkDisruption}
* Cost_{BusinessDisruption}
* Cost_{Clean_up}
* Cost_{Accom}
* Cost_{EmergencyRelief}


## Intangible:

* Cost_{Environmental} 
* Cost_{DeathInjury} 
* Cost_{Dislocation} 
* Cost_{Memorabilia} 
* Cost_{HealthImpacts} 
* Cost_{CultureHeritage}


# Spreadsheet tracked variables

* Evacuated
* Affected
* Homeless
* Injuries
* Deaths
* Insured Cost
* Reported cost
* 2011 normalised
* Calls to SES

* Train(s) damaged
* Train(s) destroyed
* Home(s) damaged
* Home(s) destroyed
* Building(s) damaged
* Building(s) destroyed
* Bridge(s) damaged
* Bridge(s) destroyed
* Aircraft damaged
* Aircraft destroyed
* Motor Vehicle(s) damaged
* Motor Vehicle(s) destroyed
* Water vessel(s) damaged
* Water vessel(s) destroyed
* Business(es) damaged
* Business(es) destroyed
* Crop(s) destroyed
* Livestock destroyed
* Roads
* Ports

* Environmental

* Male death
* Female death
* Children
* Adults
* Elderly

# Relationships

# Direct

$Cost_{D} = Insured.Cost * Indexation$


## Indirect

$Cost_{EmergencyRelief} = Evacuated * Cost_{Evacuation}$


## Intangibles


$Cost_{Death} = $2,400,000$
$Cost_{HospitalisedInjury} = $214,000$
$Cost_{NonHospitalisedInjury} = $2,100$
$Cost_{DeathInjury} = Deaths * Cost_{Death} + Deaths * Cost_{Injury}$

$Cost_{Dislocation} = Injuries$


# Sources of data:

* Disaster spreadsheet
* Inflation (ABS CPI)
* Population (ABS?)
* Climate Change (?)
* Changing conditions of industry
* Density
* Community Resilience
* [Disaster Assist](http://www.disasterassist.gov.au/DisasterRecoveryExpenditure/Pages/default.aspx)
* [Fed govt budget](http://www.budget.gov.au/2013-14/content/bp3/html/bp3_03_part_2i.htm)



# References


* "Cost of road crashes in Australia 2006", http://www.bitre.gov.au/publications/2010/report_118.aspx 



