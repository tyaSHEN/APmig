The input data in the folder are mock data instead of the real data used in our paper, so the results is different. 

To get the actual results, please refer to the paper to retrieve the real data. However, the stucture of the inputs should be kept to ensure the program runs correctly.

Here is the description of each dataset in this folder

#### Step 1 ####
###
"generation inputs (mock).csv" is for regrssion model in step 1. It consists of covariates values

Column names 
i_iso3: ISO 3166-1 alpha-3 of Origin
year: year
ap: whether this is a Asia Pacific country
eu: whether this is a IMEM country
i_pop: population of this country
small: whether this country has less than 6,000,000 people in 2018
i_gdp2: GDP per capita data, measured in terms of current US dollars
i_depr: old-age dependency ratios 
i_fele: female life expectancy
i_name: name of the origin country
i_urbanpop: percentage urban population
i_pc_mig: percentage foreign-born population

###
"imem (mock).RData" is for regrssion model in step 1. It consists of the emigration counts and rates for IMEM countries. This data can be retrieved in the IMEM website. Inside this RData, there is one data frame named "temp1".

Column names in "temp1"
iteration: iteration index
origin: ISO 3166-1 alpha-2 of Origin    
year: year     
value: number of emigrants 
i_iso3: ISO 3166-1 alpha-3 of Origin      
i_name: name of the origin country       
i_pop: population of this country        
emig_rate: emigration rates   
ln_emig_rate: Natural logarithm of emigration rates  
####

#### Step 2 ####
###
"stock gross (mock). csv" is for distribution in step 2.

Column names 
year: year
i_iso3: ISO 3166-1 alpha-3 of Origin
j_iso3: ISO 3166-1 alpha-3 of Destination
f_migstock: migrant stock in j_iso3 from i_iso3
f_migstock_t: migrant stock in i_iso3 from j_iso3
migstock_gross: sum of `f_migstock` and `f_migstock_t`

###
"stock gross (mock). csv" is for distribution in step 2.

Column names 
year: year
i_iso3: ISO 3166-1 alpha-3 of Origin
j_iso3: ISO 3166-1 alpha-3 of Destination
trade: trade flow from i_iso3 to j_iso3
####

#### Step 3f ####
###
"AP Country list.csv" is the list of countries used in this paper.

Column names 
region: name of the sub-region
country/area name: name of the country or area
code: ISO 3166-1 alpha-3 of this country

###
"pop (mock).csv" is annual population.

Column names 
year: year
i_iso3: ISO 3166-1 alpha-3 of Origin
pop: mid-year population

###
"UN net rate (mock).csv" is the net migration rate from UN

Column names 
i_iso3: ISO 3166-1 alpha-3 of the country
2000-2005: net migration rate in 2000-2005
2005-2010: net migration rate in 2005-2010
2010-2015: net migration rate in 2010-2015
2015-2020: net migration rate in 2015-2020

###
"ANK (mock).csv" is the reported migration flow of Cananda, South Korea, Australia and New Zealand.

Column names 
year: year
i_iso3: ISO 3166-1 alpha-3 of the country
iflow: reported number of immigrants
eflow: reported number of emigrants
####