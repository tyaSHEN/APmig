The input data in the folder are mock data instead of the real data used in our paper, so the results is different. The stucture of the data should be kept to ensure the program runs correctly.

To get the actual results, please refer to the paper to retrieve the real data.

###
Column names in "generation inputs (mock).csv"
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
"imem (mock).RData" can be retrieved in the IMEM website. In this RData, there is one data frame named "temp1".

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
