## Estimating International Migration Flows for the Asia-Pacific Region: Application of a Generation-Distribution Model
***James Raymer,  Qing Guan,  Tianyu Shen,  Arkadiusz Wiśniowski, Juliet Pietsch***

This is a repository for our paper in [*Migration Studies*](https://academic.oup.com/migration). The paper estimates annual flows of international migration amongst 53 populations in the Asia-Pacific region and four macro world regions from 2000 to 2019 using a generation-distribution framework. 

The final estimated flows in the paper are in "res_od.csv". Column names are as follows:
- year: year of flow
- o_name: country or region of origin
- d_name: country or region of destination
- q50: the point estimates of origin-destination-specific flows
- q10: lower bound of 80% prediction interval for the point estimates
- q90: upper bound of 80% prediction interval for the point estimates




The code to produce the estimates are in "Code.r". 

Input data are in "inputs" folder. 
  Note that the input data are not actual data used in the paper. We provided a set of test data files to run the code. 
  To replicate the outputs used in the paper, you need to download the publicly available data from sources we specified in Appendix Table 1 of the paper.



For questions with the code, please email tianyu.shen@anu.edu.au
