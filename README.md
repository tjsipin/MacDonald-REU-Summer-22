# Predicting Disease Rates in the Amazon Basin on Climate, Land-Use, and Socioeconomic Variables [WIP]
### By TJ Sipin and Lyndsey Umsted

This project is the product of an internship funded by the National Science Foundation Research Experience for Undergraduates program. The MacDonald Lab at the University of California, Santa Barbara, in collaboration with the Mordecai Lab at Stanford University leads a study on the effects of climate, land-use change, and socioeconomic variables on vector-borne disease dynamics.

## Background

Leishmaniasis is a neglected parasitic disease most commonly found in the tropics, southern tropics, and southern Europe. Infection is caused by the Leishmania parasite transmitted by the bite of infected female phlebotomine sandflies. The most common type of leishmaniasis is cutaneous where an infection reflects skin lesions, which typically develop within several weeks or months after exposure. Lesions can grow larger and eventually lead to open sores that scab and can take anywhere from months to years to heal. Mucosal leishmaniasis is a metastatic progression of cutaneous where the infection spreads to the mucous membranes of the infected individual. The less common, but deadlier type of leishmaniasis is visceral. Visceral leishmaniasis is a systemic form of the disease which, left untreated, is fatal in most cases. 

The World Health Organization estimates there are between 600,000 and 1 million new cases of cutaneous leishmaniasis each year with the majority of cases reported in Afghanistan, Brazil, Iran, Peru, Saudi Arabia, and Syria. Cases reported in South America are found in highly forested areas of the Amazon where higher levels of humidity and biodiversity make for optimal breeding and transmission. Sandflies are even more successful on the forest edges and where there are high rates of standing water.

### Anthropogenic Impacts on Leishmaniasis

Prior to the late 20th century, deforestation in the Amazon was the result of subsistence farmers who cleared small areas of forest to grow crops for their families and local communities. Towards the later half of the 20th century, deforestation became industrialized for large-scale agriculture and mining activities. In the year 2000, more than three-quarters of forest clearing in the Amazon was for cattle-ranching.

Between 2000 and 2021 the Amazon has seen a 16% decrease in forest cover due to deforestation, and cattle-ranching has remained the leading cause of this decline. Illegal and artisanal gold mining has also gained prominence for causing deforestation in remote areas of the Amazon. These mining operations, while heavily contributing to deforestation, have also been known to increase rates of mercury pollution, criminal activities, human rights abuses, and displacement of indigenous communities.

These land-use activities contributing to deforestation have resulted in large amounts of forest clearances, fragmentation of forest patches, and open areas of standing water which provide optimal habitats for the sandflies transmitting leishmaniasis to local populations, miners, and travelers. 

## Building the Models

### Data Used

Our research and analysis comes from an annual data set containing numbers on 69 variables from all municipalities in the countries of Peru, Colombia, and Brazil between the years 2001 and 2019. The data contains information on case numbers of several tropical diseases including cutaneous, mucosal, and visceral leishmaniasis, malaria, dengue, chikungunya, zika, and yellow fever. Along with case reportings from each municipality, the collection includes measurements on abiotic features such as temperature, precipitation, and standing water, and also includes measurements of land use change including deforestation and urbanization.

**Note:** We encountered a problem involving the methods of measuring light emitted by each municipality. Before 2014, a variable called StableLights was used to measure light, but 2014 onward, a variable called AvgRad (average radiance) was used. To deal with this, the data was split between the early, pre-2014 observations and the later, 2014-present observations and models for each were developed separately.
TJ was responsible for building models using the variables from the early data while Lyndsey built models using the variables from the later data.

### Missing Values
Missing values were dealt with by imputing the predictor values of the training set using random-forest-based imputation.

### Models

Several base models had been run, including but not limited to:
* Generalized linear models
* XGBoost
* Random forest
* Support vector machine
* Naive Bayes

Then, we built stacked models including an ensemble of the above base models, with above satisfactory results.

### Picking the Threshold

We built five stacked models for five different quantiles to categorize leishmaniasis levels into low- and high-risk, ranging from the 30th percentile to the 70th percentile by tens. We decided to stay within this range to avoid large amounts of false positive and false negative missclassifications.  

These five stacked models all used training data from the imputed data set which combated the problem of missing values among the predictor variables. Each model was then tested on on a test set of the same dimensions of the testing set from the imputed data in order to validate our models on the original raw data.

In order of importance, we chose our "best" model based on high Area Under the Receiver Operating Characteristics (AUROC) Curve, sensitivity, and accuracy.

For the early data set, the following AUC, sensitivity and accuracy were recorded for each percentile:

* 30th Percentile:
**
* 40th Percentile:
**
* 50th Percentile:
**
* 60th Percentile:
**
* 70th Percentile:
**

For the later data set, the following AUC, sensitivity and accuracy were recorded for each percentile:

* 30th Percentile:
**
* 40th Percentile:
**
* 50th Percentile:
**
* 60th Percentile:
**
* 70th Percentile:
**
