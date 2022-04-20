# Telecom Customer Churn Project with R
### Author: Yuri Jeong, Kalvin Chan, Ahmed Gherfal

The topic for this analysis is **How to retain customers in a business environment**. We want to find out why customers were leaving their network provider, so we can find a solution to tackle this problem. Hence we chose this telco dataset: (https://www.kaggle.com/datasets/blastchar/telco-customer-churn). As for many analyses, we will first conduct a Exploratory Data Analysis (EDA) to take a deeper look into the characteristics of this telco dataset. Furthermore, after initial EDA we will be using *logistic regression* and *confusion matrix* to predict why customers were leaving their data plan provider. The dataset contains 20 columns and 7044 rows, which include churn, gender, phone service, contract, online security, and online backup etc. These are important variables which will aid us in completing our objective in this analysis. 

# Methods
To check relationships between variables, *correlation functions and graphs* are created. After confirming correlation values, a *stepwise selection method* was used to filter important independent variables. Thereafter, three different *logistic regression models* were created, and evaluated with *ROC curve graph* as well as *AUC value*. Models’ accuracy, sensitivity, recall, f1-score were calculated by creating a *confusion matrix*.

# Conclusion
In conclusion, an Exploratory Data Analysis (EDA) was conducted by producing and providing relative graphs using RStudio. Interpretations of each graph were constructed in order to aid readers in understanding the concept and characteristics of the dataset. After initial analysis of the dataset, we have an idea of what we should anticipate when we get to the logistics regression model and confusion matrix later in the report. As we have mentioned before, the confusion matrix is very good at predicting when customers are staying, but it produces many false positives so it has difficulty at predicting when people are leaving. After fitting the model, we have noticed that there are two variables that have strong correlation to the response variable Churn, which are contract and tenure. Contracted customers tend to stay longer with their provider than month to month customers. Solution to this could be improving contracts policy, such as, activation fee and hidden fee. We can also give out free subscription services like Spotify and Netflix for contracted customers. Implementing used phone trade in promotion to make customers stay longer or even extend their contracts. VIP status for loyal customers, this acts as a points earning program so people can spend points earned from their provider in other places, for example, with certain points you can get a discount in a movie theater. To test out our solutions, we can conduct an A/B testing within the provider’s customers network. After conducting the experiment, data will be analyzed and the model can be improved accordingly, and decisions can be made.

# References
BlastChar. (2018, February 23). Telco customer churn. Kaggle. Retrieved February 1, 2022, from https://www.kaggle.com/blastchar/telco-customer-churn 

How to put labels over geom_bar for each bar in R with ggplot2. (2019, July 25). Intellipaat Community. https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2

R Plot Mean in ggplot2 Barplot (Example) | Show Averages in geom_bar. (2021, December 13). Statistics Globe. https://statisticsglobe.com/plot-mean-in-ggplot2-barplot-r

Datatricks, One-Hot Encoding in R: Three simple methods. Retrieved February 7, from https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods 

RDocumentation, Cor: Covariance and Correlation (Matrices), Retrieved February 7, from https://www.rdocumentation.org/packages/DescTools/versions/0.99.44/topics/Cor

Bevans, R. (2021, June 18). An introduction to the akaike information criterion. Scribbr. Retrieved February 9, 2022, from https://www.scribbr.com/statistics/akaike-information-criterion/ 
