# Master's thesis: Unravelling non-household contact patterns during the COVID-19 pandemic in Belgium

This master's thesis will analyse the non-household contacts from wave 12-43 in the Belgian CoMix data.

* **Data management:** data cleaning and handling of missing values.
* **Exploratory analysis:** summarizing the data.
* **Model adults GPO:** code for the model building to model the number of non-household contacts for the adults age category via a Generalised Poisson GAMLSS model.
* **Model adults NBI:** code for the model building to model the number of non-household contacts for the adults age category via a Negative Binomial GAMLSS model.
* **Model children GPO:** code for the model building to model the number of non-household contacts for the children age category via a Generalised Poisson GAMLSS model.
* **Model children NBI:** code for the model building to model the number of non-household contacts for the children age category via a Negative Binomial GAMLSS model.
* **Model elderly GPO:** code for the model building to model the number of non-household contacts for the elderly age category via a Generalised Poisson GAMLSS model.
* **Model elderly NBI:** code for the model building to model the number of non-household contacts for the elderly age category via a Negative Binomial GAMLSS model.
* **Hurdle 1 adults:** code for the hurdle 1 model for the probability of having at least 1 non-household contact (via GLMM logistic regression) for adults.
* **Hurdle 1 children:** code for the hurdle 1 model for the probability of having at least 1 non-household contact (via GLMM logistic regression) for children.
* **Hurdle 1 elderly:** code for the hurdle 1 model for the probability of having at least 1 non-household contact (via GLMM logistic regression) for elderly.
* **Hurdle 2 adults:** code for the hurdle 2 model for the number of (non-zero) non-household contacts (Negative Binomial GLMM) for adults.
* **Hurdle 2 children:** code for the hurdle 2 model for the number of (non-zero) non-household contacts (Negative Binomial GLMM) for children.
* **Hurdle 2 elderly:** code for the hurdle 2 model for the number of (non-zero) non-household contacts (Negative Binomial GLMM) for elderly.
* **Clustering:** code for the clustering task of this thesis:
  * Make contact profiles based on locations of contacts per age/income/occupation category.
  * Clustering of participants based on demographic characteristics.
