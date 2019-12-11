## Transformations and Weighting to Correct Model Inadequacies


<h3 id='1'>5.1 Intro</h3>

* Chapter4: techniques for 【checking the adequacy】 of the linear regression model
* Regression Model fitting has 3 implicit assumptions 【三个假设】
  * 1. the model errors have mean zero and constant variance an are uncorrelated
  * 2. the model errors have a Normal distribution(to Conduct hypothesis tests and construct CIs), errors are independent
  
<h3 id='2>5.2 Variance Stabilizing Transformations</h3>
* Constant Variance Assumption
  * Often violated when the variance is functionally related to the mean
  * Transformation on the response may eliminate the problem
  * The strength of the transformation depends on the amount of curvature that is induced
  * If not satisfied, the regression coefficiens will have larger standard errors (less precision)
[table5.1]

<h3 id='3'>5.3 Transformations to Linearize the Model</h3>

* Note: Least-squares Estimator has least-squares properties with respect to the transformed data, not original data


<h3 id='4'>5.4 Analytical Methods for Selecting a Transformation</h3>

* #### 5.4.1 Transformations on y: The Box-Cox Method
* #### 5.4.2 Transformations on the Regressor Variables
  * As we say in the windmill data, sometimes a transformation on one or more regressor variables is useful
  * These transformations are often selected empirically [经验选择]
  * The Box-Tidwell method can be used to analytically determine the transformation


<h3 id='5'>5.5 Genralized and Weighted Least Squares</h3>

* Linear Regression Models with nonconstant error variance can also be fitted by the method of weighted least squares
* In this method of estimation the deviation between the observed and expected values of yi is multiplied by a weight wi chosen inversely proportional to the variance of yi.

#### 5.5.1 Generalized Least Squares
* When the model under investigation is
  * y = X 
  
  
$\beta B$ 
