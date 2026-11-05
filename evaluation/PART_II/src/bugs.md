# Bugs encountered during Dast homework and their solution
## Type errors
### Lengths differs among variables when defining a linear model
The error message: 
```
Error in model.frame.default(formula = biomass ~ dat$num_trees + dat$mean_dbh,  : 
  variable lengths differ (found for 'dat$num_trees')
```
I added a rename of given variable like varName <- dat$varName and it stopped.