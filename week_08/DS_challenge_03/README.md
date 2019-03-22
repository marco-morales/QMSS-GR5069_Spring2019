# DS CHALLENGE 3 - (the perils of data missingness )

You have three datasets, each one with a different missingness pattern. The purpose of this exercise is for you to (i) get hands-on experience imputing missing data and (ii) get a sense of the perils of ignoring data missingness patterns.

This repo contains the starter code for this week. Among other things, you'll find
* the location of the data and a baseline OLS regression on the `linear_regression.R` file
* an example of how to knit your `Rmd` notebooks using `ezknitr` in the `knit_notebooks.R` file
* a sample `Rmd` notebook and its knitted `md` version 

**Deliverables:**

* **[40%]** Create a new branch `data-imputation-branch` where you:
	* impute each dataset using one method we have discussed in class per dataset. Make sure to **use all three**. Document your process using an R Markdown (`Rmd`) notebook, knit it to a markdown (`md`) file and only upload this last file (along with figures where necessary)
	* make sure to
		* `commit` **logical atomic groups of changes** with informative messages in your local machine
		* `push` to GitHub
		* `merge` the branch back to the `master` branch


* **[40%]** Create a new branch `analysis-comparison-branch` where you:
	* run a simple OLS regression in the `linear_regression.R` script with each one of your non-imputed and imputed data sets (6 in total). Plot the coefficients and standard errors produced by each one of your regressions in a single graph. Document your process using an R Markdown (`Rmd`) notebook, knit it to a markdown (`md`) file and only upload this last file (along with figures where necessary)
	* make sure to
		* `commit` **logical atomic groups of changes** with informative messages in your local machine
		* `push` to GitHub
		* `merge` the branch back to the `master` branch


* **[20%]** Create a new branch `missingness-patterns-branch` where you:
	* create a markdown (`md`) file where you determine the missingness pattern on each dataset, and explain how you reached that conclusion
	* make sure to
		* `commit` **logical atomic groups of changes** with informative messages in your local machine
		* `push` to GitHub
		* `merge` the branch back to the `master` branch


Keep the following structure for the project:

```
project\
	| -- src\
		| -- models\                      <- add scripts here
				| -- linear_regression.R

		| -- notebooks\                   <- add Rmd notebooks here
				| -- sample_notebook.Rmd  <- erase from final repo!
				| -- knit_notebooks.R

		| -- reports\                     <- add md reports here
				| -- sample_notebook.md   <- erase from final repo!
				| -- images\              <- add images here

	| -- README.md
	| -- .gitignore
```


Please make sure to submit it **no later than 6PM on the day of our Week 10 class**.
