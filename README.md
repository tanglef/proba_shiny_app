# Shiny app for probability distributions

A first try at using shiny apps to visualize probability distributions and compute easily probabilities

## Goals when doing this

The main goal of this project was for me to discover how to create a shiny app that is user-friendly and visually all right.
I ended up doing a lot more than the small thing I add in mind (with a code that now would need some organizing...)

This allows the user to 

* interactivly input the parameters for more than 10 types of probability distributions,
* Visualize the new distributions in comparison with the standard ones with interactive plots,
* display the numerical values of the first and second moments,
* display the formulas for the repartition function, the probability distribution and the moments,
* compute some probabilities for these distributions (only for one-or-two sided calculations),
* find an interactive dictionnary of the basic theorems and definitions in probability and statistics (bechelor degree level).

## Disclaimer

This project could absolutly be continued now, and improved (a lot).
In the mean time, the app is deployed at [shinyapp](https://tanguylefort.shinyapps.io/probas/) (with only the free plan so there are time and memory limitations).
If you wish to run it locally there are two options:

- simply clone the project and run the `app.R` file,
- use `rshiny` connection to github as follows.

```R
# install.packages(c("shiny", "plotly", "rsconnect", "shinydashboard"))
library(shiny)
runGitHub( "proba_shiny_app", "tanglef")
``` 

Enjoy and don't hesitate to give me some feedback, one never know when/if I'll go back to it, it might be useful.
