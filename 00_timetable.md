### Semester timetable
This is probably more a record than a plan. We'll mostly work on projects and interweave some material as appropriate.

#### Week 1
* **Reading**
  * [Clark et al 2001](https://science.sciencemag.org/content/293/5530/657)
* **Class**
  * Introductions to each other
  * What is forecasting? Why and how might we do it?
  * [Slides week 1](01_slides.pdf)
  * Discussion of project ideas

#### Week 2
* **Reading**
  * [Dietze 2018 Ecological Forecasting, Chapter 2](https://drive.google.com/open?id=1nlbliQzHe-KU8qnpKNOQkDYdOSAM1KTQ)
  * This chapter focuses on uncertainty
  * Read it over two weeks
* **Prep**
  * Talk with each other about projects and form groups
* **Class**
  * Discussion of project ideas
  * [Slides week 2](02_slides.pdf)
  * [List of project ideas](02_project_ideas.md)

#### Week 3
* **Reading**
  * Dietze 2018 Ecological Forecasting, Chapter 2 (see above)
* **Resources**
  * [Git tutorials](tutorials) - new users do setup + basics
  * [Tutorials](tutorials) for data wrangling and visualization (ggplot, dplyr, tidy)
* **Prep**
  * Talk with each other about projects and form groups
    * Review the project ideas that we discussed last week
    * Express interest in projects by visiting the `Issues` tab in the [Data-sandbox repository](https://github.com/EBIO6100Spring2020/Data-sandbox)
  * Send me your GitHub username so I can add you to the class's GitHub organization
    * New to GitHub? [Signup](https://github.com/) with your colorado.edu email.
  * Get hacking to wrangle and visualize data
    * Use the [Data-sandbox repository](https://github.com/EBIO6100Spring2020/Data-sandbox) (see instructions there)
    * [NEON data](https://www.neonscience.org/)
    * [Niwot data](https://nwt.lternet.edu/data-catalog) - and see [Google Drive](https://drive.google.com/drive/folders/1Todaiop6BTS8-CipZFkF9vwBUECzHxKn) for Sarah's list of signature datasets.
* **Class**
  * [Slides week 3](03_slides.pdf)
  * Hackathon - data visualization

#### Week 4
* **Reading**
  * [Harris et al. 2018. Forecasting biodiversity in breeding birds using best practices](https://peerj.com/articles/4278/)
  * This example provides a good overview of issues and methods and provides a walk through of an overall worflow
* **Prep**
  * Continue hacking to wrangle and visualize data
  * Use GitHub `Issues` to organize and coordinate
  * [Git tutorials](tutorials)
    * Review or learn to use branching & teamwork tools
    * See [git05_workflow tutorial](tutorials/git05_workflow.md) for a suggested way for us all to work together on the same repository.
* **Class**
  * [Slides week 4](04_slides.pdf)
  * Hackathon - data visualization

#### Week 5
* **Reading**
  * [Harris et al. 2018. Forecasting biodiversity in breeding birds using best practices](https://peerj.com/articles/4278/)
  * [Summary of the reading](05_Harris_etal_2018_best_practices.md), including best practices
* **Prep**
  * Continue hacking to wrangle and visualize data
  * Prepare a 5 minute report back to be presented to the class
* **Class**
* [Slides week 5](05_slides.pdf)
* Report back on data, models, and plans: class-wide discussion
* Report slides are in the [class Google drive](https://drive.google.com/drive/folders/1xX27yW_LSYoDMCYtr6SOfRI4SizLzyIp)
* The four awesome projects:
  * NEON-Niwot carabid project
  * NEON tick project
  * Niwot codominant plants project
  * Niwot plant SDMs project

#### Week 6
* **Reading**
  * None
* **Prep**
  * Understand how to make a forecast using a simple model such as the temporal average or random walk (aka "naive")
  * Use the reading from the previous two weeks (and consult the code)
* **Class**
  * [Slides week 6](06_slides.pdf)
  * Hackathon - simple models and forecasting pipeline

#### Week 7
* **Reading**
  * [Gelman & Hill (2007) Ch. 12 Multilevel linear models: the basics](https://drive.google.com/open?id=1KCTymYZ1HLE57iilvjUXKLc-safRL6rd)
  * A refresher or introduction to multilevel linear models
* **Resources**
  * Tutorial for G&H Chapter 12
    * [mlvl01](tutorials/mlvl01_multilevel_models_1.md) - simple multilevel model for the average
* **Prep and class**
  * Continue working on a simple model of the average, including:
    * sketching the data structure (e.g. hierarchical)
    * estimating scales of variance
  * Continue working on data and more complex models

#### Week 8
* **Reading**
  * [Hyndman & Athanasopoulos (2018) Forecasting: Principles and Practice](https://otexts.com/fpp2/)
  * This is primarily a time series book. I highlight sections that might be useful in the time series basics tutorial below.
* **Resources**
  * More tutorials for G&H Chapter 12
    * [mlvl02](tutorials/mlvl02_multilevel_models_2.md) - including a predictor
    * [mlvl03](tutorials/mlvl03_multilevel_models_3.md) - predictors at different scales
  * Time series tutorials
    * [tmsr01](tutorials/tmsr01_time_series_basics.md) - time series basics
  * Markdown tutorials
    * [tutorials/repsci02-05](tutorials)
* **Prep and class**
  * Complete a simple model of the average, including:
    * reproducible report output in GitHub markdown format
    * report includes sketch of data structure
    * analysis includes discussion of model diagnostics
  * Move on to more complex models if ready

#### Week 9
* **Resources**
  * Tutorials
    * [mlvl04](tutorials/mlvl04_multilevel_equations.pdf) - equations for multilevel models
    * prediction intervals - coming soon
    * k-fold cross validation - coming soon
<!--    * [fcst01](tutorials/fcst01_prediction_intervals) - prediction intervals
    * [fcst02](tutorials/fcst02_k-fold_CV) - k-fold cross validation
-->
* **Prep and class**
  * Add to simple model report (if you hadn't done this already)
    * model equations
    * forecast with prediction intervals
    * cross validation
  * Move on to more complex models if ready


#### Weeks 10+
* Continue hacking
* Tutorial/theory modules on various forecasting/model topics
