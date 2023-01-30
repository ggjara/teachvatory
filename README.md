# teachvatory

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of teachvatory is to provide an easy visualization of students’
performance and quizzes’ answers.

## Installation

- Clone respository branch `main`
- Copy `.secrets/` and `.Renviron` into the app’s main directory (you will need to ask the admin for these files)
- Install packages listed in `DESCRIPTION`

## Run Locally

- Run `dev/run_dev.R` to launch the server

## Editing app using GitHub workflow

1) Get GitHub branch name of the issue you want to solve (See options in [Linear](https://linear.app))
2) **Checkout** Standing on the app's directory, write on  terminal:  
`git checkout -b [branch-name]`  
The command `-b` allows you to create and move to the branch in one command.
3) Make all the changes you need to do
4) **Commit your changes:** On the terminal write:  
`git commit -m "[A message to describe your change]"`
    * You can make as many commits as you want. A good approach would be to make a commit for every self-contained update.
5) **Push your changes:**  On the terminal write:  
`git push origin [branch-name]`
6) Go to the [GitHub repository](https://github.com/ggjara/teachvatory) and make a `pull request` from your branch `[branch-name]` to `main`.
7) Admin will review your changes and merge the branch.
8) Once the branch is merged into `main` you can delete the branch remotely and locally.

## Publishing app on shinyapps.io

1) Be sure you are on the latest update of the `main` branch.
    * To update to the lastest update write on terminal:   
    `git pull origin main`

2) Connect your account to your shinyapps.io account. Read more [here](https://shiny.rstudio.com/articles/shinyapps.html).
3) You can publish the app using two methods:
    * Open `app.R` and press the Rsconnect publish button on the top right of RStudio.
    * Standing on the app's directory write on R:  
    `rsconnect::deployApp()`


## Others

- You can access the terminal through any terminal shell or from [RStudio IDE](https://posit.co)
- You can interact with Git through the terminal (as described above) or through [GitHub Desktop](https://desktop.github.com) (which I highly recommend it)




***Last update***: January 30th, 2023 by Gonzalo
