install.packages(c("usethis", "credentials"))


library(usethis)
library(credentials)
## set your user name and email:


#### 1. Sign up at GitHub.com ################################################

## If you do not have a GitHub account, sign up here:
## https://github.com/join

# ----------------------------------------------------------------------------

### 2. Configure git with Rstudio ############################################

usethis::use_git_config(user.name = "githubusername", user.email = "useremail@mail.com")


# ----------------------------------------------------------------------------

### 3. Configure github with Rstudio ############################################

## create a personal access token for authentication:
usethis::create_github_token()




## set personal access token:
credentials::set_github_pat()


# ----------------------------------------------------------------------------

#### 4. Restart R! ###########################################################

# ----------------------------------------------------------------------------



###no need to do this below step!


#### 5. Verify settings ######################################################

usethis::git_sitrep()

## Your username and email should be stated correctly in the output.
## Also, the report should contain something like:
## 'Personal access token: '<found in env var>''

## If you are still having troubles, read the output carefully.
## It might be that the PAT is still not updated in your `.Renviron` file.
## Call `usethis::edit_r_environ()` to update that file manually.
