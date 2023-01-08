#We will start with instructions from https://github.com/rmcelreath/rethinking
#Just follow them and you will succeed. If you do not want to zigzag between rethinking, rstan and related manuals, just follow this script that includes all necessary steps.

#First rethinking directs you to https://mc-stan.org/users/interfaces/rstan.html
#Which redirects you to its own github https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started 
#And eventualy to rtools CRAN interface - there are several options depending on your system and prefences
#We will aim to install the most recent built of Rtools (which makes some parts the whole process a bit more complicated, but it will minimize the chance that we will need to reinstall some parts soon again)

#Go here: https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html and click the "Rtools42 installer" link. Accept or the defaults.

#restart R.

#First: Install the new version of rstan, but not from CRAN (which is not compatible with Rtools4.2) but from here (just run the code)
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

#If the system asks for other packages that you do not have, install those as well

#Check out if everything is safe and sound
example(stan_model, package = "rstan", run.dontrun = TRUE)

#Then do the following:
library("rstan") # observe startup messages
#As the startup message says, if you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#In addition, you should follow the second startup message that says to execute
rstan_options(auto_write = TRUE)

#You can try the example again, if nothing is damaged
example(stan_model, package = "rstan", run.dontrun = TRUE)

#Second: try to install cmdstanr, if you do not succeed, it is not a big deal
#They recommend running this is a fresh R session or restarting your current session
#RESTARTING IS CRUCIAL, do not skip this step and turn off and on you Rstudio again and continue with the lines below
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::check_cmdstan_toolchain()

#install it (do this just once)
library(cmdstanr)
install_cmdstan()

check_cmdstan_toolchain(fix = TRUE)

#If this does not work, more datails can be found here: https://mc-stan.org/cmdstanr/articles/cmdstanr.html
#But do not worry too much about it anyway, cmdstanr is faster for most applications, but rstan is just fine

#Here starts the installation
#If you have R version 4. or newer, continue below. If you want to attempt to install older version of rethinking (version 3.6) go to the line 51 in the setup_Ubuntu_R3.6.R and continue from there. 

#Third, once rstan and cmdstanr are installed (almost there), then you can install rethinking from within R using:
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")


#check if rethinking works
library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

set_ulam_cmdstan(FALSE)

fit_stan <- ulam( f , data=list(y=c(-1,1)) )

precis(fit_stan)
