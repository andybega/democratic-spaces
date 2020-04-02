FROM rocker/r-ver:3.6.1

WORKDIR modelrunner

# Install R stuff -- heavy-weights here so we can cache this
RUN install2.r dplyr tidyr ggplot2 remotes readr

# Install remaining R stuff, mainly demspaces dependencies so that install
# goes quicker
RUN install2.r glmnet ranger lubridate recipes yardstick future doFuture \
    jsonlite here future.apply lgr

# Create a .here file so R here() works correctly
RUN ["touch", "../.here"]

# Install R packages from GitHub
RUN ls -l
RUN R -e 'remotes::install_github("andybega/demspaces")'

# Source files change frequently, so copy them in last to avoid rebuilding
# earlier layers that can be cached
COPY ./ /modelrunner
RUN mkdir output

ENTRYPOINT ["Rscript", "controller.R"]
CMD ["help"]
