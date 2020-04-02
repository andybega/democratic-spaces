modelrunner
===========

Run the random forest models to generate forecasts for all 6 spaces and, for each, both up and down movements. The core models are in `R/rf.R`. This folder also is setup as a Docker app to make it easier to deploy and train models in the cloud. 

*Docker is not needed to run the forecast models. You can directly run `R/rf.R`. This is in fact what the Docker app does and equivalent.*

## Usage

With Docker:

The Docker container serves as an app that when run will execute model training and write the final forecasts and any other output to the remote or local output folder. E.g. here are some options for running the app:

```bash
CMD="docker run -it \
    --mount type=bind,source=`pwd`/output,target=/modelrunner/output \
    modelrunner"
    
# prints a hello world message
eval $CMD 

# run test script
eval $CMD test

# train random forest models and forecast
eval $CMD rf
```

The last run I did on a DO droplet took 12 hours to complete. It probably runs faster on a modern laptop (with macOS or linux), however note that the script is set up so that it will use all cores to run in parallel. This will make the laptop unusable for anything else. 

## Output

The final output will end up in the `output` folder. 

- `fcasts-rf.csv`: test and live forecasts
- `log`: log files for the model runs. The final run took 12 hours to run on a DO droplet
- `rf-chunks/`: each CSV file here has the forecasts from a particular end year/indicator combination
- `rf-models/`: saved models
- `rf-model-grid.csv`: the order in which models were run/sent to workers
- `rf-pars.csv`: the final tuned hyperparameter values for each model; num.trees is hard coded to 900 and min.node.size to 1, only mtry is tuned over a limited grid with 13 values using the RF out-of-bag prediction error. See the [**demspaces**](https://github.com/andybega/demspaces) package internals for this. 

The file `fcasts-rf.csv` contains the test and live forecasts. It has the following structure:

- `outcome`: one of the space outcome DVs
- `from_year`: the year of data on which the forecasts are based
- `for_years`: the years covered by the forecasts
- `gwcode`: country code
- `p_up` / `p_same` / `p_down`: probability of a increase or decrease in that DV, or that neither an up or down movement will occur. 

## Setup

To install locally:

- clone or download this GitHub repo
- install Docker if it is not already installed and start it
- in a terminal, navigate to the `closing-spaces/modelrunner` folder
- build the container image with `docker build -t modelrunner ./`

The app is now ready to run. Instructions are in the next section below. 

To run remotely on a Digital Ocean droplet, create a droplet with:

- a Docker image (Choose an image > Marketplace > Docker)
- check Monitoring (not strictly needed but probably wanted)
- SSH keys authentication; this is needed for the deploy/run instructions to run seamlessly

Once the server is up add the IP address to `modelrunner.sh` for the `DOIP` variable.

See `modelrunner.sh` for remaining local and server instructions, e.g. how to use `rsync` to deploy the code and data files to the server and build the container image there. 
