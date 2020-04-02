
DOIP=167.172.168.139

# Instructions for running locally
# --------------------------------

docker build -t modelrunner ./

CMD="docker run -it\
    --mount type=bind,source=`pwd`/output,target=/modelrunner/output \
    modelrunner"
CMD2="docker run -d \
    --mount type=bind,source=`pwd`/output,target=/modelrunner/output \
    modelrunner"

eval $CMD
eval $CMD test
# Running this command will start the container in the background. In a little bit 
# the fans should start coming on because the CPU is doing things. 
eval $CMD2 rf

# To monitor progress one can check the log file:
cat "$(find output/log -name '*.txt' | sort | tail -n1)"

# Or search the number of output chunks that have been modified since yesterday.
# There are 6 * 15 = 90 total chunks (2020)
find output/rf-chunks -maxdepth 1 -mtime -1 | wc -l


# Instructions for running on a server
# ------------------------------------

# deploy
ssh root@$DOIP 'mkdir -p modelrunner'
ssh root@$DOIP 'mkdir -p modelrunner/output'
rsync -zanv --exclude-from '.dockerignore' ./ root@$DOIP:modelrunner
rsync -zavP --exclude-from '.dockerignore' ./ root@$DOIP:modelrunner

# this will take a while
ssh root@$DOIP 'cd modelrunner; docker build -t modelrunner ./'

RCMD="docker run \
    --mount type=bind,source=/root/modelrunner/output,target=/modelrunner/output \
    modelrunner"
RCMD2="docker run -d \
    --mount type=bind,source=/root/modelrunner/output,target=/modelrunner/output \
    modelrunner"

# hello world and testing
ssh root@$DOIP "eval $RCMD"
ssh root@$DOIP "eval $RCMD test"

# train the RF models and forecast
ssh root@$DOIP "eval $RCMD2 rf"

# monitor log file for progress
ssh root@$DOIP 'cat "$(find modelrunner/output/log -name *.txt | sort | tail -n1)"'

# pull output
# (shoudl be in modelrunner as wd)
rsync -zanv root@$DOIP:modelrunner/output ./
rsync -zavP root@$DOIP:modelrunner/output ./


# Teardown
# ------------



