# PSD GS app

## Description
This is the Docker environment used in production to host the PSD ldpanelist application. 

It mounts 3 separate volumes (apps, data, logs), and thus you can reuse this to load other Rshiny apps by placing the apps in the apps folder.  Note that the `shiny-app` subfolder within `volumes/logs` has to be manually created.

Ensure that a newer openanalytics/r-base image is used (> R 3.5.0) by running `docker pull openanalytics/r-base`

## Files
- `startup.sh`: the Docker environment calls this to load the application on startup of container
- `Dockerfile`: standard Dockerfile to specify the environment
- `docker-compose.yml`: standard docker-compose.yml file to specify how to run the Docker environment

## How to run
1. Run `docker build -t <image:name> .` in the current folder
2. Edit `docker-compose.yml` to reflect the correct image
3. Run `docker-compose up` to run the app
4. Navigate to `http://localhost` and enter password


