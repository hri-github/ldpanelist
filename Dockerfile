# set base image
FROM openanalytics/r-base

MAINTAINER andrew.tan "andrew@data.gov.sg"

# get dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

#### Shiny Setup ####

# basic shiny functionality and required packages
RUN Rscript -e "install.packages(c('shiny', 'dplyr', 'janitor', 'readxl', 'writexl', 'tidyr', 'lubridate', 'reshape2'), repos='https://cloud.r-project.org/')"

#### Dependencies ####

#### Environment Settings ####

# expose port
EXPOSE 80

# copy shell script
RUN mkdir -p /etc/shell
COPY startup.sh /etc/shell/startup.sh
RUN chmod +x /etc/shell/startup.sh

# execute shell script
CMD ["bash", "/etc/shell/startup.sh"]

