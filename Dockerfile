
########################
## Install R packages ##
########################

# Create image
FROM rocker/r-ver:4.2 AS r_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # to install ncdf4
        libnetcdf-dev \
        # to install sf
        libgdal-dev \
        # to install classInt, a dependency of sf
        gfortran \
        # to install units, a dependency of sf
        libudunits2-dev && \
    rm -rf /var/lib/apt/lists/*

# set CRAN mirror
ARG CRAN_MIRROR="getOption('repos')"

# install R packages
RUN R -e "options(warn=2); install.packages('ncdf4', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('dplyr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tibble', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('sf', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('stringr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('stringi', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tidyr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('purrr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('yaml', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('glue', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('lubridate', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('htmltools', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leafem', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('rnaturalearth', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('R6', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('forcats', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tidync', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('metR', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('logger', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('htmlwidgets', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('here', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('gstat', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('automap', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet.extras2', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('RCurl', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"



########################
## CREATE FINAL IMAGE ##
########################

# Create final image
FROM rocker/r-ver:4.2 AS r_image

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # to be able to use ncdf4 (R)
        libnetcdf-dev \
        # to be able to use sf (R)
        libgdal-dev \
        # to be able to use units, a dependency of sf
        libudunits2-dev \
        # to be able to use htmlwidgets::saveWidget with selfcontained = TRUE
        pandoc \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop \
        # to run sudo
        sudo \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Setup cron to allow it run as a non root user
RUN sudo chmod u+s $(which cron)

# Install R packages from r_builder
# https://forums.docker.com/t/using-multi-stage-docker-build-for-slimming-down-images-with-r-dependency/67967
# OBS: La imagen oficial de R, r-base, se crea usando la rama testing de debian y R se instala usando apt, lo
# que implica que los datos a copiar sean: 1- los ejecutables /usr/bin/R y /usr/bin/Rscript, y 2- las carpetas
# /usr/lib/R y /usr/local/lib/R/site-library. Sin embargo, cuando se usa la imagen rocker/r-ver, creada usando
# la rama estable de debian, los datos a copiar son los siguientes: 1- los ejecutables /usr/local/bin/R y 
# /usr/local/bin/Rscript, y 2- las carpetas /usr/local/lib/R y /usr/local/lib/R/site-library
RUN mkdir -p /usr/local/lib/R \
             /usr/local/lib/R/site-library
COPY --from=r_builder /usr/local/bin/R /usr/local/bin/R
COPY --from=r_builder /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=r_builder /usr/local/lib/R /usr/local/lib/R
COPY --from=r_builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=r_builder /tmp /tmp

# Create work directory
RUN mkdir -p /opt/plotter

# Copy app code
COPY . /opt/plotter

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p /data/shapefiles
RUN mkdir -p /data/acc-cpt/input/predictands
RUN mkdir -p /data/acc-cpt/input/predictors
RUN mkdir -p /data/acc-cpt/output
RUN mkdir -p /data/acc-cpt/plots/web-crc-sas
RUN mkdir -p /data/ereg/generados/nmme_output
RUN mkdir -p /data/ereg/generados/nmme_output/rt_forecasts
RUN mkdir -p /data/ereg/generados/nmme_output/comb_forecasts
RUN mkdir -p /data/ereg/generados/nmme_figuras/web-crc-sas
RUN mkdir -p /data/ereg/descargas/NMME



#######################
## SETUP FINAL IMAGE ##
#######################

# Import final image
FROM r_image

# Set passwords
ARG ROOT_PWD="nonroot"
ARG NON_ROOT_PWD="nonroot"

# Pasar a root
USER root

# Modify root password
RUN echo "root:$ROOT_PWD" | chpasswd

# Create a non-root user, so the container can run as non-root
# OBS: the UID and GID must be the same as the user that own the
# input and the output volumes, so there isn't perms problems!!
ARG NON_ROOT_USR="nonroot"
ARG NON_ROOT_UID="1000"
ARG NON_ROOT_GID="1000"
RUN groupadd --gid $NON_ROOT_GID $NON_ROOT_USR
RUN useradd --uid $NON_ROOT_UID --gid $NON_ROOT_GID --comment "Non-root User Account" --create-home $NON_ROOT_USR

# Modify the password of the non-root user
RUN echo "$NON_ROOT_USR:$NON_ROOT_PWD" | chpasswd

# Add non-root user to sudoers
RUN adduser $NON_ROOT_USR sudo

# Change owner of source code
RUN chown -R $NON_ROOT_UID:$NON_ROOT_GID /opt/plotter

# Setup cron for run once a month
ARG CRON_TIME_STR="0 12 18 * *"
RUN (echo "${CRON_TIME_STR} /usr/local/bin/Rscript /opt/plotter/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1") | crontab -u $NON_ROOT_USR -

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD ["cron", "-f"]
# or docker run your-image /your/program ...

# Access non-root user directory
WORKDIR /home/$NON_ROOT_USR

# Switch back to non-root user to avoid accidental container runs as root
USER $NON_ROOT_USR


# CONSTRUIR CONTENEDOR
# export DOCKER_BUILDKIT=1
# docker build --file Dockerfile \
#        --build-arg ROOT_PWD=nonroot \
#        --build-arg NON_ROOT_PWD=nonroot \
#        --build-arg NON_ROOT_UID=$(stat -c "%u" .) \
#        --build-arg NON_ROOT_GID=$(stat -c "%g" .) \
#        --build-arg CRON_TIME_STR="0 12 18 * *" \
#        --tag plotter:latest .

# CORRER OPERACIONALMENTE CON CRON
# docker run --name plot-pronos \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#        --volume <path-to-folder>:/data/acc-cpt/output \
#        --volume <path-to-folder>:/data/acc-cpt/plots/web-crc-sas \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#        --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc\
#        --detach plotter:latest

# CORRER MANUALMENTE
# docker run --name plot-pronos \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#        --volume <path-to-folder>:/data/acc-cpt/output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#        --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc \
#        --volume <path-to-file>:/opt/plotter/config.yaml \
#        --rm plotter:latest /usr/local/bin/Rscript /opt/plotter/Main.R

