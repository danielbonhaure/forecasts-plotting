
########################
## Install R packages ##
########################

# Create image
FROM r-base:4.1.2 AS r_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# install OS packages
RUN apt-get -y -qq update && \
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

# install R packages
RUN R -e "options(warn=2); install.packages('ncdf4', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('dplyr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tibble', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('sf', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('stringr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('stringi', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tidyr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('purrr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('yaml', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('glue', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('lubridate', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('htmltools', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leafem', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('rnaturalearth', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('R6', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('forcats', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tidync', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('metR', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('logger', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('htmlwidgets', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('here', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('gstat', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('automap', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet.extras2', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"



########################
## CREATE FINAL IMAGE ##
########################

# Create final image
FROM r-base:4.1.2 AS r_image

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get -y -qq update &&\
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
RUN mkdir -p /usr/lib/R \
             /usr/local/lib/R/site-library
COPY --from=r_builder /usr/bin/R /usr/bin/R
COPY --from=r_builder /usr/bin/Rscript /usr/bin/Rscript
COPY --from=r_builder /usr/lib/R /usr/lib/R
COPY --from=r_builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=r_builder /tmp /tmp

# Create work directory
RUN mkdir -p /opt/plotter

# Copy app code
COPY . /opt/plotter

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p /data/shapefiles
RUN mkdir -p /data/images
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

# La imagen de r tiene un usuario docker con el uid 1000 y un grupo docker
# con el gid 1000. Generalmente esos uid y gid con los de los usuarios
# que serán dueños de los datos a ser leídos. Por lo tanto, es necesario
# modificar el uid del usuario docker y el gid del grupo docker.
RUN usermod -u 777 docker
RUN find / -ignore_readdir_race -user 1000 -exec chown -h docker {} \;
RUN groupmod -g 777 docker
RUN find / -ignore_readdir_race -group 1000 -exec chgrp -h docker {} \;

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
RUN (echo "0 12 18 * * /usr/bin/Rscript /opt/plotter/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1") | crontab -u $NON_ROOT_USR -

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
#        --tag plotter:latest .

# CORRER OPERACIONALMENTE CON CRON
# docker run --name verify-pronos \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#        --volume <path-to-file>:/data/images/logo-crcsas.png \
#        --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc\
#        --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#        --volume <path-to-folder>:/data/acc-cpt/output \
#        --volume <path-to-folder>:/data/acc-cpt/plots/web-crc-sas \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#        --detach plotter:latest

# CORRER MANUALMENTE
# docker run --name plot-pronos --rm \
#        --volume <path-to-file>:/opt/plotter/config.yaml \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#        --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#        --volume <path-to-file>:/data/images/logo-crcsas.png \
#        --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc\
#        --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#        --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#        --volume <path-to-folder>:/data/acc-cpt/output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#        --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#        --detach plotter:latest /usr/bin/Rscript Main.R

