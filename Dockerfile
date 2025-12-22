
##################################################################
##                           README                             ##
##################################################################
## Este Dockerfile permite crear un contendor con todos los pa- ##
## quetes y todas las configuraciones necesarias para correr el ##
## control de calidad sobre datos diarios.                      ##
## Al correr el contenedor es necesario definir, como variables ##
## de entorno, los datos de acceso a la Base de Datos.          ##
##################################################################



##########################
## Set GLOBAL arguments ##
##########################

# Set R version
ARG R_VERSION="4.2"

# Set PLOTTER HOME
ARG PLOTTER_HOME="/opt/plotter"

# Set PLOTTER data folder
ARG PLOTTER_DATA="/data"

# Set global CRON args
ARG CRON_TIME_STR="0 12 18 * *"



#################################
## Stage 1: Install R packages ##
#################################

# Create image
FROM rocker/r-ver:${R_VERSION} AS r_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        build-essential \
        # to install ncdf4 (R)
        libnetcdf-dev \
        # to install sf (R)
        libgdal-dev \
        # to install classInt, a dependency of sf (R)
        gfortran \
        # to install units, a dependency of sf (R)
        libudunits2-dev \
        # to install redux (R)
        libhiredis-dev && \
    rm -rf /var/lib/apt/lists/*

# Set CRAN mirror
ARG CRAN_MIRROR="getOption('repos')"

# Install R packages
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
RUN R -e "options(warn=2); install.packages('optparse', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('redux', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"



##############################################
## Stage 2: Copy the R installation folders ##
##############################################

# Create image
FROM rocker/r-ver:${R_VERSION} AS r_final

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # to be able to use ncdf4 (R)
        libnetcdf-dev \
        # to be able to use sf (R)
        libgdal-dev \
        # to be able to use units, a dependency of sf (R)
        libudunits2-dev \
        # to be able to use redux (R)
        libhiredis-dev \
        # to be able to use htmlwidgets::saveWidget with selfcontained = TRUE (R)
        pandoc && \
    rm -rf /var/lib/apt/lists/*

# Install R packages from r_builder
# https://forums.docker.com/t/using-multi-stage-docker-build-for-slimming-down-images-with-r-dependency/67967
RUN mkdir -p /usr/local/lib/R \
             /usr/local/lib/R/site-library
COPY --from=r_builder /usr/local/bin/R /usr/local/bin/R
COPY --from=r_builder /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=r_builder /usr/local/lib/R /usr/local/lib/R
COPY --from=r_builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=r_builder /tmp /tmp

# Set R libs paths (see: https://stat.ethz.ch/R-manual/R-devel/library/base/html/libPaths.html)
ENV R_LIBS="/usr/local/lib/R/library"
ENV R_LIBS_USER="/usr/local/lib/R/site-library"
ENV R_LIBS_SITE="/usr/local/lib/R/site-library"



##########################################
## Stage 3: Install management packages ##
##########################################

# Create image
FROM r_final AS base_image

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop procps \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Create utils directory
RUN mkdir -p /opt/utils

# Create script to load environment variables
RUN printf "#!/bin/bash \n\
export \$(cat /proc/1/environ | tr '\0' '\n' | xargs -0 -I {} echo \"{}\") \n\
\n" > /opt/utils/load-envvars

# Create startup/entrypoint script
RUN printf "#!/bin/bash \n\
set -e \n\
\043 https://docs.docker.com/reference/dockerfile/#entrypoint \n\
exec \"\$@\" \n\
\n" > /opt/utils/entrypoint

# Create script to check the container's health
RUN printf "#!/bin/bash \n\
exit 0 \n\
\n" > /opt/utils/check-healthy

# Set minimal permissions to the utils scripts
RUN chmod --recursive u=rx,g=rx,o=rx /opt/utils

# Allows utils scripts to run as a non-root user
RUN chmod u+s /opt/utils/load-envvars

# Setup cron to allow it to run as a non-root user
RUN chmod u+s $(which cron)

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]



###################################
## Stage 4: Create PLOTTER image ##
###################################

# Create PLOTTER image
FROM base_image AS plotter_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # to save scripts PID
        # to check container health
        redis-tools && \
    rm -rf /var/lib/apt/lists/*

# Renew ARGs
ARG PLOTTER_HOME
ARG PLOTTER_DATA

# Create PLOTTER_HOME folder
RUN mkdir -p ${PLOTTER_HOME}
RUN mkdir -p ${PLOTTER_HOME}/lib
RUN mkdir -p ${PLOTTER_HOME}/logos

# Copy PLOTTER code
COPY lib/ ${PLOTTER_HOME}/lib
COPY logos/ ${PLOTTER_HOME}/logos
COPY *.yaml ${PLOTTER_HOME}
COPY *.R ${PLOTTER_HOME}

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p ${PLOTTER_DATA}/shapefiles
RUN mkdir -p ${PLOTTER_DATA}/pycpt/input/predictands
RUN mkdir -p ${PLOTTER_DATA}/pycpt/input/predictors
RUN mkdir -p ${PLOTTER_DATA}/pycpt/output
RUN mkdir -p ${PLOTTER_DATA}/pycpt/plots/web-crc-sas
RUN mkdir -p ${PLOTTER_DATA}/pycpt/plots/web-sissa
RUN mkdir -p ${PLOTTER_DATA}/ereg/generados/nmme_output
RUN mkdir -p ${PLOTTER_DATA}/ereg/generados/nmme_output/rt_forecasts
RUN mkdir -p ${PLOTTER_DATA}/ereg/generados/nmme_output/comb_forecasts
RUN mkdir -p ${PLOTTER_DATA}/ereg/generados/nmme_figuras/web-crc-sas
RUN mkdir -p ${PLOTTER_DATA}/ereg/generados/nmme_figuras/web-sissa
RUN mkdir -p ${PLOTTER_DATA}/ereg/descargas/NMME

# Save Git commit hash of this build into ${PLOTTER_HOME}/repo_version.
# https://github.com/docker/hub-feedback/issues/600#issuecomment-475941394
# https://docs.docker.com/build/building/context/#keep-git-directory
COPY ./.git /tmp/git
RUN export head=$(cat /tmp/git/HEAD | cut -d' ' -f2) && \
    if echo "${head}" | grep -q "refs/heads"; then \
    export hash=$(cat /tmp/git/${head}); else export hash=${head}; fi && \
    echo "${hash}" > ${PLOTTER_HOME}/repo_version && rm -rf /tmp/git

# Set minimum required permissions for files and folders
RUN find ${PLOTTER_HOME} ${PLOTTER_DATA} -type f -exec chmod -R u=rw,g=rw,o=r -- {} + && \
    find ${PLOTTER_HOME} ${PLOTTER_DATA} -type d -exec chmod -R u=rwx,g=rwx,o=rx -- {} +



#######################################
## Stage 5: Setup PLOTTER core image ##
#######################################

# Create image
FROM plotter_builder AS plotter_core

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew ARGs
ARG PLOTTER_HOME
ARG PLOTTER_DATA
ARG CRON_TIME_STR

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # to configure locale
        locales && \
    rm -rf /var/lib/apt/lists/*

# Configure Locale en_US.UTF-8
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    sed -i -e 's/# es_US.UTF-8 UTF-8/es_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales

# Set locale
ENV LC_ALL es_US.UTF-8

# Create CRON configuration file
RUN printf "\n\
SHELL=/bin/bash \n\
BASH_ENV=/opt/utils/load-envvars \n\
\n\
\043 Setup cron to run files processor \n\
${CRON_TIME_STR} /usr/local/bin/Rscript ${PLOTTER_HOME}/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1\n\
\n" > ${PLOTTER_HOME}/crontab.conf

# Create startup/entrypoint script
RUN printf "#!/bin/bash \n\
set -e \n\
\n\
\043 Reemplazar tiempo ejecución automática del procesador de archivos \n\
sed -i \"/Main.R/ s|^\d\S+\s\S+\s\S+\s\S+\s\S+\s|\$CRON_TIME_STR|g\" ${PLOTTER_HOME}/crontab.conf \n\
crontab -l | sed \"/Main.R/ s|^\d\S+\s\S+\s\S+\s\S+\s\S+\s|\$CRON_TIME_STR|g\" | crontab - \n\
\n\
exec \"\$@\" \n\
\n" > /opt/utils/entrypoint

# Create script to check the container's health
RUN printf "#!/bin/bash\n\
if [ \$(find ${PLOTTER_HOME} -type f -name '*.pid' 2>/dev/null | wc -l) != 0 ] || \n\
   [ \$(echo 'KEYS *' | redis-cli -h \${REDIS_HOST} 2>/dev/null | grep -c fcsts) != 0 ] && \n\
   [ \$(ps -ef | grep -v 'grep' | grep -c 'Main.R') == 0 ] \n\
then \n\
  exit 1 \n\
else \n\
  exit 0 \n\
fi \n\
\n" > /opt/utils/check-healthy

# Set minimal permissions to the new scripts and files
RUN chmod u=rw,g=r,o=r ${PLOTTER_HOME}/crontab.conf

# Set read-only environment variables
ENV PLOTTER_HOME=${PLOTTER_HOME}
ENV PLOTTER_DATA=${PLOTTER_DATA}

# Set user-definable environment variables
ENV CRON_TIME_STR=${CRON_TIME_STR}



######################################
## Stage 6: Setup rPLOT final image ##
######################################

# Create image
FROM rplot_core AS rplot-root

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew ARGs
ARG PLOTTER_HOME

# Setup CRON for root user
RUN (cat ${PLOTTER_HOME}/crontab.conf) | crontab -

# Create standard directories used for specific types of user-specific data, as defined 
# by the XDG Base Directory Specification. For when "docker run --user uid:gid" is used.
# OBS: don't forget to add --env HOME=/home when running the container.
RUN mkdir -p /home/.local/share && \
    mkdir -p /home/.cache && \
    mkdir -p /home/.config
# Set permissions, for when "docker run --user uid:gid" is used
RUN chmod -R a+rwx /home/.local /home/.cache /home/.config

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT [ "/usr/bin/tini", "-g", "--", "/opt/utils/entrypoint" ]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD [ "cron", "-fL", "15" ]
# or docker run your-image /your/program ...

# Configurar verificación de la salud del contenedor
HEALTHCHECK --interval=3s --timeout=3s --retries=3 CMD bash /opt/utils/check-healthy

# Set work directory
WORKDIR ${PLOTTER_HOME}



#####################################################
## Usage: Commands to Build and Run this container ##
#####################################################


# CONSTRUIR IMAGEN (CORE)
# docker build --force-rm \
#   --target rplot-root \
#   --tag ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0 \
#   --build-arg CRON_TIME_STR="0 12 18 * *" \
#   --file Dockerfile .

# LEVANTAR IMAGEN A GHCR
# docker push ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0

# CORRER OPERACIONALMENTE CON CRON
# docker run --name plot-pronos \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/input/predictors \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/input/predictands \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/output \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/plots/web-crc-sas \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output/rt_forecasts \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output/comb_forecasts \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_figuras/web-crc-sas \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.shp \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.shx \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.prj \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.dbf \
#   --mount type=bind,src=<path-to-file>,dst=/data/ereg/descargas/NMME/dry_mask.nc\
#   --detach ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0

# CORRER MANUALMENTE
# docker run --name plot-pronos \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/input/predictors \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/input/predictands \
#   --mount type=bind,src=<path-to-folder>,dst=/data/pycpt/output \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output/rt_forecasts \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_output/comb_forecasts \
#   --mount type=bind,src=<path-to-folder>,dst=/data/ereg/generados/nmme_figuras/web-crc-sas \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.shp \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.shx \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.prj \
#   --mount type=bind,src=<path-to-file>,dst=/data/shapefiles/CRC_SAS.dbf \
#   --mount type=bind,src=<path-to-file>,dst=/data/ereg/descargas/NMME/dry_mask.nc \
#   --mount type=bind,src=<path-to-file>,dst=/opt/plotter/config.yaml \
#   --rm ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0\
# Rscript /opt/plotter/Main.R --year 2023 --month 6

