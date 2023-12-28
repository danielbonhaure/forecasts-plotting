
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

# Set user name and id
ARG USR_NAME="nonroot"
ARG USER_UID="1000"

# Set group name and id
ARG GRP_NAME="nonroot"
ARG USER_GID="1000"

# Set users passwords
ARG ROOT_PWD="root"
ARG USER_PWD=$USR_NAME

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
RUN R -e "options(warn=2); install.packages('optparse', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"



##############################################
## Stage 2: Copy the R installation folders ##
##############################################

# Create image
FROM rocker/r-ver:${R_VERSION} AS r_final

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



###################################
## Stage 3: Create PLOTTER image ##
###################################

# Create PLOTTER image
FROM r_final AS plotter_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew PLOTTER ARGs
ARG PLOTTER_HOME
ARG PLOTTER_DATA

# Create PLOTTER_HOME folder
RUN mkdir -p ${PLOTTER_HOME}

# Copy project
COPY . ${PLOTTER_HOME}

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

# Set permissions of app files
RUN chmod -R ug+rw,o+r ${PLOTTER_HOME}
RUN chmod -R ug+rw,o+r ${PLOTTER_DATA}



###########################################
## Stage 4: Install management packages  ##
###########################################

# Create image
FROM plotter_builder AS plotter_mgmt

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop procps \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Setup cron to allow it run as a non root user
RUN chmod u+s $(which cron)

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]



########################################
## Stage 5: Setup PLOTTER core image  ##
########################################

# Create image
FROM plotter_mgmt AS plotter-core

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew PLOTTER ARGs
ARG PLOTTER_HOME
ARG PLOTTER_DATA

# Renew CRON ARGs
ARG CRON_TIME_STR

# Set read-only environment variables
ENV PLOTTER_HOME=${PLOTTER_HOME}
ENV PLOTTER_DATA=${PLOTTER_DATA}

# Set environment variables
ENV CRON_TIME_STR=${CRON_TIME_STR}

# Crear archivo de configuración de CRON
RUN printf "\n\
# Setup cron to run files processor \n\
${CRON_TIME_STR} /usr/local/bin/Rscript ${PLOTTER_HOME}/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1\n\
\n" > ${PLOTTER_HOME}/crontab.txt
RUN chmod a+rw ${PLOTTER_HOME}/crontab.txt

# Setup CRON for root user
RUN (cat ${PLOTTER_HOME}/crontab.txt) | crontab -

# Crear script de inicio.
RUN printf "#!/bin/bash \n\
set -e \n\
\n\
# Reemplazar tiempo ejecución automática del procesador de archivos \n\
crontab -l | sed \"/Main.R/ s|^\S* \S* \S* \S* \S*|\$CRON_TIME_STR|g\" | crontab - \n\
\n\
# Ejecutar cron \n\
cron -fL 15 \n\
\n" > /startup.sh
RUN chmod a+x /startup.sh

# Create script to check container health
RUN printf "#!/bin/bash\n\
if [ \$(ls /tmp/plotter.pid 2>/dev/null | wc -l) != 0 ] && \n\
   [ \$(ps -ef | grep 'Main.R' | grep -v 'grep' | wc -l) == 0 ] \n\
then \n\
  exit 1 \n\
else \n\
  exit 0 \n\
fi \n\
\n" > /check-healthy.sh
RUN chmod a+x /check-healthy.sh

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD [ "bash", "-c", "/startup.sh" ]
# or docker run your-image /your/program ...

# Verificar si hubo alguna falla en la ejecución del replicador
HEALTHCHECK --interval=3s --timeout=3s --retries=3 CMD bash /check-healthy.sh



###################################
## Stage 6: Create non-root user ##
###################################

# Create image
FROM plotter-core AS plotter_nonroot_builder

# Renew USER args
ARG USR_NAME
ARG USER_UID
ARG GRP_NAME
ARG USER_GID
ARG ROOT_PWD
ARG USER_PWD

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # to run sudo
        sudo && \
    rm -rf /var/lib/apt/lists/*

# Modify root password
RUN echo "root:$ROOT_PWD" | chpasswd

# Create a non-root user, so the container can run as non-root
# OBS: the UID and GID must be the same as the user that own the
# input and the output volumes, so there isn't perms problems!!
# Se recomienda crear usuarios en el contendor de esta manera,
# ver: https://nickjanetakis.com/blog/running-docker-containers-as-a-non-root-user-with-a-custom-uid-and-gid
# Se agregar --no-log-init para prevenir un problema de seguridad,
# ver: https://jtreminio.com/blog/running-docker-containers-as-current-host-user/
RUN groupadd --gid $USER_GID $GRP_NAME
RUN useradd --no-log-init --uid $USER_UID --gid $USER_GID --shell /bin/bash \
    --comment "Non-root User Account" --create-home $USR_NAME

# Modify the password of non-root user
RUN echo "$USR_NAME:$USER_PWD" | chpasswd

# Add non-root user to sudoers and to adm group
# The adm group was added to allow non-root user to see logs
RUN usermod -aG sudo $USR_NAME && \
    usermod -aG adm $USR_NAME

# To allow sudo without password
# RUN echo "$USR_NAME ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USR_NAME && \
#     chmod 0440 /etc/sudoers.d/$USR_NAME



############################################
## Stage 7: Setup and run final APP image ##
############################################

# Create image
FROM plotter_nonroot_builder AS plotter-nonroot

# Become root
USER root

# Renew PLOTTER_HOME
ARG PLOTTER_HOME

# Renew USER ARGs
ARG USR_NAME
ARG USER_UID
ARG USER_GID

# Change files owner
RUN chown -R $USER_UID:$USER_GID $PLOTTER_HOME

# Setup cron to allow it run as a non root user
RUN chmod u+s $(which cron)

# Setup cron
RUN (cat $PLOTTER_HOME/crontab.txt) | crontab -u $USR_NAME -

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD [ "bash", "-c", "/startup.sh" ]
# or docker run your-image /your/program ...

# Verificar si hubo alguna falla en la ejecución del replicador
HEALTHCHECK --interval=3s --timeout=3s --retries=3 CMD bash /check-healthy.sh

# Access non-root user directory
WORKDIR /home/$USR_NAME

# Switch back to non-root user to avoid accidental container runs as root
USER $USR_NAME


# Activar docker build kit
# export DOCKER_BUILDKIT=1

# CONSTRUIR IMAGEN (CORE)
# docker build --force-rm \
#   --target plotter-core \
#   --tag ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0 \
#   --build-arg CRON_TIME_STR="0 12 18 * *" \
#   --file Dockerfile .

# LEVANTAR IMAGEN A GHCR
# docker push ghcr.io/danielbonhaure/forecasts-plotting:plotter-core-v1.0

# CONSTRUIR IMAGEN (NON-ROOT
# docker build --force-rm \
#   --target plotter-nonroot \
#   --tag plotter-nonroot:latest \
#   --build-arg USER_UID=$(stat -c "%u" .) \
#   --build-arg USER_GID=$(stat -c "%g" .) \
#   --build-arg CRON_TIME_STR="0 12 18 * *" \
#   --file Dockerfile .

# CORRER OPERACIONALMENTE CON CRON
# docker run --name plot-pronos \
#   --volume <path-to-folder>:/data/pycpt/input/predictors \
#   --volume <path-to-folder>:/data/pycpt/input/predictands \
#   --volume <path-to-folder>:/data/pycpt/output \
#   --volume <path-to-folder>:/data/pycpt/plots/web-crc-sas \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#   --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc\
#   --detach plotter-nonroot:latest

# CORRER MANUALMENTE
# docker run --name plot-pronos \
#   --volume <path-to-folder>:/data/pycpt/input/predictors \
#   --volume <path-to-folder>:/data/pycpt/input/predictands \
#   --volume <path-to-folder>:/data/pycpt/output \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#   --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc \
#   --volume <path-to-file>:/opt/plotter/config.yaml \
#   --rm plotter-nonroot:latest Rscript /opt/plotter/Main.R --year 2023 --month 6


