
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
ARG R_VERSION=4.2

# Set APP installation folder
ARG APP_HOME=/opt/plotter

# App data folder
ARG APP_DATA=/data

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
## Stage 3: Create non-root user ##
###################################

# Create image
FROM r_final AS non_root

# Load global USER args
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
RUN adduser $USR_NAME sudo && \
    adduser $USR_NAME adm

# To allow sudo without password
# RUN echo "$USR_NAME ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USR_NAME && \
#     chmod 0440 /etc/sudoers.d/$USR_NAME



###########################################
## Stage 4: Install management packages  ##
###########################################

# Create image
FROM non_root AS base_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Setup cron to allow it run as a non root user
RUN chmod u+s $(which cron)

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]



####################################
## Stage 5: Install and setup APP ##
####################################

# Create image
FROM base_builder AS app_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew ARGs
ARG APP_HOME
ARG APP_DATA
ARG USR_NAME
ARG GRP_NAME

# Renew CRON ARGs
ARG CRON_TIME_STR

# Create APP_HOME folder and change its owner
RUN mkdir -p $APP_HOME && chown -R $USR_NAME:$GRP_NAME $APP_HOME

# Copy project
COPY --chown=$USR_NAME:$GRP_NAME . $APP_HOME

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p ${APP_DATA}/shapefiles
RUN mkdir -p ${APP_DATA}/acc-cpt/input/predictands
RUN mkdir -p ${APP_DATA}/acc-cpt/input/predictors
RUN mkdir -p ${APP_DATA}/acc-cpt/output
RUN mkdir -p ${APP_DATA}/acc-cpt/plots/web-crc-sas
RUN mkdir -p ${APP_DATA}/acc-cpt/plots/web-sissa
RUN mkdir -p ${APP_DATA}/ereg/generados/nmme_output
RUN mkdir -p ${APP_DATA}/ereg/generados/nmme_output/rt_forecasts
RUN mkdir -p ${APP_DATA}/ereg/generados/nmme_output/comb_forecasts
RUN mkdir -p ${APP_DATA}/ereg/generados/nmme_figuras/web-crc-sas
RUN mkdir -p ${APP_DATA}/ereg/generados/nmme_figuras/web-sissa
RUN mkdir -p ${APP_DATA}/ereg/descargas/NMME

# Crear archivo de configuración de CRON
RUN printf "\n\
# Setup cron to run files processor \n\
${CRON_TIME_STR} /usr/local/bin/Rscript ${APP_HOME}/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1\n\
\n" > /tmp/crontab.txt
RUN (echo "${CRON_TIME_STR} /usr/local/bin/Rscript ${APP_HOME}/Main.R >> /proc/1/fd/1 2>> /proc/1/fd/1") 

# Crear script para verificar salud del contendor
RUN printf "#!/bin/bash\n\
if [ \$(ls /tmp/plotter.pid 2>/dev/null | wc -l) != 0 ] && \n\
   [ \$(ps | grep Main.R | wc -l) == 0 ] \n\
then \n\
  exit 1 \n\
else \n\
  exit 0 \n\
fi \n\
\n" > /check-healthy.sh
RUN chmod a+x /check-healthy.sh

# Definir variables de entorno para el contendor final
ENV CRON_TIME_STR=${CRON_TIME_STR}

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



############################################
## Stage 6: Setup and run final APP image ##
############################################

# Create image
FROM app_builder AS final_app_image

# Become root
USER root

# Renew the ARG
ARG USR_NAME

# Setup cron to allow it run as a non root user
RUN chmod u+s $(which cron)

# Setup cron
RUN (cat /tmp/crontab.txt) | crontab -u $USR_NAME -

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



####################################
## Stage 7: Set the DEFAULT image ##
####################################

FROM final_app_image



# CONSTRUIR CONTENEDOR
# export DOCKER_BUILDKIT=1
# docker build --force-rm \
#   --target final_app_image \
#   --tag plotter:latest .
#   --build-arg USER_UID=$(stat -c "%u" .) \
#   --build-arg USER_GID=$(stat -c "%g" .) \
#   --build-arg CRON_TIME_STR="0 12 18 * *" \
#   --file Dockerfile .

# CORRER OPERACIONALMENTE CON CRON
# docker run --name plot-pronos \
#   --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#   --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#   --volume <path-to-folder>:/data/acc-cpt/output \
#   --volume <path-to-folder>:/data/acc-cpt/plots/web-crc-sas \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/rt_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_output/comb_forecasts \
#   --volume <path-to-folder>:/data/ereg/generados/nmme_figuras/web-crc-sas \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shp \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.shx \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.prj \
#   --volume <path-to-file>:/data/shapefiles/CRC_SAS.dbf \
#   --volume <path-to-file>:/data/ereg/descargas/NMME/dry_mask.nc\
#   --detach plotter:latest

# CORRER MANUALMENTE
# docker run --name plot-pronos \
#   --volume <path-to-folder>:/data/acc-cpt/input/predictors \
#   --volume <path-to-folder>:/data/acc-cpt/input/predictands \
#   --volume <path-to-folder>:/data/acc-cpt/output \
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
#   --rm plotter:latest /usr/local/bin/Rscript /opt/plotter/Main.R

