FROM eddelbuettel/docker-ubuntu-r

MAINTAINER Matthew Dillon matthewrdillon@gmail.com

# Remain current
RUN apt-get update -qq
RUN apt-get dist-upgrade -y

# Install dependencies
RUN apt-get install -y gdal-bin libgdal-dev libproj-dev

RUN Rscript -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('maptools', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('plyr', repos='http://cran.rstudio.com/')"

VOLUME ["/project"]

