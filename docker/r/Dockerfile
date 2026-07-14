FROM rocker/geospatial:4.5

LABEL maintainer="Diego Vargas <vargasdie@paho.org>"
LABEL description="R environment for DSS indicators processing"

# Set working directory
WORKDIR /workspace

# Create directories for outputs (including geojson)
RUN mkdir -p /workspace/outputs/{csv,parquet,arrow,geojson}

# Set proper permissions
RUN chmod -R 755 /workspace

# geodata and RColorBrewer are not included in rocker/geospatial
# (terra, sf, tidyverse are already pre-installed in the base image)
RUN Rscript -e "install.packages(c('here', 'geodata', 'RColorBrewer'), repos='https://cran.rstudio.com/')"

# Default command
CMD ["R"]
