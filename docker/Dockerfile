FROM rocker/geospatial:4.1.2

# Disable the annoying bell on WSL2
RUN sed -i 's/^# set bell-style none$/set bell-style none/' /etc/inputrc

# Add DOI CA to local CAs so that SSL can work over VPN
COPY DOIRootCA2.crt /usr/local/share/ca-certificates
RUN update-ca-certificates

# Set a default scheduler for clustermq. This can be overridden in the
# _targets.R itself. The multicore should be nice for saving memory as it is
# threaded, but the docs say that it "sometimes causes problems (macOS,
# RStudio) and is not available on Windows. MacOS and Windows won't be problems
# since this is in docker, but RStudio is a possibility. This could potentially
# be set on a per-use-case basis; discuss use cases with team.
#
# Additionally, use the local RStudio Teams CRAN mirror for R packages (this
# requires VPN connection when building).
#
# Additionally, set GLM_PATH variable so GLM3r uses the executable built here
# instead of the one included with the package.
RUN echo '\n\
options(clustermq.scheduler = "multicore")\n\
#options(repos = c(REPO_NAME = "https://rpkg.chs.usgs.gov/prod-cran/latest"))\n\
Sys.setenv(GLM_PATH = "/usr/local/bin/GLM/glm")' \
  >> /usr/local/lib/R/etc/Rprofile.site

# Dependencies: ZeroMQ library for clustermq and libglpk40 for igraph/targets
RUN apt-get update && apt-get install -y \
  apt-utils \
  libgd-dev \
  libnetcdf-dev \
  libglpk40 \
  libzmq3-dev \
  m4  \
  vim-tiny \
  && rm -rf /var/lib/apt/lists/*

# Build GLM executable from source, based on jread's build script.
# Need to fix the other libraries to a commit around the time that the no-gui
# fix was added to GLM, since they will have moved on since then. In each case,
# I've used the most recent commit which was earlier than the no-gui fix in GLM.
WORKDIR /tmp
RUN git clone https://github.com/AquaticEcoDynamics/libplot.git && \
  cd libplot && git reset --hard 1277a5c215d38ae87e54c5134fe39b7a73a12e20 && cd .. && \
  git clone https://github.com/AquaticEcoDynamics/libaed2.git && \
  cd libaed2 && git reset --hard 3e031a7590ef6d284149fdcae3df0b2ef9482f3f && cd .. && \
  git clone https://github.com/AquaticEcoDynamics/libutil.git && \
  cd libutil && git reset --hard 8fc472f236e36a0bafc6ff2db4bf27b4644e3bf8 && cd .. && \
  git clone https://github.com/AquaticEcoDynamics/libaed-water.git && \
  cd libaed-water && git reset --hard d6a94e4e39ee65b871beebd377647d05c79bfc51 && cd .. && \
  git clone https://github.com/AquaticEcoDynamics/GLM.git && \
  cd GLM && \
  # The line below pins GLM at "version" 3.2.0a3 (plus unversioned bug
  # fix commit(s)). See this history:
  # https://github.com/AquaticEcoDynamics/GLM/commits/master
  # The glm.h file there contains a line like 
  # #define GLM_VERSION "3.2.0a3"
  # which defines the version.
  # Version 3.2.0a3 was introduced with 75b4b4063f74c4034116d0abd21a09d8521f76cf
  # i.e. 2020-11-7
  # And the no-gui fix is at cd94d86afb7e35ac6df6e0fc0845322b4be5e2e4, which
  # commit was added on 2021-02-16
  git reset --hard cd94d86afb7e35ac6df6e0fc0845322b4be5e2e4 &&\
  ./build_glm.sh && \
  mkdir /usr/local/bin/GLM && \
  mv glm /usr/local/bin/GLM && \
  cd .. && \
  rm -rf *

RUN Rscript -e 'library(remotes); \
                install_github(c("GLEON/GLMr", \
                                 "GLEON/GLM3r", \
                                 "GLEON/glmtools"));' \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install the necessary pipeline and parallelization packages for R
RUN install2.r --error \
  arrow \
  clustermq \
  fst \
  igraph \
  ncdf4 \
  ncdf4.helpers \
  retry \
  tarchetypes \
  targets \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
