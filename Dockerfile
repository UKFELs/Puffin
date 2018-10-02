# start from base
FROM ubuntu:16.04
MAINTAINER Lawrence Campbell <mightylorenzo@gmail.com>

# install system-wide deps for Puffin
RUN apt-get -yqq update && \
             apt -y install gfortran \
             libhdf5-mpi-dev \
             libhdf5-openmpi-dev \
             libfftw3-dev \
             libfftw3-mpi-dev \
             cmake \
             libopenmpi-dev \
             g++

# NOTE - 'WORKDIR' is essentially just 'cd'

# Create user for running Puffin

#ARG USER=mpi
#ENV USER ${USER}
#RUN adduser -D ${USER} \
#      && echo "${USER}   ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

#RUN adduser ${USER} \
#      && echo "${USER}   ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

#ENV USER_HOME /home/${USER}
#RUN chown -R ${USER}:${USER} ${USER_HOME}

#### CREATE WORKING DIRECTORY FOR USER ####
#ARG WORKDIR=/project
#ENV WORKDIR ${WORKDIR}
#RUN mkdir ${WORKDIR}
#RUN chown -R ${USER}:${USER} ${WORKDIR}

RUN useradd --create-home puffin_user
RUN mkdir /home/puffin_user/tmp/
RUN mkdir /home/puffin_user/tmp/puffin-src
RUN mkdir /home/puffin_user/tmp/puffin-build
RUN mkdir /home/puffin_user/tmp/puffin-test
RUN mkdir /home/puffin_user/built/
RUN mkdir /home/puffin_user/built/puffin
COPY . /home/puffin_user/tmp/puffin-src
RUN chown -R puffin_user /home/puffin_user
# Grant sudo access without password
RUN echo 'puffin_user ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers


# WORKDIR ${WORKDIR}
USER puffin_user





WORKDIR /home/puffin_user/tmp/puffin-src

# Can also use ADD, which accepts URL's, but COPY is currently recommended over ADD

# COPY . .
# COPY inputs/simple/1D/OptCommV165pp65-70/fig1 /tmp/puffin-test

WORKDIR /home/puffin_user/tmp/puffin-build

ENV PATH="/usr/include/hdf5/openmpi:${PATH}"

# Run CMake

RUN cmake -DCMAKE_INSTALL_PREFIX:PATH=/home/puffin_user/built/puffin -DENABLE_PARALLEL:BOOL=TRUE -DHdf5_MODULE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi;/usr/lib/x86_64-linux-gnu/' -DHdf5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' -DHdf5_LIBRARY_NAMES='hdf5_openmpi_fortran;hdf5_openmpi' -DHdf5_LIBRARIES='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.so;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.so' -DHdf5_STLIBS='/usr/lib/x86_64-linux-gnu/libhdf5_openmpi_fortran.a;/usr/lib/x86_64-linux-gnu/libhdf5_openmpi.a' /home/puffin_user/tmp/puffin-src

RUN make && make install

ENV OMP_NUM_THREADS=1







# Run Puffin
#ENV OMP_NUM_THREADS=1
#WORKDIR /tmp/puffin-test

# CMD mpirun -np 2 /opt/puffin/bin/puffin f1main.in



#  RUN apt-get -yqq install python-pip python-dev
#  RUN apt-get -yqq install nodejs npm
#  RUN ln -s /usr/bin/nodejs /usr/bin/node
#  
#  # copy our application code
#  ADD flask-app /opt/flask-app
#  WORKDIR /opt/flask-app
#  
#  # fetch app specific deps
#  RUN npm install
#  RUN npm run build
#  RUN pip install -r requirements.txt
#  
#  # expose port
#  EXPOSE 5000
#  
#  # start app
#  CMD [ "python", "./app.py" ]