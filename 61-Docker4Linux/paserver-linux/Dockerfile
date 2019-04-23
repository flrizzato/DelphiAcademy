FROM ubuntu:18.04

RUN ln -fs /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime

RUN \
 apt-get -y update && \
 apt-get -y install joe wget p7zip-full curl unzip build-essential zlib1g-dev libcurl4-gnutls-dev && \
 apt-get -y install mysecureshell && \
 apt-get -y autoremove && \
 apt-get -y autoclean
 
RUN wget http://altd.embarcadero.com/releases/studio/20.0/PAServer/Release1/LinuxPAServer20.0.tar.gz

RUN \
  mv LinuxPAServer20.0.tar.gz /root/LinuxPAServer20.0.tar.gz && \
  cd /root && \
  tar xzvf LinuxPAServer20.0.tar.gz
  
WORKDIR root/PAServer-20.0

# Define default command.
CMD ["/root/PAServer-20.0/paserver","-password="]

# Expose ports.
EXPOSE 64211
EXPOSE 8080