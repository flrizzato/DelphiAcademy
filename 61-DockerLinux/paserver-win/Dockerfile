FROM microsoft/windowsservercore
ADD install-paserver.bat /paserver/install-paserver.bat
ADD http://altd.embarcadero.com/releases/studio/20.0/PAServer/Release1/setup_paserver.exe /paserver 
WORKDIR /paserver
RUN install-paserver.bat
EXPOSE 64211
CMD ["/Program Files (x86)/Embarcadero/PAServer/20.0/paserver.exe"]
