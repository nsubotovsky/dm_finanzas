#instalo para que se pueda instalar en R el paquete mlrMBO
sudo apt-get update

#hago esto para que no me pida interaccion  tzdata
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y tzdata
sudo timedatectl set-timezone America/New_York

cd
sudo dpkg --add-architecture i386

sudo apt-get --yes install apt-utils
sudo apt-get --yes install libssl-dev  libcurl4-openssl-dev  libcurl4-openssl-dev
sudo apt-get --yes install git-core
sudo apt-get --yes install libgeos-dev  libproj-dev  libgdal-dev  libv8-3.14-dev  librsvg2-dev
sudo apt-get --yes install cmake
sudo apt-get --yes install curl
sudo apt-get --yes install gdebi
sudo apt-get --yes install ocl-icd-opencl-dev
sudo apt-get --yes install cron

sudo apt-get --yes install nano



#instalo Google Cloud Fuse  para poder ver el bucket
cd
curl -L -O https://github.com/GoogleCloudPlatform/gcsfuse/releases/download/v0.23.0/gcsfuse_0.23.0_amd64.deb
sudo gdebi --n gcsfuse_0.23.0_amd64.deb
rm   gcsfuse_0.23.0_amd64.deb
mkdir $HOME/cloud

#agrego al crontab
(crontab -l ; echo "@reboot gcsfuse --implicit-dirs --file-mode 777 --dir-mode 777 --key-file \$HOME/privatekey_inicial.json \$nombredelbucket  $HOME/cloud") | crontab -
(crontab -l ; echo "@reboot /bin/bash \$HOME/autorun_script.sh")| crontab -


#instalo la ultima version de R
sudo apt-get --yes update  &&  sudo apt-get --yes upgrade

sudo apt-get --yes install r-base


#instalo paquetes de R
sudo apt-get --yes install r-cran-rcpp
sudo apt-get --yes install r-cran-dplyr
sudo apt-get --yes install r-cran-matrix
sudo apt-get --yes install r-cran-data.table
sudo apt-get --yes install r-cran-rpart
sudo apt-get --yes install r-cran-rocr
sudo apt-get --yes install r-cran-mass
sudo apt-get --yes install r-cran-ranger
sudo apt-get --yes install r-cran-randomforest
sudo apt-get --yes install r-cran-curl
sudo apt-get --yes install r-cran-openssl
sudo apt-get --yes install r-cran-caret


sudo apt-get --yes install r-cran-roxygen2
sudo apt-get --yes install r-cran-devtools
sudo apt-get --yes install r-cran-tidyr
sudo apt-get --yes install r-cran-shiny



#xgboost instalo la ultima version de desarrollo de XGBoost que no esta disponible en CRAN R (para histogramas)
cd
sudo rm -rf  xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
cd R-package
sudo R CMD INSTALL .
cd
sudo rm -rf  xgboost


#LightGBM instalo ya que no esta disponible en CRAN R
cd
sudo rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM
cd LightGBM
sudo Rscript ./build_r.R
cd
sudo rm -rf  LightGBM

cd
sudo  Rscript --verbose  ./instalo_paquetes.r
sleep  10


#instalo RStudio Server
cd
sudo apt-get --yes install gdebi-core
sudo apt-get --yes install lib32gcc1 lib32stdc++6 libc6-i386 libclang-8-dev libclang-common-8-dev libclang-dev libclang1-8 libgc1c2 libobjc-8-dev libobjc4 psmisc
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.2.1335-amd64.deb
sudo dpkg -i rstudio-server-1.2.1335-amd64.deb
cd
rm  rstudio-server-1.2.1335-amd64.deb
#cambio el puerto del Rstudio Server al 80 para que se pueda acceder en universidades
sudo  chmod a=rw  /etc/rstudio/rserver.conf
sudo echo "www-port=80" >> /etc/rstudio/rserver.conf
sudo rstudio-server restart
sudo rstudio-server start


#reacomodo y limpio el desorden de las instalaciones
sudo apt-get update  &&  sudo apt-get --yes  upgrade
sudo apt-get --yes  autoremove






