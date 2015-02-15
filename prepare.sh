sudo apt-get -y install jq
wget `curl https://api.github.com/repos/jgm/pandoc/releases/latest | jq '.assets[] | .browser_download_url | select(endswith("deb"))'` -O pandoc.deb
sudo dpkg -i pandoc.deb
