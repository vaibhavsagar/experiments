# Install jq to filter Github release data for Pandoc.
sudo apt-get -y install jq xzdec texlive-full
# Get the latest .deb released.
wget `curl https://api.github.com/repos/jgm/pandoc/releases/latest | jq -r '.assets[] | .browser_download_url | select(endswith("deb"))'` -O pandoc.deb
sudo dpkg -i pandoc.deb

# Download an install script for LaTeX
# wget http://mirrors.ibiblio.org/CTAN/systems/texlive/tlnet/install-tl-unx.tar.gz
# tar xf install-tl-unx.tar.gz
# sudo install-tl-*/install-tl -profile texlive.profile
# export PATH=/usr/local/texlive/2014/bin/x86_64-linux:$PATH
# sudo env PATH="$PATH" tlmgr init-usertree
which tlmgr
sudo tlmgr init-usertree
# sudo env PATH="$PATH" tlmgr install booktabs preprint lmodern
sudo tlmgr install booktabs preprint
