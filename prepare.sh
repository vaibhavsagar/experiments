# Install jq to filter Github release data for Pandoc.
sudo apt-get -y install jq
# Get the latest .deb released.
wget `curl https://api.github.com/repos/jgm/pandoc/releases/latest | jq -r '.assets[] | .browser_download_url | select(endswith("deb"))'` -O pandoc.deb
sudo dpkg -i pandoc.deb

# Download an install script for LaTeX
wget http://mirrors.ibiblio.org/CTAN/systems/texlive/tlnet/install-tl-unx.tar.gz
tar xf install-tl-unx.tar.gz
install-tl-*/install-tl -profile texlive.profile
sudo apt-get install xzdec -y
alias tlmgr="/usr/local/texlive/2014/bin/x86_64-linux/tlmgr"
tlmgr init-usertree
tlmgr install booktabs preprint
