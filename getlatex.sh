# Download an install script for LaTeX
wget http://mirrors.ibiblio.org/CTAN/systems/texlive/tlnet/install-tl-unx.tar.gz
tar xf install-tl-unx.tar.gz
# Install LaTeX
sudo install-tl-*/install-tl -profile texlive.profile
export PATH=/usr/local/texlive/2014/bin/x86_64-linux:$PATH
sudo apt-get -y install xzdec
sudo env PATH="$PATH" tlmgr init-usertree
sudo env PATH="$PATH" tlmgr install booktabs preprint lm ec titling
