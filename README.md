# Smart Beacons
Through the use of Smart Beacons & router scripts, users will be able to interact with DApps without going through their front-end or building the smart contract transactions themselves. 

Smart Beacons is a framework for routing transactions. It is essentially AdaHandles for Smart Contract addresses. Through this framework, swapping tokens on a dex will be as simple as sending funds to @ada-to-min (or whatever token pair you want).

# Set up nix config 
Put the following lines in your nix configuration file (usually located at /etc/nix/nix.conf)
extra-experimental-features = nix-command flakes ca-derivations
extra-trusted-substituters = https://cache.iog.io https://cache.nixos.org/ https://public-plutonomicon.cachix.org https://cache.zw3rk
extra-trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=

# Installation 
After setting up nix config, restart your computer or VM. 
Then run:
    nix develop 

# License
See the [LICENSE](LICENSE) file for license rights and limitations (MIT).