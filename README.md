# Essenger
The better messenger. 

Thank you for downloading Essenger: The better Messenger (TM)

Essenger was built using OCaml (version 4.08.1) and requires some dependencies that you may need to install, listed below.
If your system does not have one or more of the following
packages, please use `opam install <package>`. Please note that 
it is important to download the packages in the order they are listed, 
especially the packages listed after str. 

Essenger requires the following OCaml packages: 
  
  `yojson`
  
  `ANSITerminal`
  
  `unix`
  
  `sha`
  
  `str`
  
  `ssl`
  
  `lwt_ssl`
  
  `cohttp`
  
  `cohttp-lwt`
  
  `cohttp-lwt-unix` 
  

If you are running on WSL and run into an ssl or tls error, run the following
two commands: 
  
  `sudo apt-get install pkg-config`
  
  `sudo apt-get install libssl-dev`

If you are on MacOS and run into an ssl or tls error, use the 3110 virtual
environment, since it provides a clean OCaml environment, and download the
previous packages. 

Once you have downloaded the required packages, run `make essenger` to run
Essenger. Please ignore the warnings. Have fun chatting!
