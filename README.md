# Essenger
Thank you for downloading Essenger: The better Messenger (TM)

Essenger requires some dependencies that you may need to install, listed below.

Please first install pkg-config on your laptop. 
For Windows, run `sudo apt-get install pkg-config`
For MacOS or Ubuntu run `brew install pkg-config` if you have Homebrew or 
`port install pkg-config` if you have MacPorts.

Additionally for Windows, run `sudo apt-get install libssl-dev`

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

If you fun into an ssl or tls error, please use the 3110 virtual
environment, since it provides a clean environment, and download OCaml and 
repeat the steps above. 

Once you have downloaded the required packages, run `make essenger` to run
Essenger. Please ignore the warnings. Have fun chatting!

If you are looking around in server.ml please ignore the red squigglies. 
We think the merlin file doesn't recognize the cohttp-lwt-unix package
yet the program runs perfectly fine.

## First Time Logging In 
Please only enter usernames and passwords that are all lowercase and don't contain spaces.


