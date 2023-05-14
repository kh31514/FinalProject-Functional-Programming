Install instructions:

After unzipping the source code, run "make build".

If the terminal prompts an error message, you likely need to install packages. Try running the following command: opam install ounit2 ANSITerminal pyml yojson

If errors persist, you may also need to install Python and the following Python libraries:
* dotenv (pip install python-dotenv)
* requests (pip install requests)

For Mac: If python is not loading correctly make sure you are using the correct Unix shell. May encounter error using bash instead of zsh.

Now, you should be able to run "make listen" to begin your musical journey. Further instructions will be provided in the terminal. 