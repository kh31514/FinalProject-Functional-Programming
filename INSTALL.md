Install instructions:

After unzipping the source code, change to the source code's directory in command line, and run "make build".

If the terminal prompts an error message, you likely need to install packages. Try running the following commands:
* opam install lwt
* opam install cohttp
* opam install biniou
* opam install atdgen
* opam install uri

Now, you should be able to run "make listen" to begin your musical journey. Further instructions will be provided in the terminal. 

For MS2, we needed to do a lot research to understand how we could intergrate the Spotify API with OCaml. At the moment, users can enter a song, artist, or album and learn corresponding information like a song's duration or an artist's top songs.