(** Representation of Spotify track (song) data.

    This module represents the data stored in track files, including the title,
    artist(s), and album. It handles loading of that data from JSON as well as
    querying the data. *)

exception UnknownSong of string
(** Raised when the Spotify API cannot identify a given song. It carries the
    identifier of the unknown song *)

type track
(** The abstract type of values representing Spotify tracks. *)

val track_of_json : Yojson.Basic.t -> track
(** [track_of_json j] is the Spotify track that [j] represents. Raises
    UnknownSong exception if [j] is an invalid JSON track representation (null). *)

val get_track : unit -> track
(** Reads data/track.json and returns the corresponding track *)

val get_track_name : track -> string
(** [get_track_name t] returns the title of [t]. *)

val get_track_artist : track -> string
(** [get_track_artist t] returns the artist(s) of [t]. If track [t] has two
    artists (a1 and a2) [get_track_artist t] will return the string "a1 and a2."
    If track [t] has 3+ artists (a1, a2, ..., and an) [get_track_artist t] will
    return the string "a1, ..., an-1 and an."*)

val print_track_info : track -> unit
(** [print_track_info t] will print information about track t to the terminal.
    For example, if [t] represents the json of the track "Iris",
    [print_track_info t] will print the following: "Here's what I found: Iris
    was produced by The Goo Goo Dolls in 1998. It is not explicit, lasts 4
    minutes and 49 seconds, and has a popularity ranking of 85. It is track
    number 11 in the album Dizzy up the Girl. Want to listen now? Go to
    https://open.spotify.com/album/4UMjBXcRqIgMZ1XumU2x5T" *)
