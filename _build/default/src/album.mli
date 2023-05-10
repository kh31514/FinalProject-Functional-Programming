(** Representation of Spotify artist data.

    This module represents the data stored in artist files, including the artist
    name, number of followers, and popularity. It handles loading of that data
    from JSON as well as querying the data. *)

exception UnknownAlbum of string
(** Raised when the Spotify API cannot identify a given artist. It carries the
    identifier of the unknown artist *)

type album
(** The abstract type of values representing Spotify artists. *)

val album_of_json : Yojson.Basic.t -> album
(** [track_of_json j] is the Spotify artist that [j] represents. Raises
    UnknownArtist exception if [j] is an invalid JSON artist representation
    (null). *)

val get_album : unit -> album
(** reads data/album.json and returns the corresponding album *)

val get_album_name : album -> string
(** [get_album_name t] returns the name of album t *)

val get_album_artists : album -> string
(** [get_album_artist t] returns the artist of album t *)

val print_album_info : album -> unit
(** [print_album_info t] will print information about album t to the terminal.
    For example, if [t] represents the json of the album "Rumors",
    [print_track_info t] will print the following: "Here's what I found: Rumors
    was produced by Fleatwood Mac in 1977. Rumors has a total of 11 tracks, some
    of which include." *)
