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
val get_album_name : album -> string
val print_album_info : album -> unit
