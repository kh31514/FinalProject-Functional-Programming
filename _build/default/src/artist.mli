(** Representation of Spotify artist data.

    This module represents the data stored in artist files, including the artist
    name, number of followers, and popularity. It handles loading of that data
    from JSON as well as querying the data. *)

exception UnknownArtist of string
(** Raised when the Spotify API cannot identify a given artist. It carries the
    identifier of the unknown artist *)

type artist
(** The abstract type of values representing Spotify artists. *)

type image
(** The abstract type of values representing Spotify artist images. *)

val artist_of_json : Yojson.Basic.t -> artist
(** [track_of_json j] is the Spotify artist that [j] represents. Raises
    UnknownArtist exception if [j] is an invalid JSON artist representation
    (null). *)

val image_of_json : Yojson.Basic.t -> image
(** [image_of_json j] is the Spotify image that [j] represents. Requires that
    [j] is a valid image json *)

val get_artist : unit -> artist
(** reads data/artist.json and returns the corresponding artist*)

val get_artist_name : artist -> string
(** [get_artist_name t] returns the name of artist t*)

val print_artist_info : artist -> unit
