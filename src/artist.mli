(** Representation of Spotify artist data.

    This module represents the data stored in artist files, including the artist
    name, number of followers, and popularity. It handles loading of that data
    from JSON as well as querying the data. *)

exception UnknownArtist of string
(** Raised when the Spotify API cannot identify a given artist. It carries the
    identifier of the unknown artist. *)

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
    [j] is a valid image json. *)

val get_artist : unit -> artist
(** [get_artist ()] reads data/artist.json and returns the corresponding artist. *)

val get_artist_name : artist -> string
(** [get_artist_name t] returns the name of artist [t]. *)

val top_track_string : Track.track list -> int -> string
(** [top_track_string tracks i] returns a string list of each track in [tracks].
    For example, the Beatle's top tracks may look something like "1. Here Comes
    The Sun - Remastered 2009 2. Come Together - Remastered 2009 3. Let It Be -
    Remastered 2009 4. Yesterday - Remastered 2009 5. Twist And Shout -
    Remastered 2009". [i] determines the starting index of the list. *)

val print_artist_info : artist -> unit
(** [print_artist_info a] will print information about artist [a] to the
    terminal. For example, if [a] represents the json of the artist Taylor
    Swift, [print_track_info a] will print the following: "Here's what I found:
    Taylor Swift has 74021262 followers and a popularity ranking of 100. Here
    are a few of Taylor Swift's top songs: 1. Anti-Hero 2. Cruel Summer 3. Blank
    Space 4. All Of The Girls You Loved Before 5. Lavender Haze Want to listen
    to Taylor Swift now? Go to
    https://open.spotify.com/artist/06HL4z0CvFAxyc27GXpf02". *)
