# source: https://www.youtube.com/watch?v=WAmEZBEeNmg&ab_channel=Linode
from dotenv import load_dotenv
import os
import base64
from requests import post, get
import json

load_dotenv()

client_id = os.getenv("CLIENT_ID")
client_secret = os.getenv("CLIENT_SECRET")


def get_token():
    auth_string = client_id + ":" + client_secret
    auth_bytes = auth_string.encode("utf-8")
    auth_base64 = str(base64.b64encode(auth_bytes), "utf-8")

    url = "https://accounts.spotify.com/api/token"
    headers = {
        "Authorization": "Basic " + auth_base64,
        "Content-Type": "application/x-www-form-urlencoded"
    }
    data = {"grant_type": "client_credentials"}
    result = post(url, headers=headers, data=data)
    json_result = json.loads(result.content)  # dictionary type
    token = json_result["access_token"]
    return token  # string type


def get_auth_header(token):
    return {"Authorization": "Bearer " + token}

def search_for_artist(token, artist_name):
    artist_name = artist_name.replace(" ", "%")
    url = "https://api.spotify.com/v1/search"
    headers = get_auth_header(token)
    query = f"?q={artist_name}&type=artist&limit=1"

    query_url = url + query
    result = get(query_url, headers=headers)
    json_result = json.loads(result.content)["artists"]["items"]
    if len(json_result) == 0:
        return None
    return json_result[0]

def get_artist_id(artist_name):
    with open('data/artist.json') as json_file:
        data = json.load(json_file)
    return data["id"]

def get_top_tracks(token, artist_name):
    artist_name = artist_name.replace(" ", "%")
    id = get_artist_id(artist_name)
    url = f"https://api.spotify.com/v1/artists/{id}/top-tracks?country=US"
    headers = get_auth_header(token)
    result = get(url, headers=headers)
    json_result = json.loads(result.content)["tracks"]
    return json_result[0:5]

def handle_artist():
    with open('data/user_input.txt') as f:
        artist = f.readlines()  # returns a list
    # grabs the firts (and only) string in the list and removes the trailing newline character
    artist = artist[0].strip()
    token = get_token()
    result = search_for_artist(token, artist)
    with open("data/artist.json", "w") as f:
        json.dump(result, f)
    result = get_top_tracks(token, artist)
    with open("data/top_artist_tracks.json", "w") as f:
        json.dump(result, f)
    return

def search_for_album(token, album_name):
    url = "https://api.spotify.com/v1/search"
    headers = get_auth_header(token)
    query = f"?q={album_name}&type=album&limit=1"

    query_url = url + query
    result = get(query_url, headers=headers)
    json_result = json.loads(result.content)["albums"]["items"]
    if len(json_result) == 0:
        return None
    return json_result[0]


def handle_album():
    with open('data/user_input.txt') as f:
        album = f.readlines()  # returns a list
    # grabs the firts (and only) string in the list and removes the trailing newline character
    album = album[0].strip()
    token = get_token()
    result = search_for_album(token, album)
    with open("data/album.json", "w") as f:
        json.dump(result, f)
    return


def search_for_track(token, track_name, artist_name):
    track_name = track_name.replace(" ", "%")
    artist_name = artist_name.replace(" ", "%")
    url = "https://api.spotify.com/v1/search"
    headers = get_auth_header(token)
    if artist_name == "":
        query = f"?q={track_name}&type=track&limit=1"
    else:
        query = f"?q=name:{track_name}%20artist:{artist_name}&type=track&limit=1"

    query_url = url + query
    result = get(query_url, headers=headers)
    json_result = json.loads(result.content)["tracks"]["items"]
    if len(json_result) == 0:
        return None
    return json_result[0]


def handle_track():
    with open('data/user_input.txt') as f:
        track = f.readlines()  # returns a list
    # grabs the firts (and only) string in the list and removes the trailing newline character
    track_name = track[0].strip()
    token = get_token()
    try:
        artist = track[1].strip()
    except:
        result = search_for_track(token, track_name, "")
    else:
        result = search_for_track(token, track_name, artist)

    with open("data/track.json", "w") as f:
        json.dump(result, f)
    return

