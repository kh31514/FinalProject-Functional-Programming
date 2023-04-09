from dotenv import load_dotenv
import os
import base64
from requests import post
import json

load_dotenv()

client_id = "76af978f82c3430bb2fe9661b6147767"
client_secret = "aa3b6d552ada418bbdbc6b4b9105126a"

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
  json_result = json.loads(result.content) # dictionary type
  token = json_result["access_token"]
  return token # string type

def main():
  token = get_token()
  with open('token.txt', 'w') as f:
    f.write(token)