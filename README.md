# heb-haskell

A service which accepts images and tries to detect objects in them.

API endpoints:
- GET http://localhost:8080/images/<ID>
  - gets an image with the given ID
- GET http://localhost:8080/images?objects="elephant,building"
  - get images that are tagged with tags "elephant" or "building"
  - if the query string "objects" was omitted then it returns all images 
- POST http://localhost:8080/images
  file: the image file to upload
  url: the url of an image to upload
  label: an optional field to add to the image
  detect_objects: if present, it will try to detect objects in the image
  - accepts either a url or file with preference for the former. Then, if detect_objects is present, it sends the content up to Google Vision and asks for object detection which returns a list objects detected. 

## Dependencies
- haskell compiler
- Postgres DB
- API key for GCP

## How to Run

- build it
```
stack build
```

- set the env (this example is for Windows)
```
$env:DB_HOST= 
$env:DB_PORT= 
$env:DB_NAME= 
$env:DB_USER= 
$env:DB_PASSWORD= 
```


- add a config.yaml file to contain the API key
```
googleVision:
    url: "https://vision.googleapis.com/v1/images:annotate"
    feature: "OBJECT_LOCALIZATION"
    apiKey: ""
```


- run the generated executable