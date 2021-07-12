## Elm Lightbox

### Flags

Json encoded array of image data

```
[ 
  { fullSrc = "https:/path/to/full-size/image1"
    , thumbSrc = "https://path/to/thumbnail/image1"
  }
, { fullSrc = "https:/path/to/full-size/image2"
    , thumbSrc = "https://path/to/thumbnail/image2"
  }

  ...

]
```

### Serve to localhost
`elm-live src/LightBox.elm --hot -- --output=lightbox.js`

### Compile

#### Development
`elm make src/LightBox.elm --output=lightbox.js`

#### Production
`elm make src/LightBox.elm --optimize --output=lightbox.js`

### Embed in html

```
<script src="path/to/js/lightbox.js"></script>
```


```
    <div id="lightbox">Lightbox</div>
    <script>
      var app = Elm.LightBox.init({
      node: document.getElementById('lightbox')
    });
    </script>

```
