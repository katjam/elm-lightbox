## Elm Lightbox

### Compile

`elm make src/LightBox.elm --output=lightbox.js`

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
