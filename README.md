Scrape is a simple web page scraping language, intended to be easy to use even by nonexperienced programmers.

### Build

1. install [stack](https://docs.haskellstack.org/en/stable/README/)

2. build
```sh
$ git clone https://github.com/romand/scrape.git
$ cd scrape
$ stack setup
$ stack build
$ stack test
```

### Example

Consider a web page:
```html
<!DOCTYPE html>
<html>
  <head>
    <title>DVD Rewinder Pro</title>
    <meta property="og:image"
          content="http://www.dvdrewinder.com/images/dvdrewwilly166x105.jpg">
    <span class="notranslate" id="prcIsum" itemprop="price" style="">US $59.99</span>
    <script id="main">
      (function() { p = {productInfo: transmogrify({product_id: 29823453}) } })()
    </script>
  </head>
  <body></body>
</html>
```

Then scraper program:
```
|dvdrewinder\\.com| {
  title = title
  image_url = meta[property="og:image"] @content
  price = #prcIsum =~ after /(US )?/
  product_id = #main !json*.productInfo*.product_id
}
```

would scrape like this:
```sh
$ stack exec scrape http://dvdrewinder.com/dvd_rewinder.html test/data/dvd_rewinder.html test/data/dvd_rewinder.scraper | jq .
{
  "page": "dvd_rewinder",
  "title": "DVD Rewinder Pro",
  "image_url": "http://www.dvdrewinder.com/images/dvdrewwilly166x105.jpg",
  "price": "$59.99",
  "product_id": 29823453
}
```

### Language

The program consists of Perl regular expressions, CSS selectors and «JSON selectors».

A regexp enclosed in `|` matching target url comes first, followed by a sequence of field selector expressions of form `<field_name> = <selector>`, enclosed in curly brackets.

A selector consists of CSS selector, optionally followed by
* `@attribute_name` returns the value of the attribute
 OR
 `!deep-content` returns all the content of target element and its child elements, stripping the markup
 OR if not specified, only content of target element is returned, without children content
* `<before|match|after> <regexp>` matches selected content against the regexp, and returns appropriate part of it (`match` is the default)
* JSON selector

#### JSON selector

Interesting bits of info are often buried in javascript code, which may be tricky and fragile to match with regexps.

JSON selector views javascript code transformed into JSON value, replacing language constructs with JSON objects and arrays, stripping some, for example:

```
var a = [1, function(foo) {
  for (var i = 0; i < 10; i++) {
    foo({bar: "baz"})
  }
}, null];
```

would transform into

```json
[1,[0,10,{"bar":"baz"}],null]
```

The resulting JSON value can be queried with:
* regular indexing and field accessor, like `!json.somefield[2].otherfield`
* wildcard `?`, looks 1 level down, like `!json.somefield?.otherfield`
* wildcard `*`, looks at all nested values, like `!json*.somefield*.otherfield`
