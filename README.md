RoboBrowser-Scala
=================

RoboBrowser-Scala is a Scala port of the great [RoboBrowser](https://github.com/jmcarp/robobrowser) headless browser built in Python.
Just like the original, this incarnation of RoboBrowser clicks links, buttons, and fills and submits forms.


Let's browse Amazon for hotdogs:

```scala
import robobrowser.Browser
import robobrowser.forms.{Form, SingleValueField}

val browser = new Browser(history = Some(10))
browser.open("https://www.amazon.com/")

// get the search form
val formOption = browser.getForm(attrs = Option(Map("action" -> "/s/ref=nb_sb_noss")))
val form = formOption match {
  case Some(f) => f
  case _ => throw new Exception("form does not exist!")
}
println(form) //<RoboForm url=, field-keywords=>

// enter our search query
form("field-keywords") match {
  case s: SingleValueField => s.value = "hotdogs"
}

browser.submitForm(form)

// get the results and navigate to one of the links
val resultLinks = browser.getLinks(attrs = Some(Map("class" -> "a-link-normal a-text-normal")))
browser.followLink(resultLinks(2))

// extract some text from the page
var aboutProduct = browser.select("#feature-bullets").text()
println(aboutProduct) //All Beef Hot Dogs Each package contains 4 hot dogs [...]

// go back
browser.back()

// repeat
browser.followLink(resultLinks(5))
aboutProduct = browser.select("#feature-bullets").text()
println(aboutProduct) //This fits your . Make sure this fits by entering [...]
```

## Dependencies

This project uses the following two libraries:

- [Requests-Scala](https://github.com/lihaoyi/requests-scala)
- [Jsoup](http://jsoup.org/)
 
## TODO

The following functionalities are still under development:
- caching
- connection retries
- multipart form submissions

## Disclaimer

Though I love Scala, I am by no means an expert with this language.
Much of the code may or may not be entirely idiomatic.
Any and all pointers are welcome.