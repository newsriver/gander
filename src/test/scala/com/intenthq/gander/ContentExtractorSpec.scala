package com.intenthq.gander

import com.intenthq.gander.extractors.ContentExtractor._
import org.jsoup.Jsoup
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ContentExtractorSpec extends Specification {
  "extractTitle" >> {
    def docFromTitle(title: String) = Jsoup.parse(s"<html><head><title>$title</title></head><body></body></html>")
    "should extract a title" >> {
      val title = "the title"
      extractTitle(docFromTitle(title)) must_== title
    }
    "should extract an empty title" >> {
      val title = ""
      extractTitle(docFromTitle(title)) must_== title
    }
  }

  "extractCanonicalLink" >> {
    "should return none if no link found" >> {
      val html =
        """<html lang="ca">
          |  <head>
          |  </head>
          |<body></body></html>""".stripMargin
      extractCanonicalLink(Jsoup.parse(html)) must beNone
    }

    "should extract the canonical link from the meta tag" >> {
      val html =
        """<html lang="ca">
          |  <head>
          |    <link rel="canonical" href="http://example.com/canonical">
          |    <meta property="og:url" content="http://example.com/og" />
          |    <meta name="twitter:url" content="http://example.com/twitter" />
          |  </head>
          |<body></body></html>""".stripMargin
      extractCanonicalLink(Jsoup.parse(html)) must beSome("http://example.com/canonical")
    }
    "should extract the facebook og:url meta tag" >> {
      val html =
        """<html lang="ca">
          |  <head>
          |    <meta property="og:url" content="http://example.com/og" />
          |    <meta name="twitter:url" content="http://example.com/twitter" />
          |  </head>
          |<body></body></html>""".stripMargin
      extractCanonicalLink(Jsoup.parse(html)) must beSome("http://example.com/og")
    }
    "should extract the twitter:url meta tag" >> {
      val html =
        """<html lang="ca">
          |  <head>
          |    <meta name="twitter:url" content="http://example.com/twitter" />
          |  </head>
          |<body></body></html>""".stripMargin
      extractCanonicalLink(Jsoup.parse(html)) must beSome("http://example.com/twitter")
    }
  }

  "extractLang" >> {
    "should extract lang from html tag and give priority to it" >> {
      val html =
        """<html lang="ca">
          |  <head>
          |    <meta http-equiv="Content-Language" content="en">
          |    <meta property="og:locale" content="en_GB" />
          |  </head>
          |<body></body></html>""".stripMargin

      extractLang(Jsoup.parse(html)) must beSome("ca")
    }
    "should extract language from meta tag with more priority than og:locale" >> {
      val html =
        """<html>
          |  <head>
          |    <meta http-equiv="Content-Language" content="ca">
          |    <meta property="og:locale" content="en_GB" />
          |  </head>
          |<body></body></html>""".stripMargin

      extractLang(Jsoup.parse(html)) must beSome("ca")
    }
    "should extract language from og:locale" >> {
      val html =
        """<html>
          |  <head>
          |    <meta property="og:locale" content="ca" />
          |  </head>
          |<body></body></html>""".stripMargin

      extractLang(Jsoup.parse(html)) must beSome("ca")
    }
  }
}
