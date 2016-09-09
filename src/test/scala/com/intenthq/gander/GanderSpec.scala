package com.intenthq.gander

import java.io.InputStreamReader
import java.net.URL
import java.nio.charset.Charset
import java.util.zip.GZIPInputStream

import com.google.common.base.Charsets
import com.google.common.io.CharStreams
import com.intenthq.gander.opengraph.OpenGraphData
import org.joda.time.DateTime
import org.specs2.mutable.Specification

class GanderSpec extends Specification {

  def extract(url: String, charset: Charset = Charsets.UTF_8): PageInfo = {
    val resource = getClass.getResourceAsStream("/" + url.stripPrefix("http://").replace('/', '_') + ".gz")
    val rawHTML = CharStreams.toString(new InputStreamReader(new GZIPInputStream(resource), charset))
    Gander.extract(rawHTML).get
  }

  def check(pageInfo: PageInfo, title: String, processedTitle: String, metaDescription: String, metaKeywords: String,
            lang: Option[String], date: Option[String], content: String, url: String, links: Seq[Link]) = {
    pageInfo.title must_== title
    pageInfo.processedTitle must_== processedTitle
    pageInfo.metaDescription must_== metaDescription
    pageInfo.metaKeywords must_== metaKeywords
    pageInfo.lang must_== lang
    pageInfo.publishDate must_== date.map(DateTime.parse(_).toDate)
    pageInfo.cleanedText.get must startWith(content)
    pageInfo.canonicalLink.map(_ must_== url).getOrElse(1 must_== 1)
    pageInfo.links must_== links
  }

  "UTF-8 encoding of unicode non breaking char must be sanitised as a space" >> {
    //Some pages (like the Apple Watch one) contain this char instead of a space
    //For more info check https://en.wikipedia.org/wiki/Non-breaking_space
    val url = "http://www.apple.com/watch/"
    extract(url).cleanedText.get must contain("Apple Watch")
  }

  "intenthq" >> {
    val url = "http://engineering.intenthq.com/2015/03/what-is-good-code-a-scientific-definition/"
    check(extract(url),
      url = url,
      content = "Here at Intent HQ we believe how important it is to write good code. Why? First, because writing good code is much cheaper and more fun than writing bad code. Second, because if you write good code chances are that the product you are building will be much better. Third, and more important, because writing good code is what we are supposed to do: after all, we are getting paid for doing our job well",
      title = "What is good code? A scientific definition. - Intent HQ Engineering blog",
      processedTitle = "What is good code? A scientific definition.",
      metaDescription = "How would you define good code? This article gives a pseudo-scientific answer to that question after asking a sample of 65 developers that same question.",
      metaKeywords = "",
      lang = Some("en-GB"),
      date = Some("2015-03-01"),
      links = List(Link("Uncle Bob", "http://en.wikipedia.org/wiki/Robert_Cecil_Martin"),
        Link("DRY", "http://en.wikipedia.org/wiki/Don%27t_repeat_yourself")))
  }

  "bbc.com" >> {
    val url = "http://www.bbc.com/news/business-33697945"
    check(extract(url),
      url = url,
      content = "Disneyland Paris is facing a pricing probe following accusations that UK and German customers are being frozen out of certain price promotions.",
      title = "Disneyland Paris faces pricing probe - BBC News",
      processedTitle = "Disneyland Paris faces pricing probe",
      metaDescription = "Disneyland Paris is facing a pricing probe following accusations that UK and German customers are being frozen out of promotions available in other European member states.",
      metaKeywords = "",
      lang = Some("en"),
      date = None,
      links = List(Link("Financial Times said", "http://www.ft.com/cms/s/0/27e42c8e-351d-11e5-b05b-b01debd57852.html#axzz3hDFfsPCX"),
        Link("said in a report", "http://www.ft.com/cms/s/0/27e42c8e-351d-11e5-b05b-b01debd57852.html#axzz3hDFfsPCX")))

  }

  "bbc.com/sport" >> {
    val url = "http://www.bbc.com/sport/football/34203622"
    check(extract(url),
      url = url,
      content = "Manchester City's Champions League campaign got off to a disappointing start with a home defeat by last season's runners-up Juventus. City, who have struggled to make a serious impact in the Champions League and have never won their opening home game in the group stage, looked to be on course for victory when Juventus defender Giorgio Chiellini headed into his own net under pressure from Vincent Kompany.",
      title = "Alvaro Morata & Mario Mandzukic score as Juventus shock Man City - BBC Sport",
      processedTitle = "Alvaro Morata & Mario Mandzukic score as Juventus shock Man City",
      metaDescription = "Manchester City concede two goals in the last 20 minutes as Juventus fight back from a goal down to win at Etihad Stadium.",
      metaKeywords = "",
      lang = Some("en"),
      date = None,
      links = List()
    )
  }

  "businessinsider" >> {
    val url = "http://www.businessinsider.com/goldman-on-the-fed-announcement-2011-9"
    check(extract(url),
      url = url,
      content = "From Goldman on the FOMC operation twist announcement: ------------- 1. As we had expected, the Federal Open Market Committee decided to \"do the twist\" and increase the duration of its securities holdings by selling shorter-maturity securities ($400bn of Treasuries with maturity of 3 years or less)",
      title = "GOLDMAN: 4 Key Points On The FOMC Announcement - Business Insider",
      processedTitle = "GOLDMAN: 4 Key Points On The FOMC Announcement",
      metaDescription = "Here it is.",
      metaKeywords = "",
      lang = Some("en"),
      date = Some("2011-09-21"),
      links = List(Link("announcement", "http://www.businessinsider.com/federal-reserve-announcement-fomc-operation-twist-2011-9")))
  }

  "elpais" >> {
    val url = "http://internacional.elpais.com/internacional/2015/07/28/actualidad/1438076596_960360.html"
    check(extract(url),
      url = url,
      content = "Los aliados de la OTAN ofrecieron este martes respaldo político a Turquía en su ofensiva contra el Estado Islámico tras una reunión convocada de urgencia por el Gobierno de Ankara.",
      title = "La OTAN apoya con cautela la ofensiva turca contra el yihadismo | Internacional | EL PAÍS",
      processedTitle = "La OTAN apoya con cautela la ofensiva turca contra el yihadismo",
      metaDescription = "La Alianza se ha reunido este martes con carácter de urgencia a pedición de Ankara para tratar el avance del Estado Islámico",
      metaKeywords = "otan, apoyar, cautela, ofensiva, turca, turco, yihadismo, alianza, haber, reunir, martes, urgencia, pedición, ankara, secretario, general, jens stoltenberg, resaltar, unidad, aliado",
      lang = Some("es"),
      date = Some("2015-07-29"),
      links = List(Link("en su ofensiva contra el Estado Islámico", "http://internacional.elpais.com/internacional/2015/07/24/actualidad/1437717227_199769.html"),
        Link("Jens Stoltenberg.", "http://elpais.com/tag/jens_stoltenberg/a/"),
        Link("que este martes hizo estallar un tramo de un gasoducto procedente de Irán", "http://internacional.elpais.com/internacional/2015/07/28/actualidad/1438079899_805996.html"),
        Link("onflicto entre Ankara y los simpatizantes del PKK", "http://internacional.elpais.com/internacional/2015/07/27/actualidad/1437986632_361510.html"),
        Link("crear una zona libre de combatientes del EI", "http://internacional.elpais.com/internacional/2015/07/27/actualidad/1438026945_461718.html"),
        Link("Ahmet Davutoglu", "http://elpais.com/tag/ahmet_davutoglu/a/")))
  }

  "corriere" >> {
    val url = "http://www.corriere.it/cronache/15_luglio_29/relazione-alfano-mafia-fatti-gravi-sindaco-ha-sottovalutato-25146a6c-35b0-11e5-b050-7dc71ce7db4c.shtml"
    check(extract(url, Charsets.ISO_8859_1),
      url = url,
      content = "ROMA La strada è tracciata, la relazione potrebbe arrivare a Palazzo Chigi prima della pausa estiva. Il ministro dell’Interno Angelino Alfano non proporrà lo scioglimento per mafia del comune di Roma, ma nella relazione al governo",
      title = "La relazione di Alfano sulla mafia: fatti gravi, il sindaco ha sottovalutato - Corriere.it",
      processedTitle = "La relazione di Alfano sulla mafia: fatti gravi, il sindaco ha sottovalutato",
      metaDescription = "Non si propone lo scioglimento ma si lascia aperta la possibilità di una «diversa valutazione»",
      metaKeywords = "Ignazio Marino, Angelino Alfano",
      lang = Some("it"),
      date = None,
      links = List(Link("giunta guidata da Ignazio Marino", "http://roma.corriere.it/notizie/politica/15_luglio_28/giunta-marino-senatore-no-tav-esposito-assessore-trasporti-d0e76efa-34fe-11e5-984f-1e10ffe171ae.shtml")))

  }

  /*"canarias7" >> {
    val url = "http://www.canarias7.es/articulo.cfm?Id=434625"
    check(extract(url, Charsets.UTF_8),
      url = url,
      content = "La Junta de Portavoces del Congreso, con el apoyo de la mayoría de los grupos salvo el PP, ha acordado que el ministro de Economía, Luis de Guindos, comparezca para explicar el nombramiento y renuncia de José Manuel Soria como director ejecutivo del Banco Mundial en un pleno extraordinario, para el que aún no hay fecha.\n\nLa fecha de la comparecencia de De Guindos deberá fijarla ahora la Mesa del Congreso, que está semana aprobó el calendario de plenos ordinarios de los próximos meses sin contemplar ninguna sesión plenaria extraordinaria.\n\nNo obstante, PSOE y Unidos Podemos han logrado sacar adelante en la Junta de Portavoces su petición para que De Guindos comparezca en un pleno urgente y no en la Comisión de Economía, como había solicitado el ministro. Solicitud que ha contado con el apoyo de Ciudadanos y otros partidos como el PNV, ERC y Partido Demócrata Catalán, que representa al grupo Mixto, según fuentes parlamentarias.",
      title = "Canarias7. Nacional. El Congreso acuerda que De Guindos explique en un pleno el caso Soria",
      processedTitle = "El Congreso acuerda que De Guindos explique en un pleno el caso Soria",
      metaDescription = "La Junta de Portavoces del Congreso, con el apoyo de la mayoría de los grupos salvo el PP, ha acordado que el ministro de Economía, Luis de Guindos, comparezca para explicar el nombramiento y renuncia de José Manuel Soria como director ejecutivo del Banco Mundial en un pleno extraordinario, para el ...",
      metaKeywords = "Canarias7, prensa, noticias, información, informacion general, Islas Canarias, Canarias, Canary Islands,",
      lang = Some("es"),
      date = None,
      links = List())

  }*/





  "lemonde" >> {
    //    val url = "http://www.lemonde.fr/football/article/2015/07/23/pep-guardiola-un-as-dans-la-manche-des-independantistes_4695701_1616938.html"
    //    check(extract(url),
    //      url = url,
    //      content = "Dans la planète Barça, Pep Guardiola est un demi-dieu. Entraîneur du FC Barcelone entre 2008 et 2012, il a fait remporter aux Blaugrana 14 titres officiels. Dont six en une seule année : 2009",
    //      title = "En Catalogne, Pep Guardiola, figure du Barça, se présente sur la liste indépendantiste",
    //      processedTitle = "En Catalogne, Pep Guardiola, figure du Barça, se présente sur la liste indépendantiste",
    //      metaDescription = "L’ancien entraîneur du FC Barcelone devrait clore la liste unitaire visant à exiger l’indépendance de la Catalogne lors des élections du 27 septembre.",
    //      metaKeywords = "",
    //      lang = Some("fr"),
    //      date = Some("2015-07-23T15:57:46"),
    //      links = List.empty)
    pending
  }

  "globoesporte" >> {
    val url = "http://globoesporte.globo.com/futebol/times/sao-paulo/noticia/2012/04/filho-do-gramado-leao-administra-o-sao-paulo-na-base-da-conversa.html"
    check(extract(url),
      url = url,
      content = "Emerson Leão não foi ao campo na manhã desta terça-feira no centro de treinamento do São Paulo",
      title = "'Filho do gramado', Leão administra o São Paulo na base da conversa | globoesporte.com",
      processedTitle = "'Filho do gramado', Leão administra o São Paulo na base da conversa",
      metaDescription = "Emerson Le&atilde;o cobra lideran&ccedil;a ao S&atilde;o Paulo (Foto: M&aacute;rio &Acirc;ngelo / Ag. Estado) Emerson Le&atilde;o n&atilde;o foi ao campo na manh&atilde; desta ter&ccedil;a-feira no centro de treinamento do S&atilde;o Paulo. Bem humorado e com roupa casual, preferiu acompanhar de longe ...",
      metaKeywords = "notícias, notícia, são paulo",
      lang = None,
      date = Some("2012-04-03T13:49"),
      links = List())
  }

  "opengraph" >> {
    val url = "http://internacional.elpais.com/internacional/2015/07/28/actualidad/1438076596_960360.html"

    extract(url).openGraphData must_==
      OpenGraphData(title = Some("La OTAN apoya con cautela la ofensiva turca contra el yihadismo"),
        siteName = Some("EL PAÍS"),
        url = Some(new URL(url)),
        description = Some("La Alianza se ha reunido este martes con carácter de urgencia a pedición de Ankara para tratar el avance del Estado Islámico"),
        image = Some(new URL("http://ep00.epimg.net/internacional/imagenes/2015/07/28/actualidad/1438076596_960360_1438078067_noticia_normal.jpg")),
        `type` = Some("article"),
        locale = None,
        publishedTime = Some(new DateTime(2015, 7, 29, 0, 0)))

  }

  "fcbarcelona" >> {
    val url = "http://www.fcbarcelona.com/club/detail/article/30-years-since-visit-of-pope-john-paul-ii"

    check(extract(url),
      url = url,
      content = "On November 7, 1982, the Camp Nou enjoyed a historic moment.",
      title = "30 years since visit of Pope John Paul II | FC Barcelona",
      processedTitle = "30 years since visit of Pope John Paul II",
      metaDescription = "This Wednesday is the 30th anniversary of mass given by Pope John Paul at the Camp Nou",
      metaKeywords = "Josep Lluís Núñez, Camp Nou, Club, Season 2012-2013",
      lang = Some("en"),
      date = None,
      links = List()
    )
  }

  "Daily Mail (date is malformed + publish_date misused)" >> {
    val url = "http://www.dailymail.co.uk/news/article-486484/A-spectacular-destruction-How-email-led-downfall-barrister-all.html"

    check(extract(url),
      url = url,
      content = "by PAUL BRACCHI Last updated at 01:01 09 October 2007 An Oxford First, a brilliant radio career and newly qualified as a barrister, Bruce Hyman seemed to have all life's gifts",
      title = "A spectacular destruction: How one email led to the downfall of a barrister who had it all | Daily Mail Online",
      processedTitle = "A spectacular destruction: How one email led to the downfall of a barrister who had it all",
      metaDescription = "An Oxford First, a brilliant radio career and newly qualified as a barrister, Bruce Hyman seemed to have all life's gifts  -  until a moment of utter madness put him behind bars and left his life in ruins",
      metaKeywords = "A,spectacular,destruction,How,email,led,downfall,barrister,all",
      lang = Some("en"),
      date = None,
      links = List()
    )
  }

}
